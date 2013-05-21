#include "Context.h"

#include "Wrapper.h"
#include "Engine.h"


// Extracts a C string from a V8 Utf8Value.
const char* ToCString(const v8::String::Utf8Value& value) {
  return *value ? *value : "<string conversion failed>";
}


// Reads a file into a v8 string.
v8::Handle<v8::String> ReadFile(const char* name) {
  FILE* file = fopen(name, "rb");
  if (file == NULL) return v8::Handle<v8::String>();

  fseek(file, 0, SEEK_END);
  int size = ftell(file);
  rewind(file);

  char* chars = new char[size + 1];
  chars[size] = '\0';
  for (int i = 0; i < size;) {
    int read = static_cast<int>(fread(&chars[i], 1, size - i, file));
    i += read;
  }
  fclose(file);
  v8::Handle<v8::String> result = v8::String::New(chars, size);
  delete[] chars;
  return result;
}


void ReportException(v8::Isolate* isolate, v8::TryCatch* try_catch) {
  v8::HandleScope handle_scope(isolate);
  v8::String::Utf8Value exception(try_catch->Exception());
  const char* exception_string = ToCString(exception);
  v8::Handle<v8::Message> message = try_catch->Message();
  if (message.IsEmpty()) {
    // V8 didn't provide any extra information about this error; just
    // print the exception.
    fprintf(stderr, "%s\n", exception_string);
  } else {
    // Print (filename):(line number): (message).
    v8::String::Utf8Value filename(message->GetScriptResourceName());
    const char* filename_string = ToCString(filename);
    int linenum = message->GetLineNumber();
    fprintf(stderr, "%s:%i: %s\n", filename_string, linenum, exception_string);
    // Print line of source code.
    v8::String::Utf8Value sourceline(message->GetSourceLine());
    const char* sourceline_string = ToCString(sourceline);
    fprintf(stderr, "%s\n", sourceline_string);
    // Print wavy underline (GetUnderline is deprecated).
    int start = message->GetStartColumn();
    for (int i = 0; i < start; i++) {
      fprintf(stderr, " ");
    }
    int end = message->GetEndColumn();
    for (int i = start; i < end; i++) {
      fprintf(stderr, "^");
    }
    fprintf(stderr, "\n");
    v8::String::Utf8Value stack_trace(try_catch->StackTrace());
    if (stack_trace.length() > 0) {
      const char* stack_trace_string = ToCString(stack_trace);
      fprintf(stderr, "%s\n", stack_trace_string);
    }
  }
}


bool ExecuteString(v8::Isolate* isolate,
                   v8::Handle<v8::String> source,
                   v8::Handle<v8::Value> name,
                   bool print_result,
                   bool report_exceptions) {
  v8::HandleScope handle_scope(isolate);
  v8::TryCatch try_catch;
  v8::Handle<v8::Script> script = v8::Script::Compile(source, name);
  if (script.IsEmpty()) {
    // Print errors that happened during compilation.
    if (report_exceptions)
      ReportException(isolate, &try_catch);
    return false;
  } else {
    v8::Handle<v8::Value> result = script->Run();
    if (result.IsEmpty()) {
      assert(try_catch.HasCaught());
      // Print errors that happened during execution.
      if (report_exceptions)
        ReportException(isolate, &try_catch);
      return false;
    } else {
      assert(!try_catch.HasCaught());
      if (print_result && !result->IsUndefined()) {
        // If all went well and the result wasn't undefined then print
        // the returned value.
        v8::String::Utf8Value str(result);
        const char* cstr = ToCString(str);
        printf("%s\n", cstr);
      }
      return true;
    }
  }
}

v8::Handle<v8::Value> Print(const v8::Arguments& args) {
  bool first = true;
  for (int i = 0; i < args.Length(); i++) {
    v8::HandleScope handle_scope(args.GetIsolate());
    if (first) {
      first = false;
    } else {
      printf(" ");
    }
    v8::String::Utf8Value str(args[i]);
    const char* cstr = ToCString(str);
    printf("%s", cstr);
  }
  printf("\n");
  fflush(stdout);
  return v8::Undefined();
}


v8::Handle<v8::Value> Load(const v8::Arguments& args) {
  for (int i = 0; i < args.Length(); i++) {
    v8::HandleScope handle_scope(args.GetIsolate());
    v8::String::Utf8Value file(args[i]);
    if (*file == NULL) {
      return v8::ThrowException(v8::String::New("Error loading file"));
    }
    v8::Handle<v8::String> source = ReadFile(*file);
    if (source.IsEmpty()) {
      return v8::ThrowException(v8::String::New("Error loading file"));
    }
    if (!ExecuteString(args.GetIsolate(),
                       source,
                       v8::String::New(*file),
                       true,
                       true)) {
      return v8::ThrowException(v8::String::New("Error executing file"));
    }
  }
  return v8::Undefined();
}

v8::Handle<v8::Value> AssertEquals(const v8::Arguments& args){
    v8::HandleScope handle_scope(args.GetIsolate());
    if (args.Length()==3){
        v8::String::Utf8Value msg(args[2]);
        if(args[0]->Equals(args[1])) {
            fprintf(stdout, "[+] AssertEquals PASS %s", *msg);
            return v8::True();
        }else{
            fprintf(stderr, "[-] AssertEquals ERROR %s", *msg);
        }
    }else{
        fprintf(stderr, "[-] AssertEquals(op1, op2, string_msg).");
    }
    exit(-1);
    return v8::False();
}

void CContext::Expose(void)
{
  py::class_<CIsolate, boost::noncopyable>("JSIsolate", "JSIsolate is an isolated instance of the V8 engine.", py::no_init)
    .def(py::init<bool>((py::arg("owner") = false)))

    .add_static_property("current", &CIsolate::GetCurrent,
                         "Returns the entered isolate for the current thread or NULL in case there is no current isolate.")

    .add_property("locked", &CIsolate::IsLocked)

    .def("enter", &CIsolate::Enter, 
         "Sets this isolate as the entered one for the current thread. "
         "Saves the previously entered one (if any), so that it can be "
         "restored when exiting.  Re-entering an isolate is allowed.")

    .def("leave", &CIsolate::Leave,
         "Exits this isolate by restoring the previously entered one in the current thread. "
         "The isolate may still stay the same, if it was entered more than once.")
    ;

  py::objects::class_value_wrapper<boost::shared_ptr<CIsolate>, 
    py::objects::make_ptr_instance<CIsolate, 
    py::objects::pointer_holder<boost::shared_ptr<CIsolate>,CIsolate> > >();

  py::class_<CContext, boost::noncopyable>("JSContext", "JSContext is an execution context.", py::no_init)
    .def(py::init<const CContext&>("create a new context base on a exists context"))
    .def(py::init<py::object, py::list>((py::arg("global") = py::object(), 
                                         py::arg("extensions") = py::list()), 
                                        "create a new context base on global object"))
                  
    .add_property("securityToken", &CContext::GetSecurityToken, &CContext::SetSecurityToken)
    
    .add_property("locals", &CContext::GetGlobal, "Local variables within context")
    
    .add_static_property("entered", &CContext::GetEntered, 
                         "The last entered context.")
    .add_static_property("current", &CContext::GetCurrent, 
                         "The context that is on the top of the stack.")
    .add_static_property("calling", &CContext::GetCalling,
                         "The context of the calling JavaScript code.")
    .add_static_property("inContext", &CContext::InContext,
                         "Returns true if V8 has a current context.")

    .def("eval", &CContext::Evaluate, (py::arg("source"), 
                                       py::arg("name") = std::string(),
                                       py::arg("line") = -1,
                                       py::arg("col") = -1,
                                       py::arg("precompiled") = py::object()))
    .def("eval", &CContext::EvaluateW, (py::arg("source"), 
                                        py::arg("name") = std::wstring(),
                                        py::arg("line") = -1,
                                        py::arg("col") = -1,
                                        py::arg("precompiled") = py::object()))

    .def("enter", &CContext::Enter, "Enter this context. "
         "After entering a context, all code compiled and "
         "run is compiled and run in this context.")
    .def("leave", &CContext::Leave, "Exit this context. "
         "Exiting the current context restores the context "
         "that was in place when entering the current context.")

    .def("__nonzero__", &CContext::IsEntered, "the context has been entered.")
    ;

  py::objects::class_value_wrapper<boost::shared_ptr<CContext>, 
    py::objects::make_ptr_instance<CContext, 
    py::objects::pointer_holder<boost::shared_ptr<CContext>,CContext> > >();
}

py::object CIsolate::GetCurrent(void)
{
  v8::HandleScope handle_scope;

  v8::Isolate *isolate = v8::Isolate::GetCurrent();

  return !isolate ? py::object() :
    py::object(py::handle<>(boost::python::converter::shared_ptr_to_python<CIsolate>(
    CIsolatePtr(new CIsolate(isolate)))));  
}

CContext::CContext(v8::Handle<v8::Context> context)
{
  v8::HandleScope handle_scope;

  m_context = v8::Persistent<v8::Context>::New(context);
}

CContext::CContext(const CContext& context)
{
  v8::HandleScope handle_scope;

  m_context = context.m_context;
}

CContext::CContext(py::object global, py::list extensions)
{
  v8::HandleScope handle_scope;  

  std::auto_ptr<v8::ExtensionConfiguration> cfg;
  std::vector<std::string> ext_names;
  std::vector<const char *> ext_ptrs;

  for (Py_ssize_t i=0; i<PyList_Size(extensions.ptr()); i++)
  {
    py::extract<const std::string> extractor(::PyList_GetItem(extensions.ptr(), i));

    if (extractor.check()) 
    {
      ext_names.push_back(extractor());      
    }
  }

  for (size_t i=0; i<ext_names.size(); i++)
  {
    ext_ptrs.push_back(ext_names[i].c_str());
  }

  if (!ext_ptrs.empty()) cfg.reset(new v8::ExtensionConfiguration(ext_ptrs.size(), &ext_ptrs[0]));
  
  v8::Handle<v8::ObjectTemplate> global_functions = v8::ObjectTemplate::New();
  // Bind the global 'print' function to the C++ Print callback.
  global_functions->Set(v8::String::New("Print"), v8::FunctionTemplate::New(Print));
  global_functions->Set(v8::String::New("Load"), v8::FunctionTemplate::New(Load));
  global_functions->Set(v8::String::New("AssertEquals"), v8::FunctionTemplate::New(AssertEquals));
  m_context = v8::Context::New(cfg.get(),global_functions);

  v8::Context::Scope context_scope(m_context);

  if (global.ptr() != Py_None)
  {    
    m_context->Global()->Set(v8::String::NewSymbol("__proto__"), CPythonObject::Wrap(global));  
  }
}

py::object CContext::GetGlobal(void) 
{ 
  v8::HandleScope handle_scope;

  return CJavascriptObject::Wrap(m_context->Global()); 
}

py::str CContext::GetSecurityToken(void)
{
  v8::HandleScope handle_scope;
 
  v8::Handle<v8::Value> token = m_context->GetSecurityToken();

  if (token.IsEmpty()) return py::str();
  
  v8::String::AsciiValue str(token->ToString());

  return py::str(*str, str.length());    
}

void CContext::SetSecurityToken(py::str token)
{
  v8::HandleScope handle_scope;

  if (token.ptr() == Py_None) 
  {
    m_context->UseDefaultSecurityToken();
  }
  else
  {    
    m_context->SetSecurityToken(v8::String::New(py::extract<const char *>(token)()));  
  }
}

py::object CContext::GetEntered(void) 
{ 
  v8::HandleScope handle_scope;

  return !v8::Context::InContext() ? py::object() :
    py::object(py::handle<>(boost::python::converter::shared_ptr_to_python<CContext>(
      CContextPtr(new CContext(v8::Context::GetEntered())))));
}
py::object CContext::GetCurrent(void) 
{ 
  v8::HandleScope handle_scope;

  return !v8::Context::InContext() ? py::object() :
    py::object(py::handle<>(boost::python::converter::shared_ptr_to_python<CContext>(
      CContextPtr(new CContext(v8::Context::GetCurrent()))))); 
}
py::object CContext::GetCalling(void)
{
  v8::HandleScope handle_scope;

  v8::Handle<v8::Context> calling = v8::Context::GetCalling();

  return calling.IsEmpty() ? py::object() :
    py::object(py::handle<>(boost::python::converter::shared_ptr_to_python<CContext>(
      CContextPtr(new CContext(handle_scope.Close(calling))))));
}

py::object CContext::Evaluate(const std::string& src, 
                              const std::string name,
                              int line, int col,
                              py::object precompiled) 
{ 
  CEngine engine;

  CScriptPtr script = engine.Compile(src, name, line, col, precompiled);

  return script->Run(); 
}

py::object CContext::EvaluateW(const std::wstring& src, 
                               const std::wstring name,
                               int line, int col,
                               py::object precompiled) 
{ 
  CEngine engine;

  CScriptPtr script = engine.CompileW(src, name, line, col, precompiled);

  return script->Run(); 
}
