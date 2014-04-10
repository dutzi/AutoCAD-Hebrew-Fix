text_edit : dialog {
   key = "box";
   value = "Text";
   fixed_width = true;
   width = 60;
   initial_focus    = "text";
   : edit_box {
      key           = "text";
      allow_accept  = true;
   }
   ok_cancel;
}
