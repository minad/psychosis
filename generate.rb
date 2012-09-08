File.read('macros').split(/\s+/).each do |line|
  proc = line.gsub('->','_to_').gsub('-','_').gsub('?','_p').gsub('!','')
  puts "define_macro(#{line.inspect}, macro_#{proc});"
end
