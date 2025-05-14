object PigLatin:
   private val vowels = Seq('a', 'e', 'i', 'o', 'u')
   private val consonants = ('a' to 'z').diff(vowels)
   def translate(s: String): String =
      s match
         case s1 if vowels.contains(s1.head) || Seq("xr", "yt").contains(s1.take(2)) => s + "ay"
         case s22 if s22.take(2).forall(consonants.contains(_))                      => s22.drop(2) + s22.take(2) + "ay"
         case s2 if consonants.contains(s2.head)                                     => s2.drop(1) + s2.head + "ay"
         case s3 if s3.contains("qu")                                                => "ay"
         case s4 if s4.contains("y")                                                 => "ay"
