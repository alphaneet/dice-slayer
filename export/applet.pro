-dontnote
-dontwarn
-injars       AppletIn.jar
-injars       /usr/local/Cellar/scala/2.9.1/libexec/lib/scala-library.jar(!META-INF/MANIFEST.MF,!library.properties)
-injars       /Users/alphaneet/lib/jar/processing-core-1.5.1.jar(!META-INF/MANIFEST.MF)
-libraryjars  <java.home>/../Classes/classes.jar
-outjars      Applet.jar
-keep public class * extends processing.core.PGraphics
-keep public class Applet
-keep public class Application {
  public static void main(java.lang.String[]);
}
