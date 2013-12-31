![Sample Image](sample_640x480_ss4_s4.png)

# Haspt: A Path Tracing Renderer Written in Haskell

Haspt is a physical-based global illumination renderer.

- It uses path tracing algorithm.
- it is written in Haskell
- It is generally based on [edupt](https://github.com/githole/edupt).

## How to Use

You must use -O2 or -O3 option, or you'll lose a lot of time!

    $ ghc -O3 haspt.hs && ./haspt > image.ppm

You can use IrfanView (Windows), ToyViewer (OSX) or something to view the PPM file.

