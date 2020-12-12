{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.InsertableImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.InsertableImage
  ( InsertableImage (..),

    -- * Smart constructor
    mkInsertableImage,

    -- * Lenses
    iiImageX,
    iiHeight,
    iiStartTime,
    iiFadeOut,
    iiWidth,
    iiOpacity,
    iiLayer,
    iiDuration,
    iiImageY,
    iiImageInserterInput,
    iiFadeIn,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Settings that specify how your still graphic overlay appears.
--
-- /See:/ 'mkInsertableImage' smart constructor.
data InsertableImage = InsertableImage'
  { imageX ::
      Lude.Maybe Lude.Natural,
    height :: Lude.Maybe Lude.Natural,
    startTime :: Lude.Maybe Lude.Text,
    fadeOut :: Lude.Maybe Lude.Natural,
    width :: Lude.Maybe Lude.Natural,
    opacity :: Lude.Maybe Lude.Natural,
    layer :: Lude.Maybe Lude.Natural,
    duration :: Lude.Maybe Lude.Natural,
    imageY :: Lude.Maybe Lude.Natural,
    imageInserterInput :: Lude.Maybe Lude.Text,
    fadeIn :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InsertableImage' with the minimum fields required to make a request.
--
-- * 'duration' - Specify the time, in milliseconds, for the image to remain on the output video. This duration includes fade-in time but not fade-out time.
-- * 'fadeIn' - Specify the length of time, in milliseconds, between the Start time that you specify for the image insertion and the time that the image appears at full opacity. Full opacity is the level that you specify for the opacity setting. If you don't specify a value for Fade-in, the image will appear abruptly at the overlay start time.
-- * 'fadeOut' - Specify the length of time, in milliseconds, between the end of the time that you have specified for the image overlay Duration and when the overlaid image has faded to total transparency. If you don't specify a value for Fade-out, the image will disappear abruptly at the end of the inserted image duration.
-- * 'height' - Specify the height of the inserted image in pixels. If you specify a value that's larger than the video resolution height, the service will crop your overlaid image to fit. To use the native height of the image, keep this setting blank.
-- * 'imageInserterInput' - Specify the HTTP, HTTPS, or Amazon S3 location of the image that you want to overlay on the video. Use a PNG or TGA file.
-- * 'imageX' - Specify the distance, in pixels, between the inserted image and the left edge of the video frame. Required for any image overlay that you specify.
-- * 'imageY' - Specify the distance, in pixels, between the overlaid image and the top edge of the video frame. Required for any image overlay that you specify.
-- * 'layer' - Specify how overlapping inserted images appear. Images with higher values for Layer appear on top of images with lower values for Layer.
-- * 'opacity' - Use Opacity (Opacity) to specify how much of the underlying video shows through the inserted image. 0 is transparent and 100 is fully opaque. Default is 50.
-- * 'startTime' - Specify the timecode of the frame that you want the overlay to first appear on. This must be in timecode (HH:MM:SS:FF or HH:MM:SS;FF) format. Remember to take into account your timecode source settings.
-- * 'width' - Specify the width of the inserted image in pixels. If you specify a value that's larger than the video resolution width, the service will crop your overlaid image to fit. To use the native width of the image, keep this setting blank.
mkInsertableImage ::
  InsertableImage
mkInsertableImage =
  InsertableImage'
    { imageX = Lude.Nothing,
      height = Lude.Nothing,
      startTime = Lude.Nothing,
      fadeOut = Lude.Nothing,
      width = Lude.Nothing,
      opacity = Lude.Nothing,
      layer = Lude.Nothing,
      duration = Lude.Nothing,
      imageY = Lude.Nothing,
      imageInserterInput = Lude.Nothing,
      fadeIn = Lude.Nothing
    }

-- | Specify the distance, in pixels, between the inserted image and the left edge of the video frame. Required for any image overlay that you specify.
--
-- /Note:/ Consider using 'imageX' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiImageX :: Lens.Lens' InsertableImage (Lude.Maybe Lude.Natural)
iiImageX = Lens.lens (imageX :: InsertableImage -> Lude.Maybe Lude.Natural) (\s a -> s {imageX = a} :: InsertableImage)
{-# DEPRECATED iiImageX "Use generic-lens or generic-optics with 'imageX' instead." #-}

-- | Specify the height of the inserted image in pixels. If you specify a value that's larger than the video resolution height, the service will crop your overlaid image to fit. To use the native height of the image, keep this setting blank.
--
-- /Note:/ Consider using 'height' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiHeight :: Lens.Lens' InsertableImage (Lude.Maybe Lude.Natural)
iiHeight = Lens.lens (height :: InsertableImage -> Lude.Maybe Lude.Natural) (\s a -> s {height = a} :: InsertableImage)
{-# DEPRECATED iiHeight "Use generic-lens or generic-optics with 'height' instead." #-}

-- | Specify the timecode of the frame that you want the overlay to first appear on. This must be in timecode (HH:MM:SS:FF or HH:MM:SS;FF) format. Remember to take into account your timecode source settings.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiStartTime :: Lens.Lens' InsertableImage (Lude.Maybe Lude.Text)
iiStartTime = Lens.lens (startTime :: InsertableImage -> Lude.Maybe Lude.Text) (\s a -> s {startTime = a} :: InsertableImage)
{-# DEPRECATED iiStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | Specify the length of time, in milliseconds, between the end of the time that you have specified for the image overlay Duration and when the overlaid image has faded to total transparency. If you don't specify a value for Fade-out, the image will disappear abruptly at the end of the inserted image duration.
--
-- /Note:/ Consider using 'fadeOut' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiFadeOut :: Lens.Lens' InsertableImage (Lude.Maybe Lude.Natural)
iiFadeOut = Lens.lens (fadeOut :: InsertableImage -> Lude.Maybe Lude.Natural) (\s a -> s {fadeOut = a} :: InsertableImage)
{-# DEPRECATED iiFadeOut "Use generic-lens or generic-optics with 'fadeOut' instead." #-}

-- | Specify the width of the inserted image in pixels. If you specify a value that's larger than the video resolution width, the service will crop your overlaid image to fit. To use the native width of the image, keep this setting blank.
--
-- /Note:/ Consider using 'width' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiWidth :: Lens.Lens' InsertableImage (Lude.Maybe Lude.Natural)
iiWidth = Lens.lens (width :: InsertableImage -> Lude.Maybe Lude.Natural) (\s a -> s {width = a} :: InsertableImage)
{-# DEPRECATED iiWidth "Use generic-lens or generic-optics with 'width' instead." #-}

-- | Use Opacity (Opacity) to specify how much of the underlying video shows through the inserted image. 0 is transparent and 100 is fully opaque. Default is 50.
--
-- /Note:/ Consider using 'opacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiOpacity :: Lens.Lens' InsertableImage (Lude.Maybe Lude.Natural)
iiOpacity = Lens.lens (opacity :: InsertableImage -> Lude.Maybe Lude.Natural) (\s a -> s {opacity = a} :: InsertableImage)
{-# DEPRECATED iiOpacity "Use generic-lens or generic-optics with 'opacity' instead." #-}

-- | Specify how overlapping inserted images appear. Images with higher values for Layer appear on top of images with lower values for Layer.
--
-- /Note:/ Consider using 'layer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiLayer :: Lens.Lens' InsertableImage (Lude.Maybe Lude.Natural)
iiLayer = Lens.lens (layer :: InsertableImage -> Lude.Maybe Lude.Natural) (\s a -> s {layer = a} :: InsertableImage)
{-# DEPRECATED iiLayer "Use generic-lens or generic-optics with 'layer' instead." #-}

-- | Specify the time, in milliseconds, for the image to remain on the output video. This duration includes fade-in time but not fade-out time.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiDuration :: Lens.Lens' InsertableImage (Lude.Maybe Lude.Natural)
iiDuration = Lens.lens (duration :: InsertableImage -> Lude.Maybe Lude.Natural) (\s a -> s {duration = a} :: InsertableImage)
{-# DEPRECATED iiDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | Specify the distance, in pixels, between the overlaid image and the top edge of the video frame. Required for any image overlay that you specify.
--
-- /Note:/ Consider using 'imageY' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiImageY :: Lens.Lens' InsertableImage (Lude.Maybe Lude.Natural)
iiImageY = Lens.lens (imageY :: InsertableImage -> Lude.Maybe Lude.Natural) (\s a -> s {imageY = a} :: InsertableImage)
{-# DEPRECATED iiImageY "Use generic-lens or generic-optics with 'imageY' instead." #-}

-- | Specify the HTTP, HTTPS, or Amazon S3 location of the image that you want to overlay on the video. Use a PNG or TGA file.
--
-- /Note:/ Consider using 'imageInserterInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiImageInserterInput :: Lens.Lens' InsertableImage (Lude.Maybe Lude.Text)
iiImageInserterInput = Lens.lens (imageInserterInput :: InsertableImage -> Lude.Maybe Lude.Text) (\s a -> s {imageInserterInput = a} :: InsertableImage)
{-# DEPRECATED iiImageInserterInput "Use generic-lens or generic-optics with 'imageInserterInput' instead." #-}

-- | Specify the length of time, in milliseconds, between the Start time that you specify for the image insertion and the time that the image appears at full opacity. Full opacity is the level that you specify for the opacity setting. If you don't specify a value for Fade-in, the image will appear abruptly at the overlay start time.
--
-- /Note:/ Consider using 'fadeIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiFadeIn :: Lens.Lens' InsertableImage (Lude.Maybe Lude.Natural)
iiFadeIn = Lens.lens (fadeIn :: InsertableImage -> Lude.Maybe Lude.Natural) (\s a -> s {fadeIn = a} :: InsertableImage)
{-# DEPRECATED iiFadeIn "Use generic-lens or generic-optics with 'fadeIn' instead." #-}

instance Lude.FromJSON InsertableImage where
  parseJSON =
    Lude.withObject
      "InsertableImage"
      ( \x ->
          InsertableImage'
            Lude.<$> (x Lude..:? "imageX")
            Lude.<*> (x Lude..:? "height")
            Lude.<*> (x Lude..:? "startTime")
            Lude.<*> (x Lude..:? "fadeOut")
            Lude.<*> (x Lude..:? "width")
            Lude.<*> (x Lude..:? "opacity")
            Lude.<*> (x Lude..:? "layer")
            Lude.<*> (x Lude..:? "duration")
            Lude.<*> (x Lude..:? "imageY")
            Lude.<*> (x Lude..:? "imageInserterInput")
            Lude.<*> (x Lude..:? "fadeIn")
      )

instance Lude.ToJSON InsertableImage where
  toJSON InsertableImage' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("imageX" Lude..=) Lude.<$> imageX,
            ("height" Lude..=) Lude.<$> height,
            ("startTime" Lude..=) Lude.<$> startTime,
            ("fadeOut" Lude..=) Lude.<$> fadeOut,
            ("width" Lude..=) Lude.<$> width,
            ("opacity" Lude..=) Lude.<$> opacity,
            ("layer" Lude..=) Lude.<$> layer,
            ("duration" Lude..=) Lude.<$> duration,
            ("imageY" Lude..=) Lude.<$> imageY,
            ("imageInserterInput" Lude..=) Lude.<$> imageInserterInput,
            ("fadeIn" Lude..=) Lude.<$> fadeIn
          ]
      )
