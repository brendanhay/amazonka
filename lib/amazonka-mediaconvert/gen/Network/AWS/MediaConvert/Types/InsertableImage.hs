{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.InsertableImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.InsertableImage
  ( InsertableImage (..)
  -- * Smart constructor
  , mkInsertableImage
  -- * Lenses
  , iiDuration
  , iiFadeIn
  , iiFadeOut
  , iiHeight
  , iiImageInserterInput
  , iiImageX
  , iiImageY
  , iiLayer
  , iiOpacity
  , iiStartTime
  , iiWidth
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Settings that specify how your still graphic overlay appears.
--
-- /See:/ 'mkInsertableImage' smart constructor.
data InsertableImage = InsertableImage'
  { duration :: Core.Maybe Core.Natural
    -- ^ Specify the time, in milliseconds, for the image to remain on the output video. This duration includes fade-in time but not fade-out time.
  , fadeIn :: Core.Maybe Core.Natural
    -- ^ Specify the length of time, in milliseconds, between the Start time that you specify for the image insertion and the time that the image appears at full opacity. Full opacity is the level that you specify for the opacity setting. If you don't specify a value for Fade-in, the image will appear abruptly at the overlay start time.
  , fadeOut :: Core.Maybe Core.Natural
    -- ^ Specify the length of time, in milliseconds, between the end of the time that you have specified for the image overlay Duration and when the overlaid image has faded to total transparency. If you don't specify a value for Fade-out, the image will disappear abruptly at the end of the inserted image duration.
  , height :: Core.Maybe Core.Natural
    -- ^ Specify the height of the inserted image in pixels. If you specify a value that's larger than the video resolution height, the service will crop your overlaid image to fit. To use the native height of the image, keep this setting blank.
  , imageInserterInput :: Core.Maybe Core.Text
    -- ^ Specify the HTTP, HTTPS, or Amazon S3 location of the image that you want to overlay on the video. Use a PNG or TGA file.
  , imageX :: Core.Maybe Core.Natural
    -- ^ Specify the distance, in pixels, between the inserted image and the left edge of the video frame. Required for any image overlay that you specify.
  , imageY :: Core.Maybe Core.Natural
    -- ^ Specify the distance, in pixels, between the overlaid image and the top edge of the video frame. Required for any image overlay that you specify.
  , layer :: Core.Maybe Core.Natural
    -- ^ Specify how overlapping inserted images appear. Images with higher values for Layer appear on top of images with lower values for Layer.
  , opacity :: Core.Maybe Core.Natural
    -- ^ Use Opacity (Opacity) to specify how much of the underlying video shows through the inserted image. 0 is transparent and 100 is fully opaque. Default is 50.
  , startTime :: Core.Maybe Core.Text
    -- ^ Specify the timecode of the frame that you want the overlay to first appear on. This must be in timecode (HH:MM:SS:FF or HH:MM:SS;FF) format. Remember to take into account your timecode source settings.
  , width :: Core.Maybe Core.Natural
    -- ^ Specify the width of the inserted image in pixels. If you specify a value that's larger than the video resolution width, the service will crop your overlaid image to fit. To use the native width of the image, keep this setting blank.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InsertableImage' value with any optional fields omitted.
mkInsertableImage
    :: InsertableImage
mkInsertableImage
  = InsertableImage'{duration = Core.Nothing, fadeIn = Core.Nothing,
                     fadeOut = Core.Nothing, height = Core.Nothing,
                     imageInserterInput = Core.Nothing, imageX = Core.Nothing,
                     imageY = Core.Nothing, layer = Core.Nothing,
                     opacity = Core.Nothing, startTime = Core.Nothing,
                     width = Core.Nothing}

-- | Specify the time, in milliseconds, for the image to remain on the output video. This duration includes fade-in time but not fade-out time.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiDuration :: Lens.Lens' InsertableImage (Core.Maybe Core.Natural)
iiDuration = Lens.field @"duration"
{-# INLINEABLE iiDuration #-}
{-# DEPRECATED duration "Use generic-lens or generic-optics with 'duration' instead"  #-}

-- | Specify the length of time, in milliseconds, between the Start time that you specify for the image insertion and the time that the image appears at full opacity. Full opacity is the level that you specify for the opacity setting. If you don't specify a value for Fade-in, the image will appear abruptly at the overlay start time.
--
-- /Note:/ Consider using 'fadeIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiFadeIn :: Lens.Lens' InsertableImage (Core.Maybe Core.Natural)
iiFadeIn = Lens.field @"fadeIn"
{-# INLINEABLE iiFadeIn #-}
{-# DEPRECATED fadeIn "Use generic-lens or generic-optics with 'fadeIn' instead"  #-}

-- | Specify the length of time, in milliseconds, between the end of the time that you have specified for the image overlay Duration and when the overlaid image has faded to total transparency. If you don't specify a value for Fade-out, the image will disappear abruptly at the end of the inserted image duration.
--
-- /Note:/ Consider using 'fadeOut' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiFadeOut :: Lens.Lens' InsertableImage (Core.Maybe Core.Natural)
iiFadeOut = Lens.field @"fadeOut"
{-# INLINEABLE iiFadeOut #-}
{-# DEPRECATED fadeOut "Use generic-lens or generic-optics with 'fadeOut' instead"  #-}

-- | Specify the height of the inserted image in pixels. If you specify a value that's larger than the video resolution height, the service will crop your overlaid image to fit. To use the native height of the image, keep this setting blank.
--
-- /Note:/ Consider using 'height' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiHeight :: Lens.Lens' InsertableImage (Core.Maybe Core.Natural)
iiHeight = Lens.field @"height"
{-# INLINEABLE iiHeight #-}
{-# DEPRECATED height "Use generic-lens or generic-optics with 'height' instead"  #-}

-- | Specify the HTTP, HTTPS, or Amazon S3 location of the image that you want to overlay on the video. Use a PNG or TGA file.
--
-- /Note:/ Consider using 'imageInserterInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiImageInserterInput :: Lens.Lens' InsertableImage (Core.Maybe Core.Text)
iiImageInserterInput = Lens.field @"imageInserterInput"
{-# INLINEABLE iiImageInserterInput #-}
{-# DEPRECATED imageInserterInput "Use generic-lens or generic-optics with 'imageInserterInput' instead"  #-}

-- | Specify the distance, in pixels, between the inserted image and the left edge of the video frame. Required for any image overlay that you specify.
--
-- /Note:/ Consider using 'imageX' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiImageX :: Lens.Lens' InsertableImage (Core.Maybe Core.Natural)
iiImageX = Lens.field @"imageX"
{-# INLINEABLE iiImageX #-}
{-# DEPRECATED imageX "Use generic-lens or generic-optics with 'imageX' instead"  #-}

-- | Specify the distance, in pixels, between the overlaid image and the top edge of the video frame. Required for any image overlay that you specify.
--
-- /Note:/ Consider using 'imageY' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiImageY :: Lens.Lens' InsertableImage (Core.Maybe Core.Natural)
iiImageY = Lens.field @"imageY"
{-# INLINEABLE iiImageY #-}
{-# DEPRECATED imageY "Use generic-lens or generic-optics with 'imageY' instead"  #-}

-- | Specify how overlapping inserted images appear. Images with higher values for Layer appear on top of images with lower values for Layer.
--
-- /Note:/ Consider using 'layer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiLayer :: Lens.Lens' InsertableImage (Core.Maybe Core.Natural)
iiLayer = Lens.field @"layer"
{-# INLINEABLE iiLayer #-}
{-# DEPRECATED layer "Use generic-lens or generic-optics with 'layer' instead"  #-}

-- | Use Opacity (Opacity) to specify how much of the underlying video shows through the inserted image. 0 is transparent and 100 is fully opaque. Default is 50.
--
-- /Note:/ Consider using 'opacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiOpacity :: Lens.Lens' InsertableImage (Core.Maybe Core.Natural)
iiOpacity = Lens.field @"opacity"
{-# INLINEABLE iiOpacity #-}
{-# DEPRECATED opacity "Use generic-lens or generic-optics with 'opacity' instead"  #-}

-- | Specify the timecode of the frame that you want the overlay to first appear on. This must be in timecode (HH:MM:SS:FF or HH:MM:SS;FF) format. Remember to take into account your timecode source settings.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiStartTime :: Lens.Lens' InsertableImage (Core.Maybe Core.Text)
iiStartTime = Lens.field @"startTime"
{-# INLINEABLE iiStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | Specify the width of the inserted image in pixels. If you specify a value that's larger than the video resolution width, the service will crop your overlaid image to fit. To use the native width of the image, keep this setting blank.
--
-- /Note:/ Consider using 'width' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiWidth :: Lens.Lens' InsertableImage (Core.Maybe Core.Natural)
iiWidth = Lens.field @"width"
{-# INLINEABLE iiWidth #-}
{-# DEPRECATED width "Use generic-lens or generic-optics with 'width' instead"  #-}

instance Core.FromJSON InsertableImage where
        toJSON InsertableImage{..}
          = Core.object
              (Core.catMaybes
                 [("duration" Core..=) Core.<$> duration,
                  ("fadeIn" Core..=) Core.<$> fadeIn,
                  ("fadeOut" Core..=) Core.<$> fadeOut,
                  ("height" Core..=) Core.<$> height,
                  ("imageInserterInput" Core..=) Core.<$> imageInserterInput,
                  ("imageX" Core..=) Core.<$> imageX,
                  ("imageY" Core..=) Core.<$> imageY,
                  ("layer" Core..=) Core.<$> layer,
                  ("opacity" Core..=) Core.<$> opacity,
                  ("startTime" Core..=) Core.<$> startTime,
                  ("width" Core..=) Core.<$> width])

instance Core.FromJSON InsertableImage where
        parseJSON
          = Core.withObject "InsertableImage" Core.$
              \ x ->
                InsertableImage' Core.<$>
                  (x Core..:? "duration") Core.<*> x Core..:? "fadeIn" Core.<*>
                    x Core..:? "fadeOut"
                    Core.<*> x Core..:? "height"
                    Core.<*> x Core..:? "imageInserterInput"
                    Core.<*> x Core..:? "imageX"
                    Core.<*> x Core..:? "imageY"
                    Core.<*> x Core..:? "layer"
                    Core.<*> x Core..:? "opacity"
                    Core.<*> x Core..:? "startTime"
                    Core.<*> x Core..:? "width"
