{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.StaticImageActivateScheduleActionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.StaticImageActivateScheduleActionSettings
  ( StaticImageActivateScheduleActionSettings (..)
  -- * Smart constructor
  , mkStaticImageActivateScheduleActionSettings
  -- * Lenses
  , siasasImage
  , siasasDuration
  , siasasFadeIn
  , siasasFadeOut
  , siasasHeight
  , siasasImageX
  , siasasImageY
  , siasasLayer
  , siasasOpacity
  , siasasWidth
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.InputLocation as Types
import qualified Network.AWS.Prelude as Core

-- | Settings for the action to activate a static image.
--
-- /See:/ 'mkStaticImageActivateScheduleActionSettings' smart constructor.
data StaticImageActivateScheduleActionSettings = StaticImageActivateScheduleActionSettings'
  { image :: Types.InputLocation
    -- ^ The location and filename of the image file to overlay on the video. The file must be a 32-bit BMP, PNG, or TGA file, and must not be larger (in pixels) than the input video.
  , duration :: Core.Maybe Core.Natural
    -- ^ The duration in milliseconds for the image to remain on the video. If omitted or set to 0 the duration is unlimited and the image will remain until it is explicitly deactivated.
  , fadeIn :: Core.Maybe Core.Natural
    -- ^ The time in milliseconds for the image to fade in. The fade-in starts at the start time of the overlay. Default is 0 (no fade-in).
  , fadeOut :: Core.Maybe Core.Natural
    -- ^ Applies only if a duration is specified. The time in milliseconds for the image to fade out. The fade-out starts when the duration time is hit, so it effectively extends the duration. Default is 0 (no fade-out).
  , height :: Core.Maybe Core.Natural
    -- ^ The height of the image when inserted into the video, in pixels. The overlay will be scaled up or down to the specified height. Leave blank to use the native height of the overlay.
  , imageX :: Core.Maybe Core.Natural
    -- ^ Placement of the left edge of the overlay relative to the left edge of the video frame, in pixels. 0 (the default) is the left edge of the frame. If the placement causes the overlay to extend beyond the right edge of the underlying video, then the overlay is cropped on the right.
  , imageY :: Core.Maybe Core.Natural
    -- ^ Placement of the top edge of the overlay relative to the top edge of the video frame, in pixels. 0 (the default) is the top edge of the frame. If the placement causes the overlay to extend beyond the bottom edge of the underlying video, then the overlay is cropped on the bottom.
  , layer :: Core.Maybe Core.Natural
    -- ^ The number of the layer, 0 to 7. There are 8 layers that can be overlaid on the video, each layer with a different image. The layers are in Z order, which means that overlays with higher values of layer are inserted on top of overlays with lower values of layer. Default is 0.
  , opacity :: Core.Maybe Core.Natural
    -- ^ Opacity of image where 0 is transparent and 100 is fully opaque. Default is 100.
  , width :: Core.Maybe Core.Natural
    -- ^ The width of the image when inserted into the video, in pixels. The overlay will be scaled up or down to the specified width. Leave blank to use the native width of the overlay.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StaticImageActivateScheduleActionSettings' value with any optional fields omitted.
mkStaticImageActivateScheduleActionSettings
    :: Types.InputLocation -- ^ 'image'
    -> StaticImageActivateScheduleActionSettings
mkStaticImageActivateScheduleActionSettings image
  = StaticImageActivateScheduleActionSettings'{image,
                                               duration = Core.Nothing, fadeIn = Core.Nothing,
                                               fadeOut = Core.Nothing, height = Core.Nothing,
                                               imageX = Core.Nothing, imageY = Core.Nothing,
                                               layer = Core.Nothing, opacity = Core.Nothing,
                                               width = Core.Nothing}

-- | The location and filename of the image file to overlay on the video. The file must be a 32-bit BMP, PNG, or TGA file, and must not be larger (in pixels) than the input video.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siasasImage :: Lens.Lens' StaticImageActivateScheduleActionSettings Types.InputLocation
siasasImage = Lens.field @"image"
{-# INLINEABLE siasasImage #-}
{-# DEPRECATED image "Use generic-lens or generic-optics with 'image' instead"  #-}

-- | The duration in milliseconds for the image to remain on the video. If omitted or set to 0 the duration is unlimited and the image will remain until it is explicitly deactivated.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siasasDuration :: Lens.Lens' StaticImageActivateScheduleActionSettings (Core.Maybe Core.Natural)
siasasDuration = Lens.field @"duration"
{-# INLINEABLE siasasDuration #-}
{-# DEPRECATED duration "Use generic-lens or generic-optics with 'duration' instead"  #-}

-- | The time in milliseconds for the image to fade in. The fade-in starts at the start time of the overlay. Default is 0 (no fade-in).
--
-- /Note:/ Consider using 'fadeIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siasasFadeIn :: Lens.Lens' StaticImageActivateScheduleActionSettings (Core.Maybe Core.Natural)
siasasFadeIn = Lens.field @"fadeIn"
{-# INLINEABLE siasasFadeIn #-}
{-# DEPRECATED fadeIn "Use generic-lens or generic-optics with 'fadeIn' instead"  #-}

-- | Applies only if a duration is specified. The time in milliseconds for the image to fade out. The fade-out starts when the duration time is hit, so it effectively extends the duration. Default is 0 (no fade-out).
--
-- /Note:/ Consider using 'fadeOut' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siasasFadeOut :: Lens.Lens' StaticImageActivateScheduleActionSettings (Core.Maybe Core.Natural)
siasasFadeOut = Lens.field @"fadeOut"
{-# INLINEABLE siasasFadeOut #-}
{-# DEPRECATED fadeOut "Use generic-lens or generic-optics with 'fadeOut' instead"  #-}

-- | The height of the image when inserted into the video, in pixels. The overlay will be scaled up or down to the specified height. Leave blank to use the native height of the overlay.
--
-- /Note:/ Consider using 'height' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siasasHeight :: Lens.Lens' StaticImageActivateScheduleActionSettings (Core.Maybe Core.Natural)
siasasHeight = Lens.field @"height"
{-# INLINEABLE siasasHeight #-}
{-# DEPRECATED height "Use generic-lens or generic-optics with 'height' instead"  #-}

-- | Placement of the left edge of the overlay relative to the left edge of the video frame, in pixels. 0 (the default) is the left edge of the frame. If the placement causes the overlay to extend beyond the right edge of the underlying video, then the overlay is cropped on the right.
--
-- /Note:/ Consider using 'imageX' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siasasImageX :: Lens.Lens' StaticImageActivateScheduleActionSettings (Core.Maybe Core.Natural)
siasasImageX = Lens.field @"imageX"
{-# INLINEABLE siasasImageX #-}
{-# DEPRECATED imageX "Use generic-lens or generic-optics with 'imageX' instead"  #-}

-- | Placement of the top edge of the overlay relative to the top edge of the video frame, in pixels. 0 (the default) is the top edge of the frame. If the placement causes the overlay to extend beyond the bottom edge of the underlying video, then the overlay is cropped on the bottom.
--
-- /Note:/ Consider using 'imageY' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siasasImageY :: Lens.Lens' StaticImageActivateScheduleActionSettings (Core.Maybe Core.Natural)
siasasImageY = Lens.field @"imageY"
{-# INLINEABLE siasasImageY #-}
{-# DEPRECATED imageY "Use generic-lens or generic-optics with 'imageY' instead"  #-}

-- | The number of the layer, 0 to 7. There are 8 layers that can be overlaid on the video, each layer with a different image. The layers are in Z order, which means that overlays with higher values of layer are inserted on top of overlays with lower values of layer. Default is 0.
--
-- /Note:/ Consider using 'layer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siasasLayer :: Lens.Lens' StaticImageActivateScheduleActionSettings (Core.Maybe Core.Natural)
siasasLayer = Lens.field @"layer"
{-# INLINEABLE siasasLayer #-}
{-# DEPRECATED layer "Use generic-lens or generic-optics with 'layer' instead"  #-}

-- | Opacity of image where 0 is transparent and 100 is fully opaque. Default is 100.
--
-- /Note:/ Consider using 'opacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siasasOpacity :: Lens.Lens' StaticImageActivateScheduleActionSettings (Core.Maybe Core.Natural)
siasasOpacity = Lens.field @"opacity"
{-# INLINEABLE siasasOpacity #-}
{-# DEPRECATED opacity "Use generic-lens or generic-optics with 'opacity' instead"  #-}

-- | The width of the image when inserted into the video, in pixels. The overlay will be scaled up or down to the specified width. Leave blank to use the native width of the overlay.
--
-- /Note:/ Consider using 'width' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siasasWidth :: Lens.Lens' StaticImageActivateScheduleActionSettings (Core.Maybe Core.Natural)
siasasWidth = Lens.field @"width"
{-# INLINEABLE siasasWidth #-}
{-# DEPRECATED width "Use generic-lens or generic-optics with 'width' instead"  #-}

instance Core.FromJSON StaticImageActivateScheduleActionSettings
         where
        toJSON StaticImageActivateScheduleActionSettings{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("image" Core..= image),
                  ("duration" Core..=) Core.<$> duration,
                  ("fadeIn" Core..=) Core.<$> fadeIn,
                  ("fadeOut" Core..=) Core.<$> fadeOut,
                  ("height" Core..=) Core.<$> height,
                  ("imageX" Core..=) Core.<$> imageX,
                  ("imageY" Core..=) Core.<$> imageY,
                  ("layer" Core..=) Core.<$> layer,
                  ("opacity" Core..=) Core.<$> opacity,
                  ("width" Core..=) Core.<$> width])

instance Core.FromJSON StaticImageActivateScheduleActionSettings
         where
        parseJSON
          = Core.withObject "StaticImageActivateScheduleActionSettings"
              Core.$
              \ x ->
                StaticImageActivateScheduleActionSettings' Core.<$>
                  (x Core..: "image") Core.<*> x Core..:? "duration" Core.<*>
                    x Core..:? "fadeIn"
                    Core.<*> x Core..:? "fadeOut"
                    Core.<*> x Core..:? "height"
                    Core.<*> x Core..:? "imageX"
                    Core.<*> x Core..:? "imageY"
                    Core.<*> x Core..:? "layer"
                    Core.<*> x Core..:? "opacity"
                    Core.<*> x Core..:? "width"
