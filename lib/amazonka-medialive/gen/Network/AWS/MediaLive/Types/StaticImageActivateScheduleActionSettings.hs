{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.StaticImageActivateScheduleActionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.StaticImageActivateScheduleActionSettings
  ( StaticImageActivateScheduleActionSettings (..),

    -- * Smart constructor
    mkStaticImageActivateScheduleActionSettings,

    -- * Lenses
    siasasImageX,
    siasasHeight,
    siasasFadeOut,
    siasasWidth,
    siasasOpacity,
    siasasLayer,
    siasasDuration,
    siasasImageY,
    siasasFadeIn,
    siasasImage,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.InputLocation
import qualified Network.AWS.Prelude as Lude

-- | Settings for the action to activate a static image.
--
-- /See:/ 'mkStaticImageActivateScheduleActionSettings' smart constructor.
data StaticImageActivateScheduleActionSettings = StaticImageActivateScheduleActionSettings'
  { imageX ::
      Lude.Maybe
        Lude.Natural,
    height ::
      Lude.Maybe
        Lude.Natural,
    fadeOut ::
      Lude.Maybe
        Lude.Natural,
    width ::
      Lude.Maybe
        Lude.Natural,
    opacity ::
      Lude.Maybe
        Lude.Natural,
    layer ::
      Lude.Maybe
        Lude.Natural,
    duration ::
      Lude.Maybe
        Lude.Natural,
    imageY ::
      Lude.Maybe
        Lude.Natural,
    fadeIn ::
      Lude.Maybe
        Lude.Natural,
    image ::
      InputLocation
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StaticImageActivateScheduleActionSettings' with the minimum fields required to make a request.
--
-- * 'duration' - The duration in milliseconds for the image to remain on the video. If omitted or set to 0 the duration is unlimited and the image will remain until it is explicitly deactivated.
-- * 'fadeIn' - The time in milliseconds for the image to fade in. The fade-in starts at the start time of the overlay. Default is 0 (no fade-in).
-- * 'fadeOut' - Applies only if a duration is specified. The time in milliseconds for the image to fade out. The fade-out starts when the duration time is hit, so it effectively extends the duration. Default is 0 (no fade-out).
-- * 'height' - The height of the image when inserted into the video, in pixels. The overlay will be scaled up or down to the specified height. Leave blank to use the native height of the overlay.
-- * 'image' - The location and filename of the image file to overlay on the video. The file must be a 32-bit BMP, PNG, or TGA file, and must not be larger (in pixels) than the input video.
-- * 'imageX' - Placement of the left edge of the overlay relative to the left edge of the video frame, in pixels. 0 (the default) is the left edge of the frame. If the placement causes the overlay to extend beyond the right edge of the underlying video, then the overlay is cropped on the right.
-- * 'imageY' - Placement of the top edge of the overlay relative to the top edge of the video frame, in pixels. 0 (the default) is the top edge of the frame. If the placement causes the overlay to extend beyond the bottom edge of the underlying video, then the overlay is cropped on the bottom.
-- * 'layer' - The number of the layer, 0 to 7. There are 8 layers that can be overlaid on the video, each layer with a different image. The layers are in Z order, which means that overlays with higher values of layer are inserted on top of overlays with lower values of layer. Default is 0.
-- * 'opacity' - Opacity of image where 0 is transparent and 100 is fully opaque. Default is 100.
-- * 'width' - The width of the image when inserted into the video, in pixels. The overlay will be scaled up or down to the specified width. Leave blank to use the native width of the overlay.
mkStaticImageActivateScheduleActionSettings ::
  -- | 'image'
  InputLocation ->
  StaticImageActivateScheduleActionSettings
mkStaticImageActivateScheduleActionSettings pImage_ =
  StaticImageActivateScheduleActionSettings'
    { imageX = Lude.Nothing,
      height = Lude.Nothing,
      fadeOut = Lude.Nothing,
      width = Lude.Nothing,
      opacity = Lude.Nothing,
      layer = Lude.Nothing,
      duration = Lude.Nothing,
      imageY = Lude.Nothing,
      fadeIn = Lude.Nothing,
      image = pImage_
    }

-- | Placement of the left edge of the overlay relative to the left edge of the video frame, in pixels. 0 (the default) is the left edge of the frame. If the placement causes the overlay to extend beyond the right edge of the underlying video, then the overlay is cropped on the right.
--
-- /Note:/ Consider using 'imageX' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siasasImageX :: Lens.Lens' StaticImageActivateScheduleActionSettings (Lude.Maybe Lude.Natural)
siasasImageX = Lens.lens (imageX :: StaticImageActivateScheduleActionSettings -> Lude.Maybe Lude.Natural) (\s a -> s {imageX = a} :: StaticImageActivateScheduleActionSettings)
{-# DEPRECATED siasasImageX "Use generic-lens or generic-optics with 'imageX' instead." #-}

-- | The height of the image when inserted into the video, in pixels. The overlay will be scaled up or down to the specified height. Leave blank to use the native height of the overlay.
--
-- /Note:/ Consider using 'height' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siasasHeight :: Lens.Lens' StaticImageActivateScheduleActionSettings (Lude.Maybe Lude.Natural)
siasasHeight = Lens.lens (height :: StaticImageActivateScheduleActionSettings -> Lude.Maybe Lude.Natural) (\s a -> s {height = a} :: StaticImageActivateScheduleActionSettings)
{-# DEPRECATED siasasHeight "Use generic-lens or generic-optics with 'height' instead." #-}

-- | Applies only if a duration is specified. The time in milliseconds for the image to fade out. The fade-out starts when the duration time is hit, so it effectively extends the duration. Default is 0 (no fade-out).
--
-- /Note:/ Consider using 'fadeOut' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siasasFadeOut :: Lens.Lens' StaticImageActivateScheduleActionSettings (Lude.Maybe Lude.Natural)
siasasFadeOut = Lens.lens (fadeOut :: StaticImageActivateScheduleActionSettings -> Lude.Maybe Lude.Natural) (\s a -> s {fadeOut = a} :: StaticImageActivateScheduleActionSettings)
{-# DEPRECATED siasasFadeOut "Use generic-lens or generic-optics with 'fadeOut' instead." #-}

-- | The width of the image when inserted into the video, in pixels. The overlay will be scaled up or down to the specified width. Leave blank to use the native width of the overlay.
--
-- /Note:/ Consider using 'width' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siasasWidth :: Lens.Lens' StaticImageActivateScheduleActionSettings (Lude.Maybe Lude.Natural)
siasasWidth = Lens.lens (width :: StaticImageActivateScheduleActionSettings -> Lude.Maybe Lude.Natural) (\s a -> s {width = a} :: StaticImageActivateScheduleActionSettings)
{-# DEPRECATED siasasWidth "Use generic-lens or generic-optics with 'width' instead." #-}

-- | Opacity of image where 0 is transparent and 100 is fully opaque. Default is 100.
--
-- /Note:/ Consider using 'opacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siasasOpacity :: Lens.Lens' StaticImageActivateScheduleActionSettings (Lude.Maybe Lude.Natural)
siasasOpacity = Lens.lens (opacity :: StaticImageActivateScheduleActionSettings -> Lude.Maybe Lude.Natural) (\s a -> s {opacity = a} :: StaticImageActivateScheduleActionSettings)
{-# DEPRECATED siasasOpacity "Use generic-lens or generic-optics with 'opacity' instead." #-}

-- | The number of the layer, 0 to 7. There are 8 layers that can be overlaid on the video, each layer with a different image. The layers are in Z order, which means that overlays with higher values of layer are inserted on top of overlays with lower values of layer. Default is 0.
--
-- /Note:/ Consider using 'layer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siasasLayer :: Lens.Lens' StaticImageActivateScheduleActionSettings (Lude.Maybe Lude.Natural)
siasasLayer = Lens.lens (layer :: StaticImageActivateScheduleActionSettings -> Lude.Maybe Lude.Natural) (\s a -> s {layer = a} :: StaticImageActivateScheduleActionSettings)
{-# DEPRECATED siasasLayer "Use generic-lens or generic-optics with 'layer' instead." #-}

-- | The duration in milliseconds for the image to remain on the video. If omitted or set to 0 the duration is unlimited and the image will remain until it is explicitly deactivated.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siasasDuration :: Lens.Lens' StaticImageActivateScheduleActionSettings (Lude.Maybe Lude.Natural)
siasasDuration = Lens.lens (duration :: StaticImageActivateScheduleActionSettings -> Lude.Maybe Lude.Natural) (\s a -> s {duration = a} :: StaticImageActivateScheduleActionSettings)
{-# DEPRECATED siasasDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | Placement of the top edge of the overlay relative to the top edge of the video frame, in pixels. 0 (the default) is the top edge of the frame. If the placement causes the overlay to extend beyond the bottom edge of the underlying video, then the overlay is cropped on the bottom.
--
-- /Note:/ Consider using 'imageY' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siasasImageY :: Lens.Lens' StaticImageActivateScheduleActionSettings (Lude.Maybe Lude.Natural)
siasasImageY = Lens.lens (imageY :: StaticImageActivateScheduleActionSettings -> Lude.Maybe Lude.Natural) (\s a -> s {imageY = a} :: StaticImageActivateScheduleActionSettings)
{-# DEPRECATED siasasImageY "Use generic-lens or generic-optics with 'imageY' instead." #-}

-- | The time in milliseconds for the image to fade in. The fade-in starts at the start time of the overlay. Default is 0 (no fade-in).
--
-- /Note:/ Consider using 'fadeIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siasasFadeIn :: Lens.Lens' StaticImageActivateScheduleActionSettings (Lude.Maybe Lude.Natural)
siasasFadeIn = Lens.lens (fadeIn :: StaticImageActivateScheduleActionSettings -> Lude.Maybe Lude.Natural) (\s a -> s {fadeIn = a} :: StaticImageActivateScheduleActionSettings)
{-# DEPRECATED siasasFadeIn "Use generic-lens or generic-optics with 'fadeIn' instead." #-}

-- | The location and filename of the image file to overlay on the video. The file must be a 32-bit BMP, PNG, or TGA file, and must not be larger (in pixels) than the input video.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siasasImage :: Lens.Lens' StaticImageActivateScheduleActionSettings InputLocation
siasasImage = Lens.lens (image :: StaticImageActivateScheduleActionSettings -> InputLocation) (\s a -> s {image = a} :: StaticImageActivateScheduleActionSettings)
{-# DEPRECATED siasasImage "Use generic-lens or generic-optics with 'image' instead." #-}

instance Lude.FromJSON StaticImageActivateScheduleActionSettings where
  parseJSON =
    Lude.withObject
      "StaticImageActivateScheduleActionSettings"
      ( \x ->
          StaticImageActivateScheduleActionSettings'
            Lude.<$> (x Lude..:? "imageX")
            Lude.<*> (x Lude..:? "height")
            Lude.<*> (x Lude..:? "fadeOut")
            Lude.<*> (x Lude..:? "width")
            Lude.<*> (x Lude..:? "opacity")
            Lude.<*> (x Lude..:? "layer")
            Lude.<*> (x Lude..:? "duration")
            Lude.<*> (x Lude..:? "imageY")
            Lude.<*> (x Lude..:? "fadeIn")
            Lude.<*> (x Lude..: "image")
      )

instance Lude.ToJSON StaticImageActivateScheduleActionSettings where
  toJSON StaticImageActivateScheduleActionSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("imageX" Lude..=) Lude.<$> imageX,
            ("height" Lude..=) Lude.<$> height,
            ("fadeOut" Lude..=) Lude.<$> fadeOut,
            ("width" Lude..=) Lude.<$> width,
            ("opacity" Lude..=) Lude.<$> opacity,
            ("layer" Lude..=) Lude.<$> layer,
            ("duration" Lude..=) Lude.<$> duration,
            ("imageY" Lude..=) Lude.<$> imageY,
            ("fadeIn" Lude..=) Lude.<$> fadeIn,
            Lude.Just ("image" Lude..= image)
          ]
      )
