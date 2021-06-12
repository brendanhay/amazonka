{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.StaticImageActivateScheduleActionSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.StaticImageActivateScheduleActionSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.InputLocation

-- | Settings for the action to activate a static image.
--
-- /See:/ 'newStaticImageActivateScheduleActionSettings' smart constructor.
data StaticImageActivateScheduleActionSettings = StaticImageActivateScheduleActionSettings'
  { -- | The height of the image when inserted into the video, in pixels. The
    -- overlay will be scaled up or down to the specified height. Leave blank
    -- to use the native height of the overlay.
    height :: Core.Maybe Core.Natural,
    -- | Placement of the left edge of the overlay relative to the left edge of
    -- the video frame, in pixels. 0 (the default) is the left edge of the
    -- frame. If the placement causes the overlay to extend beyond the right
    -- edge of the underlying video, then the overlay is cropped on the right.
    imageX :: Core.Maybe Core.Natural,
    -- | Placement of the top edge of the overlay relative to the top edge of the
    -- video frame, in pixels. 0 (the default) is the top edge of the frame. If
    -- the placement causes the overlay to extend beyond the bottom edge of the
    -- underlying video, then the overlay is cropped on the bottom.
    imageY :: Core.Maybe Core.Natural,
    -- | The duration in milliseconds for the image to remain on the video. If
    -- omitted or set to 0 the duration is unlimited and the image will remain
    -- until it is explicitly deactivated.
    duration :: Core.Maybe Core.Natural,
    -- | The width of the image when inserted into the video, in pixels. The
    -- overlay will be scaled up or down to the specified width. Leave blank to
    -- use the native width of the overlay.
    width :: Core.Maybe Core.Natural,
    -- | The number of the layer, 0 to 7. There are 8 layers that can be overlaid
    -- on the video, each layer with a different image. The layers are in Z
    -- order, which means that overlays with higher values of layer are
    -- inserted on top of overlays with lower values of layer. Default is 0.
    layer :: Core.Maybe Core.Natural,
    -- | Opacity of image where 0 is transparent and 100 is fully opaque. Default
    -- is 100.
    opacity :: Core.Maybe Core.Natural,
    -- | The time in milliseconds for the image to fade in. The fade-in starts at
    -- the start time of the overlay. Default is 0 (no fade-in).
    fadeIn :: Core.Maybe Core.Natural,
    -- | Applies only if a duration is specified. The time in milliseconds for
    -- the image to fade out. The fade-out starts when the duration time is
    -- hit, so it effectively extends the duration. Default is 0 (no fade-out).
    fadeOut :: Core.Maybe Core.Natural,
    -- | The location and filename of the image file to overlay on the video. The
    -- file must be a 32-bit BMP, PNG, or TGA file, and must not be larger (in
    -- pixels) than the input video.
    image :: InputLocation
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StaticImageActivateScheduleActionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'height', 'staticImageActivateScheduleActionSettings_height' - The height of the image when inserted into the video, in pixels. The
-- overlay will be scaled up or down to the specified height. Leave blank
-- to use the native height of the overlay.
--
-- 'imageX', 'staticImageActivateScheduleActionSettings_imageX' - Placement of the left edge of the overlay relative to the left edge of
-- the video frame, in pixels. 0 (the default) is the left edge of the
-- frame. If the placement causes the overlay to extend beyond the right
-- edge of the underlying video, then the overlay is cropped on the right.
--
-- 'imageY', 'staticImageActivateScheduleActionSettings_imageY' - Placement of the top edge of the overlay relative to the top edge of the
-- video frame, in pixels. 0 (the default) is the top edge of the frame. If
-- the placement causes the overlay to extend beyond the bottom edge of the
-- underlying video, then the overlay is cropped on the bottom.
--
-- 'duration', 'staticImageActivateScheduleActionSettings_duration' - The duration in milliseconds for the image to remain on the video. If
-- omitted or set to 0 the duration is unlimited and the image will remain
-- until it is explicitly deactivated.
--
-- 'width', 'staticImageActivateScheduleActionSettings_width' - The width of the image when inserted into the video, in pixels. The
-- overlay will be scaled up or down to the specified width. Leave blank to
-- use the native width of the overlay.
--
-- 'layer', 'staticImageActivateScheduleActionSettings_layer' - The number of the layer, 0 to 7. There are 8 layers that can be overlaid
-- on the video, each layer with a different image. The layers are in Z
-- order, which means that overlays with higher values of layer are
-- inserted on top of overlays with lower values of layer. Default is 0.
--
-- 'opacity', 'staticImageActivateScheduleActionSettings_opacity' - Opacity of image where 0 is transparent and 100 is fully opaque. Default
-- is 100.
--
-- 'fadeIn', 'staticImageActivateScheduleActionSettings_fadeIn' - The time in milliseconds for the image to fade in. The fade-in starts at
-- the start time of the overlay. Default is 0 (no fade-in).
--
-- 'fadeOut', 'staticImageActivateScheduleActionSettings_fadeOut' - Applies only if a duration is specified. The time in milliseconds for
-- the image to fade out. The fade-out starts when the duration time is
-- hit, so it effectively extends the duration. Default is 0 (no fade-out).
--
-- 'image', 'staticImageActivateScheduleActionSettings_image' - The location and filename of the image file to overlay on the video. The
-- file must be a 32-bit BMP, PNG, or TGA file, and must not be larger (in
-- pixels) than the input video.
newStaticImageActivateScheduleActionSettings ::
  -- | 'image'
  InputLocation ->
  StaticImageActivateScheduleActionSettings
newStaticImageActivateScheduleActionSettings pImage_ =
  StaticImageActivateScheduleActionSettings'
    { height =
        Core.Nothing,
      imageX = Core.Nothing,
      imageY = Core.Nothing,
      duration = Core.Nothing,
      width = Core.Nothing,
      layer = Core.Nothing,
      opacity = Core.Nothing,
      fadeIn = Core.Nothing,
      fadeOut = Core.Nothing,
      image = pImage_
    }

-- | The height of the image when inserted into the video, in pixels. The
-- overlay will be scaled up or down to the specified height. Leave blank
-- to use the native height of the overlay.
staticImageActivateScheduleActionSettings_height :: Lens.Lens' StaticImageActivateScheduleActionSettings (Core.Maybe Core.Natural)
staticImageActivateScheduleActionSettings_height = Lens.lens (\StaticImageActivateScheduleActionSettings' {height} -> height) (\s@StaticImageActivateScheduleActionSettings' {} a -> s {height = a} :: StaticImageActivateScheduleActionSettings)

-- | Placement of the left edge of the overlay relative to the left edge of
-- the video frame, in pixels. 0 (the default) is the left edge of the
-- frame. If the placement causes the overlay to extend beyond the right
-- edge of the underlying video, then the overlay is cropped on the right.
staticImageActivateScheduleActionSettings_imageX :: Lens.Lens' StaticImageActivateScheduleActionSettings (Core.Maybe Core.Natural)
staticImageActivateScheduleActionSettings_imageX = Lens.lens (\StaticImageActivateScheduleActionSettings' {imageX} -> imageX) (\s@StaticImageActivateScheduleActionSettings' {} a -> s {imageX = a} :: StaticImageActivateScheduleActionSettings)

-- | Placement of the top edge of the overlay relative to the top edge of the
-- video frame, in pixels. 0 (the default) is the top edge of the frame. If
-- the placement causes the overlay to extend beyond the bottom edge of the
-- underlying video, then the overlay is cropped on the bottom.
staticImageActivateScheduleActionSettings_imageY :: Lens.Lens' StaticImageActivateScheduleActionSettings (Core.Maybe Core.Natural)
staticImageActivateScheduleActionSettings_imageY = Lens.lens (\StaticImageActivateScheduleActionSettings' {imageY} -> imageY) (\s@StaticImageActivateScheduleActionSettings' {} a -> s {imageY = a} :: StaticImageActivateScheduleActionSettings)

-- | The duration in milliseconds for the image to remain on the video. If
-- omitted or set to 0 the duration is unlimited and the image will remain
-- until it is explicitly deactivated.
staticImageActivateScheduleActionSettings_duration :: Lens.Lens' StaticImageActivateScheduleActionSettings (Core.Maybe Core.Natural)
staticImageActivateScheduleActionSettings_duration = Lens.lens (\StaticImageActivateScheduleActionSettings' {duration} -> duration) (\s@StaticImageActivateScheduleActionSettings' {} a -> s {duration = a} :: StaticImageActivateScheduleActionSettings)

-- | The width of the image when inserted into the video, in pixels. The
-- overlay will be scaled up or down to the specified width. Leave blank to
-- use the native width of the overlay.
staticImageActivateScheduleActionSettings_width :: Lens.Lens' StaticImageActivateScheduleActionSettings (Core.Maybe Core.Natural)
staticImageActivateScheduleActionSettings_width = Lens.lens (\StaticImageActivateScheduleActionSettings' {width} -> width) (\s@StaticImageActivateScheduleActionSettings' {} a -> s {width = a} :: StaticImageActivateScheduleActionSettings)

-- | The number of the layer, 0 to 7. There are 8 layers that can be overlaid
-- on the video, each layer with a different image. The layers are in Z
-- order, which means that overlays with higher values of layer are
-- inserted on top of overlays with lower values of layer. Default is 0.
staticImageActivateScheduleActionSettings_layer :: Lens.Lens' StaticImageActivateScheduleActionSettings (Core.Maybe Core.Natural)
staticImageActivateScheduleActionSettings_layer = Lens.lens (\StaticImageActivateScheduleActionSettings' {layer} -> layer) (\s@StaticImageActivateScheduleActionSettings' {} a -> s {layer = a} :: StaticImageActivateScheduleActionSettings)

-- | Opacity of image where 0 is transparent and 100 is fully opaque. Default
-- is 100.
staticImageActivateScheduleActionSettings_opacity :: Lens.Lens' StaticImageActivateScheduleActionSettings (Core.Maybe Core.Natural)
staticImageActivateScheduleActionSettings_opacity = Lens.lens (\StaticImageActivateScheduleActionSettings' {opacity} -> opacity) (\s@StaticImageActivateScheduleActionSettings' {} a -> s {opacity = a} :: StaticImageActivateScheduleActionSettings)

-- | The time in milliseconds for the image to fade in. The fade-in starts at
-- the start time of the overlay. Default is 0 (no fade-in).
staticImageActivateScheduleActionSettings_fadeIn :: Lens.Lens' StaticImageActivateScheduleActionSettings (Core.Maybe Core.Natural)
staticImageActivateScheduleActionSettings_fadeIn = Lens.lens (\StaticImageActivateScheduleActionSettings' {fadeIn} -> fadeIn) (\s@StaticImageActivateScheduleActionSettings' {} a -> s {fadeIn = a} :: StaticImageActivateScheduleActionSettings)

-- | Applies only if a duration is specified. The time in milliseconds for
-- the image to fade out. The fade-out starts when the duration time is
-- hit, so it effectively extends the duration. Default is 0 (no fade-out).
staticImageActivateScheduleActionSettings_fadeOut :: Lens.Lens' StaticImageActivateScheduleActionSettings (Core.Maybe Core.Natural)
staticImageActivateScheduleActionSettings_fadeOut = Lens.lens (\StaticImageActivateScheduleActionSettings' {fadeOut} -> fadeOut) (\s@StaticImageActivateScheduleActionSettings' {} a -> s {fadeOut = a} :: StaticImageActivateScheduleActionSettings)

-- | The location and filename of the image file to overlay on the video. The
-- file must be a 32-bit BMP, PNG, or TGA file, and must not be larger (in
-- pixels) than the input video.
staticImageActivateScheduleActionSettings_image :: Lens.Lens' StaticImageActivateScheduleActionSettings InputLocation
staticImageActivateScheduleActionSettings_image = Lens.lens (\StaticImageActivateScheduleActionSettings' {image} -> image) (\s@StaticImageActivateScheduleActionSettings' {} a -> s {image = a} :: StaticImageActivateScheduleActionSettings)

instance
  Core.FromJSON
    StaticImageActivateScheduleActionSettings
  where
  parseJSON =
    Core.withObject
      "StaticImageActivateScheduleActionSettings"
      ( \x ->
          StaticImageActivateScheduleActionSettings'
            Core.<$> (x Core..:? "height")
            Core.<*> (x Core..:? "imageX")
            Core.<*> (x Core..:? "imageY")
            Core.<*> (x Core..:? "duration")
            Core.<*> (x Core..:? "width")
            Core.<*> (x Core..:? "layer")
            Core.<*> (x Core..:? "opacity")
            Core.<*> (x Core..:? "fadeIn")
            Core.<*> (x Core..:? "fadeOut")
            Core.<*> (x Core..: "image")
      )

instance
  Core.Hashable
    StaticImageActivateScheduleActionSettings

instance
  Core.NFData
    StaticImageActivateScheduleActionSettings

instance
  Core.ToJSON
    StaticImageActivateScheduleActionSettings
  where
  toJSON StaticImageActivateScheduleActionSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("height" Core..=) Core.<$> height,
            ("imageX" Core..=) Core.<$> imageX,
            ("imageY" Core..=) Core.<$> imageY,
            ("duration" Core..=) Core.<$> duration,
            ("width" Core..=) Core.<$> width,
            ("layer" Core..=) Core.<$> layer,
            ("opacity" Core..=) Core.<$> opacity,
            ("fadeIn" Core..=) Core.<$> fadeIn,
            ("fadeOut" Core..=) Core.<$> fadeOut,
            Core.Just ("image" Core..= image)
          ]
      )
