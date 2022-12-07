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
-- Module      : Amazonka.MediaLive.Types.StaticImageActivateScheduleActionSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.StaticImageActivateScheduleActionSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.InputLocation
import qualified Amazonka.Prelude as Prelude

-- | Settings for the action to activate a static image.
--
-- /See:/ 'newStaticImageActivateScheduleActionSettings' smart constructor.
data StaticImageActivateScheduleActionSettings = StaticImageActivateScheduleActionSettings'
  { -- | Applies only if a duration is specified. The time in milliseconds for
    -- the image to fade out. The fade-out starts when the duration time is
    -- hit, so it effectively extends the duration. Default is 0 (no fade-out).
    fadeOut :: Prelude.Maybe Prelude.Natural,
    -- | Placement of the left edge of the overlay relative to the left edge of
    -- the video frame, in pixels. 0 (the default) is the left edge of the
    -- frame. If the placement causes the overlay to extend beyond the right
    -- edge of the underlying video, then the overlay is cropped on the right.
    imageX :: Prelude.Maybe Prelude.Natural,
    -- | The width of the image when inserted into the video, in pixels. The
    -- overlay will be scaled up or down to the specified width. Leave blank to
    -- use the native width of the overlay.
    width :: Prelude.Maybe Prelude.Natural,
    -- | The duration in milliseconds for the image to remain on the video. If
    -- omitted or set to 0 the duration is unlimited and the image will remain
    -- until it is explicitly deactivated.
    duration :: Prelude.Maybe Prelude.Natural,
    -- | The number of the layer, 0 to 7. There are 8 layers that can be overlaid
    -- on the video, each layer with a different image. The layers are in Z
    -- order, which means that overlays with higher values of layer are
    -- inserted on top of overlays with lower values of layer. Default is 0.
    layer :: Prelude.Maybe Prelude.Natural,
    -- | Opacity of image where 0 is transparent and 100 is fully opaque. Default
    -- is 100.
    opacity :: Prelude.Maybe Prelude.Natural,
    -- | The height of the image when inserted into the video, in pixels. The
    -- overlay will be scaled up or down to the specified height. Leave blank
    -- to use the native height of the overlay.
    height :: Prelude.Maybe Prelude.Natural,
    -- | The time in milliseconds for the image to fade in. The fade-in starts at
    -- the start time of the overlay. Default is 0 (no fade-in).
    fadeIn :: Prelude.Maybe Prelude.Natural,
    -- | Placement of the top edge of the overlay relative to the top edge of the
    -- video frame, in pixels. 0 (the default) is the top edge of the frame. If
    -- the placement causes the overlay to extend beyond the bottom edge of the
    -- underlying video, then the overlay is cropped on the bottom.
    imageY :: Prelude.Maybe Prelude.Natural,
    -- | The location and filename of the image file to overlay on the video. The
    -- file must be a 32-bit BMP, PNG, or TGA file, and must not be larger (in
    -- pixels) than the input video.
    image :: InputLocation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StaticImageActivateScheduleActionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fadeOut', 'staticImageActivateScheduleActionSettings_fadeOut' - Applies only if a duration is specified. The time in milliseconds for
-- the image to fade out. The fade-out starts when the duration time is
-- hit, so it effectively extends the duration. Default is 0 (no fade-out).
--
-- 'imageX', 'staticImageActivateScheduleActionSettings_imageX' - Placement of the left edge of the overlay relative to the left edge of
-- the video frame, in pixels. 0 (the default) is the left edge of the
-- frame. If the placement causes the overlay to extend beyond the right
-- edge of the underlying video, then the overlay is cropped on the right.
--
-- 'width', 'staticImageActivateScheduleActionSettings_width' - The width of the image when inserted into the video, in pixels. The
-- overlay will be scaled up or down to the specified width. Leave blank to
-- use the native width of the overlay.
--
-- 'duration', 'staticImageActivateScheduleActionSettings_duration' - The duration in milliseconds for the image to remain on the video. If
-- omitted or set to 0 the duration is unlimited and the image will remain
-- until it is explicitly deactivated.
--
-- 'layer', 'staticImageActivateScheduleActionSettings_layer' - The number of the layer, 0 to 7. There are 8 layers that can be overlaid
-- on the video, each layer with a different image. The layers are in Z
-- order, which means that overlays with higher values of layer are
-- inserted on top of overlays with lower values of layer. Default is 0.
--
-- 'opacity', 'staticImageActivateScheduleActionSettings_opacity' - Opacity of image where 0 is transparent and 100 is fully opaque. Default
-- is 100.
--
-- 'height', 'staticImageActivateScheduleActionSettings_height' - The height of the image when inserted into the video, in pixels. The
-- overlay will be scaled up or down to the specified height. Leave blank
-- to use the native height of the overlay.
--
-- 'fadeIn', 'staticImageActivateScheduleActionSettings_fadeIn' - The time in milliseconds for the image to fade in. The fade-in starts at
-- the start time of the overlay. Default is 0 (no fade-in).
--
-- 'imageY', 'staticImageActivateScheduleActionSettings_imageY' - Placement of the top edge of the overlay relative to the top edge of the
-- video frame, in pixels. 0 (the default) is the top edge of the frame. If
-- the placement causes the overlay to extend beyond the bottom edge of the
-- underlying video, then the overlay is cropped on the bottom.
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
    { fadeOut =
        Prelude.Nothing,
      imageX = Prelude.Nothing,
      width = Prelude.Nothing,
      duration = Prelude.Nothing,
      layer = Prelude.Nothing,
      opacity = Prelude.Nothing,
      height = Prelude.Nothing,
      fadeIn = Prelude.Nothing,
      imageY = Prelude.Nothing,
      image = pImage_
    }

-- | Applies only if a duration is specified. The time in milliseconds for
-- the image to fade out. The fade-out starts when the duration time is
-- hit, so it effectively extends the duration. Default is 0 (no fade-out).
staticImageActivateScheduleActionSettings_fadeOut :: Lens.Lens' StaticImageActivateScheduleActionSettings (Prelude.Maybe Prelude.Natural)
staticImageActivateScheduleActionSettings_fadeOut = Lens.lens (\StaticImageActivateScheduleActionSettings' {fadeOut} -> fadeOut) (\s@StaticImageActivateScheduleActionSettings' {} a -> s {fadeOut = a} :: StaticImageActivateScheduleActionSettings)

-- | Placement of the left edge of the overlay relative to the left edge of
-- the video frame, in pixels. 0 (the default) is the left edge of the
-- frame. If the placement causes the overlay to extend beyond the right
-- edge of the underlying video, then the overlay is cropped on the right.
staticImageActivateScheduleActionSettings_imageX :: Lens.Lens' StaticImageActivateScheduleActionSettings (Prelude.Maybe Prelude.Natural)
staticImageActivateScheduleActionSettings_imageX = Lens.lens (\StaticImageActivateScheduleActionSettings' {imageX} -> imageX) (\s@StaticImageActivateScheduleActionSettings' {} a -> s {imageX = a} :: StaticImageActivateScheduleActionSettings)

-- | The width of the image when inserted into the video, in pixels. The
-- overlay will be scaled up or down to the specified width. Leave blank to
-- use the native width of the overlay.
staticImageActivateScheduleActionSettings_width :: Lens.Lens' StaticImageActivateScheduleActionSettings (Prelude.Maybe Prelude.Natural)
staticImageActivateScheduleActionSettings_width = Lens.lens (\StaticImageActivateScheduleActionSettings' {width} -> width) (\s@StaticImageActivateScheduleActionSettings' {} a -> s {width = a} :: StaticImageActivateScheduleActionSettings)

-- | The duration in milliseconds for the image to remain on the video. If
-- omitted or set to 0 the duration is unlimited and the image will remain
-- until it is explicitly deactivated.
staticImageActivateScheduleActionSettings_duration :: Lens.Lens' StaticImageActivateScheduleActionSettings (Prelude.Maybe Prelude.Natural)
staticImageActivateScheduleActionSettings_duration = Lens.lens (\StaticImageActivateScheduleActionSettings' {duration} -> duration) (\s@StaticImageActivateScheduleActionSettings' {} a -> s {duration = a} :: StaticImageActivateScheduleActionSettings)

-- | The number of the layer, 0 to 7. There are 8 layers that can be overlaid
-- on the video, each layer with a different image. The layers are in Z
-- order, which means that overlays with higher values of layer are
-- inserted on top of overlays with lower values of layer. Default is 0.
staticImageActivateScheduleActionSettings_layer :: Lens.Lens' StaticImageActivateScheduleActionSettings (Prelude.Maybe Prelude.Natural)
staticImageActivateScheduleActionSettings_layer = Lens.lens (\StaticImageActivateScheduleActionSettings' {layer} -> layer) (\s@StaticImageActivateScheduleActionSettings' {} a -> s {layer = a} :: StaticImageActivateScheduleActionSettings)

-- | Opacity of image where 0 is transparent and 100 is fully opaque. Default
-- is 100.
staticImageActivateScheduleActionSettings_opacity :: Lens.Lens' StaticImageActivateScheduleActionSettings (Prelude.Maybe Prelude.Natural)
staticImageActivateScheduleActionSettings_opacity = Lens.lens (\StaticImageActivateScheduleActionSettings' {opacity} -> opacity) (\s@StaticImageActivateScheduleActionSettings' {} a -> s {opacity = a} :: StaticImageActivateScheduleActionSettings)

-- | The height of the image when inserted into the video, in pixels. The
-- overlay will be scaled up or down to the specified height. Leave blank
-- to use the native height of the overlay.
staticImageActivateScheduleActionSettings_height :: Lens.Lens' StaticImageActivateScheduleActionSettings (Prelude.Maybe Prelude.Natural)
staticImageActivateScheduleActionSettings_height = Lens.lens (\StaticImageActivateScheduleActionSettings' {height} -> height) (\s@StaticImageActivateScheduleActionSettings' {} a -> s {height = a} :: StaticImageActivateScheduleActionSettings)

-- | The time in milliseconds for the image to fade in. The fade-in starts at
-- the start time of the overlay. Default is 0 (no fade-in).
staticImageActivateScheduleActionSettings_fadeIn :: Lens.Lens' StaticImageActivateScheduleActionSettings (Prelude.Maybe Prelude.Natural)
staticImageActivateScheduleActionSettings_fadeIn = Lens.lens (\StaticImageActivateScheduleActionSettings' {fadeIn} -> fadeIn) (\s@StaticImageActivateScheduleActionSettings' {} a -> s {fadeIn = a} :: StaticImageActivateScheduleActionSettings)

-- | Placement of the top edge of the overlay relative to the top edge of the
-- video frame, in pixels. 0 (the default) is the top edge of the frame. If
-- the placement causes the overlay to extend beyond the bottom edge of the
-- underlying video, then the overlay is cropped on the bottom.
staticImageActivateScheduleActionSettings_imageY :: Lens.Lens' StaticImageActivateScheduleActionSettings (Prelude.Maybe Prelude.Natural)
staticImageActivateScheduleActionSettings_imageY = Lens.lens (\StaticImageActivateScheduleActionSettings' {imageY} -> imageY) (\s@StaticImageActivateScheduleActionSettings' {} a -> s {imageY = a} :: StaticImageActivateScheduleActionSettings)

-- | The location and filename of the image file to overlay on the video. The
-- file must be a 32-bit BMP, PNG, or TGA file, and must not be larger (in
-- pixels) than the input video.
staticImageActivateScheduleActionSettings_image :: Lens.Lens' StaticImageActivateScheduleActionSettings InputLocation
staticImageActivateScheduleActionSettings_image = Lens.lens (\StaticImageActivateScheduleActionSettings' {image} -> image) (\s@StaticImageActivateScheduleActionSettings' {} a -> s {image = a} :: StaticImageActivateScheduleActionSettings)

instance
  Data.FromJSON
    StaticImageActivateScheduleActionSettings
  where
  parseJSON =
    Data.withObject
      "StaticImageActivateScheduleActionSettings"
      ( \x ->
          StaticImageActivateScheduleActionSettings'
            Prelude.<$> (x Data..:? "fadeOut")
              Prelude.<*> (x Data..:? "imageX")
              Prelude.<*> (x Data..:? "width")
              Prelude.<*> (x Data..:? "duration")
              Prelude.<*> (x Data..:? "layer")
              Prelude.<*> (x Data..:? "opacity")
              Prelude.<*> (x Data..:? "height")
              Prelude.<*> (x Data..:? "fadeIn")
              Prelude.<*> (x Data..:? "imageY")
              Prelude.<*> (x Data..: "image")
      )

instance
  Prelude.Hashable
    StaticImageActivateScheduleActionSettings
  where
  hashWithSalt
    _salt
    StaticImageActivateScheduleActionSettings' {..} =
      _salt `Prelude.hashWithSalt` fadeOut
        `Prelude.hashWithSalt` imageX
        `Prelude.hashWithSalt` width
        `Prelude.hashWithSalt` duration
        `Prelude.hashWithSalt` layer
        `Prelude.hashWithSalt` opacity
        `Prelude.hashWithSalt` height
        `Prelude.hashWithSalt` fadeIn
        `Prelude.hashWithSalt` imageY
        `Prelude.hashWithSalt` image

instance
  Prelude.NFData
    StaticImageActivateScheduleActionSettings
  where
  rnf StaticImageActivateScheduleActionSettings' {..} =
    Prelude.rnf fadeOut
      `Prelude.seq` Prelude.rnf imageX
      `Prelude.seq` Prelude.rnf width
      `Prelude.seq` Prelude.rnf duration
      `Prelude.seq` Prelude.rnf layer
      `Prelude.seq` Prelude.rnf opacity
      `Prelude.seq` Prelude.rnf height
      `Prelude.seq` Prelude.rnf fadeIn
      `Prelude.seq` Prelude.rnf imageY
      `Prelude.seq` Prelude.rnf image

instance
  Data.ToJSON
    StaticImageActivateScheduleActionSettings
  where
  toJSON StaticImageActivateScheduleActionSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("fadeOut" Data..=) Prelude.<$> fadeOut,
            ("imageX" Data..=) Prelude.<$> imageX,
            ("width" Data..=) Prelude.<$> width,
            ("duration" Data..=) Prelude.<$> duration,
            ("layer" Data..=) Prelude.<$> layer,
            ("opacity" Data..=) Prelude.<$> opacity,
            ("height" Data..=) Prelude.<$> height,
            ("fadeIn" Data..=) Prelude.<$> fadeIn,
            ("imageY" Data..=) Prelude.<$> imageY,
            Prelude.Just ("image" Data..= image)
          ]
      )
