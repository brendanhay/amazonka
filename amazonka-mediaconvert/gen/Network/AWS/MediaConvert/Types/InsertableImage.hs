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
-- Module      : Network.AWS.MediaConvert.Types.InsertableImage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.InsertableImage where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Settings that specify how your still graphic overlay appears.
--
-- /See:/ 'newInsertableImage' smart constructor.
data InsertableImage = InsertableImage'
  { -- | Specify the height of the inserted image in pixels. If you specify a
    -- value that\'s larger than the video resolution height, the service will
    -- crop your overlaid image to fit. To use the native height of the image,
    -- keep this setting blank.
    height :: Core.Maybe Core.Natural,
    -- | Specify the distance, in pixels, between the inserted image and the left
    -- edge of the video frame. Required for any image overlay that you
    -- specify.
    imageX :: Core.Maybe Core.Natural,
    -- | Specify the distance, in pixels, between the overlaid image and the top
    -- edge of the video frame. Required for any image overlay that you
    -- specify.
    imageY :: Core.Maybe Core.Natural,
    -- | Specify the time, in milliseconds, for the image to remain on the output
    -- video. This duration includes fade-in time but not fade-out time.
    duration :: Core.Maybe Core.Natural,
    -- | Specify the width of the inserted image in pixels. If you specify a
    -- value that\'s larger than the video resolution width, the service will
    -- crop your overlaid image to fit. To use the native width of the image,
    -- keep this setting blank.
    width :: Core.Maybe Core.Natural,
    -- | Specify how overlapping inserted images appear. Images with higher
    -- values for Layer appear on top of images with lower values for Layer.
    layer :: Core.Maybe Core.Natural,
    -- | Specify the timecode of the frame that you want the overlay to first
    -- appear on. This must be in timecode (HH:MM:SS:FF or HH:MM:SS;FF) format.
    -- Remember to take into account your timecode source settings.
    startTime :: Core.Maybe Core.Text,
    -- | Use Opacity (Opacity) to specify how much of the underlying video shows
    -- through the inserted image. 0 is transparent and 100 is fully opaque.
    -- Default is 50.
    opacity :: Core.Maybe Core.Natural,
    -- | Specify the length of time, in milliseconds, between the Start time that
    -- you specify for the image insertion and the time that the image appears
    -- at full opacity. Full opacity is the level that you specify for the
    -- opacity setting. If you don\'t specify a value for Fade-in, the image
    -- will appear abruptly at the overlay start time.
    fadeIn :: Core.Maybe Core.Natural,
    -- | Specify the HTTP, HTTPS, or Amazon S3 location of the image that you
    -- want to overlay on the video. Use a PNG or TGA file.
    imageInserterInput :: Core.Maybe Core.Text,
    -- | Specify the length of time, in milliseconds, between the end of the time
    -- that you have specified for the image overlay Duration and when the
    -- overlaid image has faded to total transparency. If you don\'t specify a
    -- value for Fade-out, the image will disappear abruptly at the end of the
    -- inserted image duration.
    fadeOut :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InsertableImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'height', 'insertableImage_height' - Specify the height of the inserted image in pixels. If you specify a
-- value that\'s larger than the video resolution height, the service will
-- crop your overlaid image to fit. To use the native height of the image,
-- keep this setting blank.
--
-- 'imageX', 'insertableImage_imageX' - Specify the distance, in pixels, between the inserted image and the left
-- edge of the video frame. Required for any image overlay that you
-- specify.
--
-- 'imageY', 'insertableImage_imageY' - Specify the distance, in pixels, between the overlaid image and the top
-- edge of the video frame. Required for any image overlay that you
-- specify.
--
-- 'duration', 'insertableImage_duration' - Specify the time, in milliseconds, for the image to remain on the output
-- video. This duration includes fade-in time but not fade-out time.
--
-- 'width', 'insertableImage_width' - Specify the width of the inserted image in pixels. If you specify a
-- value that\'s larger than the video resolution width, the service will
-- crop your overlaid image to fit. To use the native width of the image,
-- keep this setting blank.
--
-- 'layer', 'insertableImage_layer' - Specify how overlapping inserted images appear. Images with higher
-- values for Layer appear on top of images with lower values for Layer.
--
-- 'startTime', 'insertableImage_startTime' - Specify the timecode of the frame that you want the overlay to first
-- appear on. This must be in timecode (HH:MM:SS:FF or HH:MM:SS;FF) format.
-- Remember to take into account your timecode source settings.
--
-- 'opacity', 'insertableImage_opacity' - Use Opacity (Opacity) to specify how much of the underlying video shows
-- through the inserted image. 0 is transparent and 100 is fully opaque.
-- Default is 50.
--
-- 'fadeIn', 'insertableImage_fadeIn' - Specify the length of time, in milliseconds, between the Start time that
-- you specify for the image insertion and the time that the image appears
-- at full opacity. Full opacity is the level that you specify for the
-- opacity setting. If you don\'t specify a value for Fade-in, the image
-- will appear abruptly at the overlay start time.
--
-- 'imageInserterInput', 'insertableImage_imageInserterInput' - Specify the HTTP, HTTPS, or Amazon S3 location of the image that you
-- want to overlay on the video. Use a PNG or TGA file.
--
-- 'fadeOut', 'insertableImage_fadeOut' - Specify the length of time, in milliseconds, between the end of the time
-- that you have specified for the image overlay Duration and when the
-- overlaid image has faded to total transparency. If you don\'t specify a
-- value for Fade-out, the image will disappear abruptly at the end of the
-- inserted image duration.
newInsertableImage ::
  InsertableImage
newInsertableImage =
  InsertableImage'
    { height = Core.Nothing,
      imageX = Core.Nothing,
      imageY = Core.Nothing,
      duration = Core.Nothing,
      width = Core.Nothing,
      layer = Core.Nothing,
      startTime = Core.Nothing,
      opacity = Core.Nothing,
      fadeIn = Core.Nothing,
      imageInserterInput = Core.Nothing,
      fadeOut = Core.Nothing
    }

-- | Specify the height of the inserted image in pixels. If you specify a
-- value that\'s larger than the video resolution height, the service will
-- crop your overlaid image to fit. To use the native height of the image,
-- keep this setting blank.
insertableImage_height :: Lens.Lens' InsertableImage (Core.Maybe Core.Natural)
insertableImage_height = Lens.lens (\InsertableImage' {height} -> height) (\s@InsertableImage' {} a -> s {height = a} :: InsertableImage)

-- | Specify the distance, in pixels, between the inserted image and the left
-- edge of the video frame. Required for any image overlay that you
-- specify.
insertableImage_imageX :: Lens.Lens' InsertableImage (Core.Maybe Core.Natural)
insertableImage_imageX = Lens.lens (\InsertableImage' {imageX} -> imageX) (\s@InsertableImage' {} a -> s {imageX = a} :: InsertableImage)

-- | Specify the distance, in pixels, between the overlaid image and the top
-- edge of the video frame. Required for any image overlay that you
-- specify.
insertableImage_imageY :: Lens.Lens' InsertableImage (Core.Maybe Core.Natural)
insertableImage_imageY = Lens.lens (\InsertableImage' {imageY} -> imageY) (\s@InsertableImage' {} a -> s {imageY = a} :: InsertableImage)

-- | Specify the time, in milliseconds, for the image to remain on the output
-- video. This duration includes fade-in time but not fade-out time.
insertableImage_duration :: Lens.Lens' InsertableImage (Core.Maybe Core.Natural)
insertableImage_duration = Lens.lens (\InsertableImage' {duration} -> duration) (\s@InsertableImage' {} a -> s {duration = a} :: InsertableImage)

-- | Specify the width of the inserted image in pixels. If you specify a
-- value that\'s larger than the video resolution width, the service will
-- crop your overlaid image to fit. To use the native width of the image,
-- keep this setting blank.
insertableImage_width :: Lens.Lens' InsertableImage (Core.Maybe Core.Natural)
insertableImage_width = Lens.lens (\InsertableImage' {width} -> width) (\s@InsertableImage' {} a -> s {width = a} :: InsertableImage)

-- | Specify how overlapping inserted images appear. Images with higher
-- values for Layer appear on top of images with lower values for Layer.
insertableImage_layer :: Lens.Lens' InsertableImage (Core.Maybe Core.Natural)
insertableImage_layer = Lens.lens (\InsertableImage' {layer} -> layer) (\s@InsertableImage' {} a -> s {layer = a} :: InsertableImage)

-- | Specify the timecode of the frame that you want the overlay to first
-- appear on. This must be in timecode (HH:MM:SS:FF or HH:MM:SS;FF) format.
-- Remember to take into account your timecode source settings.
insertableImage_startTime :: Lens.Lens' InsertableImage (Core.Maybe Core.Text)
insertableImage_startTime = Lens.lens (\InsertableImage' {startTime} -> startTime) (\s@InsertableImage' {} a -> s {startTime = a} :: InsertableImage)

-- | Use Opacity (Opacity) to specify how much of the underlying video shows
-- through the inserted image. 0 is transparent and 100 is fully opaque.
-- Default is 50.
insertableImage_opacity :: Lens.Lens' InsertableImage (Core.Maybe Core.Natural)
insertableImage_opacity = Lens.lens (\InsertableImage' {opacity} -> opacity) (\s@InsertableImage' {} a -> s {opacity = a} :: InsertableImage)

-- | Specify the length of time, in milliseconds, between the Start time that
-- you specify for the image insertion and the time that the image appears
-- at full opacity. Full opacity is the level that you specify for the
-- opacity setting. If you don\'t specify a value for Fade-in, the image
-- will appear abruptly at the overlay start time.
insertableImage_fadeIn :: Lens.Lens' InsertableImage (Core.Maybe Core.Natural)
insertableImage_fadeIn = Lens.lens (\InsertableImage' {fadeIn} -> fadeIn) (\s@InsertableImage' {} a -> s {fadeIn = a} :: InsertableImage)

-- | Specify the HTTP, HTTPS, or Amazon S3 location of the image that you
-- want to overlay on the video. Use a PNG or TGA file.
insertableImage_imageInserterInput :: Lens.Lens' InsertableImage (Core.Maybe Core.Text)
insertableImage_imageInserterInput = Lens.lens (\InsertableImage' {imageInserterInput} -> imageInserterInput) (\s@InsertableImage' {} a -> s {imageInserterInput = a} :: InsertableImage)

-- | Specify the length of time, in milliseconds, between the end of the time
-- that you have specified for the image overlay Duration and when the
-- overlaid image has faded to total transparency. If you don\'t specify a
-- value for Fade-out, the image will disappear abruptly at the end of the
-- inserted image duration.
insertableImage_fadeOut :: Lens.Lens' InsertableImage (Core.Maybe Core.Natural)
insertableImage_fadeOut = Lens.lens (\InsertableImage' {fadeOut} -> fadeOut) (\s@InsertableImage' {} a -> s {fadeOut = a} :: InsertableImage)

instance Core.FromJSON InsertableImage where
  parseJSON =
    Core.withObject
      "InsertableImage"
      ( \x ->
          InsertableImage'
            Core.<$> (x Core..:? "height")
            Core.<*> (x Core..:? "imageX")
            Core.<*> (x Core..:? "imageY")
            Core.<*> (x Core..:? "duration")
            Core.<*> (x Core..:? "width")
            Core.<*> (x Core..:? "layer")
            Core.<*> (x Core..:? "startTime")
            Core.<*> (x Core..:? "opacity")
            Core.<*> (x Core..:? "fadeIn")
            Core.<*> (x Core..:? "imageInserterInput")
            Core.<*> (x Core..:? "fadeOut")
      )

instance Core.Hashable InsertableImage

instance Core.NFData InsertableImage

instance Core.ToJSON InsertableImage where
  toJSON InsertableImage' {..} =
    Core.object
      ( Core.catMaybes
          [ ("height" Core..=) Core.<$> height,
            ("imageX" Core..=) Core.<$> imageX,
            ("imageY" Core..=) Core.<$> imageY,
            ("duration" Core..=) Core.<$> duration,
            ("width" Core..=) Core.<$> width,
            ("layer" Core..=) Core.<$> layer,
            ("startTime" Core..=) Core.<$> startTime,
            ("opacity" Core..=) Core.<$> opacity,
            ("fadeIn" Core..=) Core.<$> fadeIn,
            ("imageInserterInput" Core..=)
              Core.<$> imageInserterInput,
            ("fadeOut" Core..=) Core.<$> fadeOut
          ]
      )
