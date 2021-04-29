{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ElasticTranscoder.Types.PresetWatermark
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.PresetWatermark where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Settings for the size, location, and opacity of graphics that you want
-- Elastic Transcoder to overlay over videos that are transcoded using this
-- preset. You can specify settings for up to four watermarks. Watermarks
-- appear in the specified size and location, and with the specified
-- opacity for the duration of the transcoded video.
--
-- Watermarks can be in .png or .jpg format. If you want to display a
-- watermark that is not rectangular, use the .png format, which supports
-- transparency.
--
-- When you create a job that uses this preset, you specify the .png or
-- .jpg graphics that you want Elastic Transcoder to include in the
-- transcoded videos. You can specify fewer graphics in the job than you
-- specify watermark settings in the preset, which allows you to use the
-- same preset for up to four watermarks that have different dimensions.
--
-- /See:/ 'newPresetWatermark' smart constructor.
data PresetWatermark = PresetWatermark'
  { -- | The horizontal position of the watermark unless you specify a non-zero
    -- value for @HorizontalOffset@:
    --
    -- -   __Left__: The left edge of the watermark is aligned with the left
    --     border of the video.
    --
    -- -   __Right__: The right edge of the watermark is aligned with the right
    --     border of the video.
    --
    -- -   __Center__: The watermark is centered between the left and right
    --     borders.
    horizontalAlign :: Prelude.Maybe Prelude.Text,
    -- | The amount by which you want the horizontal position of the watermark to
    -- be offset from the position specified by HorizontalAlign:
    --
    -- -   number of pixels (px): The minimum value is 0 pixels, and the
    --     maximum value is the value of MaxWidth.
    --
    -- -   integer percentage (%): The range of valid values is 0 to 100.
    --
    -- For example, if you specify Left for @HorizontalAlign@ and 5px for
    -- @HorizontalOffset@, the left side of the watermark appears 5 pixels from
    -- the left border of the output video.
    --
    -- @HorizontalOffset@ is only valid when the value of @HorizontalAlign@ is
    -- @Left@ or @Right@. If you specify an offset that causes the watermark to
    -- extend beyond the left or right border and Elastic Transcoder has not
    -- added black bars, the watermark is cropped. If Elastic Transcoder has
    -- added black bars, the watermark extends into the black bars. If the
    -- watermark extends beyond the black bars, it is cropped.
    --
    -- Use the value of @Target@ to specify whether you want to include the
    -- black bars that are added by Elastic Transcoder, if any, in the offset
    -- calculation.
    horizontalOffset :: Prelude.Maybe Prelude.Text,
    -- | A value that controls scaling of the watermark:
    --
    -- -   __Fit__: Elastic Transcoder scales the watermark so it matches the
    --     value that you specified in either @MaxWidth@ or @MaxHeight@ without
    --     exceeding the other value.
    --
    -- -   __Stretch__: Elastic Transcoder stretches the watermark to match the
    --     values that you specified for @MaxWidth@ and @MaxHeight@. If the
    --     relative proportions of the watermark and the values of @MaxWidth@
    --     and @MaxHeight@ are different, the watermark will be distorted.
    --
    -- -   __ShrinkToFit__: Elastic Transcoder scales the watermark down so
    --     that its dimensions match the values that you specified for at least
    --     one of @MaxWidth@ and @MaxHeight@ without exceeding either value. If
    --     you specify this option, Elastic Transcoder does not scale the
    --     watermark up.
    sizingPolicy :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the settings for one watermark. The value of
    -- @Id@ can be up to 40 characters long.
    id :: Prelude.Maybe Prelude.Text,
    -- | @VerticalOffset@
    --
    -- The amount by which you want the vertical position of the watermark to
    -- be offset from the position specified by VerticalAlign:
    --
    -- -   number of pixels (px): The minimum value is 0 pixels, and the
    --     maximum value is the value of @MaxHeight@.
    --
    -- -   integer percentage (%): The range of valid values is 0 to 100.
    --
    -- For example, if you specify @Top@ for @VerticalAlign@ and @5px@ for
    -- @VerticalOffset@, the top of the watermark appears 5 pixels from the top
    -- border of the output video.
    --
    -- @VerticalOffset@ is only valid when the value of VerticalAlign is Top or
    -- Bottom.
    --
    -- If you specify an offset that causes the watermark to extend beyond the
    -- top or bottom border and Elastic Transcoder has not added black bars,
    -- the watermark is cropped. If Elastic Transcoder has added black bars,
    -- the watermark extends into the black bars. If the watermark extends
    -- beyond the black bars, it is cropped.
    --
    -- Use the value of @Target@ to specify whether you want Elastic Transcoder
    -- to include the black bars that are added by Elastic Transcoder, if any,
    -- in the offset calculation.
    verticalOffset :: Prelude.Maybe Prelude.Text,
    -- | The vertical position of the watermark unless you specify a non-zero
    -- value for @VerticalOffset@:
    --
    -- -   __Top__: The top edge of the watermark is aligned with the top
    --     border of the video.
    --
    -- -   __Bottom__: The bottom edge of the watermark is aligned with the
    --     bottom border of the video.
    --
    -- -   __Center__: The watermark is centered between the top and bottom
    --     borders.
    verticalAlign :: Prelude.Maybe Prelude.Text,
    -- | A percentage that indicates how much you want a watermark to obscure the
    -- video in the location where it appears. Valid values are 0 (the
    -- watermark is invisible) to 100 (the watermark completely obscures the
    -- video in the specified location). The datatype of @Opacity@ is float.
    --
    -- Elastic Transcoder supports transparent .png graphics. If you use a
    -- transparent .png, the transparent portion of the video appears as if you
    -- had specified a value of 0 for @Opacity@. The .jpg file format doesn\'t
    -- support transparency.
    opacity :: Prelude.Maybe Prelude.Text,
    -- | A value that determines how Elastic Transcoder interprets values that
    -- you specified for @HorizontalOffset@, @VerticalOffset@, @MaxWidth@, and
    -- @MaxHeight@:
    --
    -- -   __Content__: @HorizontalOffset@ and @VerticalOffset@ values are
    --     calculated based on the borders of the video excluding black bars
    --     added by Elastic Transcoder, if any. In addition, @MaxWidth@ and
    --     @MaxHeight@, if specified as a percentage, are calculated based on
    --     the borders of the video excluding black bars added by Elastic
    --     Transcoder, if any.
    --
    -- -   __Frame__: @HorizontalOffset@ and @VerticalOffset@ values are
    --     calculated based on the borders of the video including black bars
    --     added by Elastic Transcoder, if any. In addition, @MaxWidth@ and
    --     @MaxHeight@, if specified as a percentage, are calculated based on
    --     the borders of the video including black bars added by Elastic
    --     Transcoder, if any.
    target :: Prelude.Maybe Prelude.Text,
    -- | The maximum height of the watermark in one of the following formats:
    --
    -- -   number of pixels (px): The minimum value is 16 pixels, and the
    --     maximum value is the value of @MaxHeight@.
    --
    -- -   integer percentage (%): The range of valid values is 0 to 100. Use
    --     the value of @Target@ to specify whether you want Elastic Transcoder
    --     to include the black bars that are added by Elastic Transcoder, if
    --     any, in the calculation.
    --
    -- If you specify the value in pixels, it must be less than or equal to the
    -- value of @MaxHeight@.
    maxHeight :: Prelude.Maybe Prelude.Text,
    -- | The maximum width of the watermark in one of the following formats:
    --
    -- -   number of pixels (px): The minimum value is 16 pixels, and the
    --     maximum value is the value of @MaxWidth@.
    --
    -- -   integer percentage (%): The range of valid values is 0 to 100. Use
    --     the value of @Target@ to specify whether you want Elastic Transcoder
    --     to include the black bars that are added by Elastic Transcoder, if
    --     any, in the calculation.
    --
    --     If you specify the value in pixels, it must be less than or equal to
    --     the value of @MaxWidth@.
    maxWidth :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PresetWatermark' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'horizontalAlign', 'presetWatermark_horizontalAlign' - The horizontal position of the watermark unless you specify a non-zero
-- value for @HorizontalOffset@:
--
-- -   __Left__: The left edge of the watermark is aligned with the left
--     border of the video.
--
-- -   __Right__: The right edge of the watermark is aligned with the right
--     border of the video.
--
-- -   __Center__: The watermark is centered between the left and right
--     borders.
--
-- 'horizontalOffset', 'presetWatermark_horizontalOffset' - The amount by which you want the horizontal position of the watermark to
-- be offset from the position specified by HorizontalAlign:
--
-- -   number of pixels (px): The minimum value is 0 pixels, and the
--     maximum value is the value of MaxWidth.
--
-- -   integer percentage (%): The range of valid values is 0 to 100.
--
-- For example, if you specify Left for @HorizontalAlign@ and 5px for
-- @HorizontalOffset@, the left side of the watermark appears 5 pixels from
-- the left border of the output video.
--
-- @HorizontalOffset@ is only valid when the value of @HorizontalAlign@ is
-- @Left@ or @Right@. If you specify an offset that causes the watermark to
-- extend beyond the left or right border and Elastic Transcoder has not
-- added black bars, the watermark is cropped. If Elastic Transcoder has
-- added black bars, the watermark extends into the black bars. If the
-- watermark extends beyond the black bars, it is cropped.
--
-- Use the value of @Target@ to specify whether you want to include the
-- black bars that are added by Elastic Transcoder, if any, in the offset
-- calculation.
--
-- 'sizingPolicy', 'presetWatermark_sizingPolicy' - A value that controls scaling of the watermark:
--
-- -   __Fit__: Elastic Transcoder scales the watermark so it matches the
--     value that you specified in either @MaxWidth@ or @MaxHeight@ without
--     exceeding the other value.
--
-- -   __Stretch__: Elastic Transcoder stretches the watermark to match the
--     values that you specified for @MaxWidth@ and @MaxHeight@. If the
--     relative proportions of the watermark and the values of @MaxWidth@
--     and @MaxHeight@ are different, the watermark will be distorted.
--
-- -   __ShrinkToFit__: Elastic Transcoder scales the watermark down so
--     that its dimensions match the values that you specified for at least
--     one of @MaxWidth@ and @MaxHeight@ without exceeding either value. If
--     you specify this option, Elastic Transcoder does not scale the
--     watermark up.
--
-- 'id', 'presetWatermark_id' - A unique identifier for the settings for one watermark. The value of
-- @Id@ can be up to 40 characters long.
--
-- 'verticalOffset', 'presetWatermark_verticalOffset' - @VerticalOffset@
--
-- The amount by which you want the vertical position of the watermark to
-- be offset from the position specified by VerticalAlign:
--
-- -   number of pixels (px): The minimum value is 0 pixels, and the
--     maximum value is the value of @MaxHeight@.
--
-- -   integer percentage (%): The range of valid values is 0 to 100.
--
-- For example, if you specify @Top@ for @VerticalAlign@ and @5px@ for
-- @VerticalOffset@, the top of the watermark appears 5 pixels from the top
-- border of the output video.
--
-- @VerticalOffset@ is only valid when the value of VerticalAlign is Top or
-- Bottom.
--
-- If you specify an offset that causes the watermark to extend beyond the
-- top or bottom border and Elastic Transcoder has not added black bars,
-- the watermark is cropped. If Elastic Transcoder has added black bars,
-- the watermark extends into the black bars. If the watermark extends
-- beyond the black bars, it is cropped.
--
-- Use the value of @Target@ to specify whether you want Elastic Transcoder
-- to include the black bars that are added by Elastic Transcoder, if any,
-- in the offset calculation.
--
-- 'verticalAlign', 'presetWatermark_verticalAlign' - The vertical position of the watermark unless you specify a non-zero
-- value for @VerticalOffset@:
--
-- -   __Top__: The top edge of the watermark is aligned with the top
--     border of the video.
--
-- -   __Bottom__: The bottom edge of the watermark is aligned with the
--     bottom border of the video.
--
-- -   __Center__: The watermark is centered between the top and bottom
--     borders.
--
-- 'opacity', 'presetWatermark_opacity' - A percentage that indicates how much you want a watermark to obscure the
-- video in the location where it appears. Valid values are 0 (the
-- watermark is invisible) to 100 (the watermark completely obscures the
-- video in the specified location). The datatype of @Opacity@ is float.
--
-- Elastic Transcoder supports transparent .png graphics. If you use a
-- transparent .png, the transparent portion of the video appears as if you
-- had specified a value of 0 for @Opacity@. The .jpg file format doesn\'t
-- support transparency.
--
-- 'target', 'presetWatermark_target' - A value that determines how Elastic Transcoder interprets values that
-- you specified for @HorizontalOffset@, @VerticalOffset@, @MaxWidth@, and
-- @MaxHeight@:
--
-- -   __Content__: @HorizontalOffset@ and @VerticalOffset@ values are
--     calculated based on the borders of the video excluding black bars
--     added by Elastic Transcoder, if any. In addition, @MaxWidth@ and
--     @MaxHeight@, if specified as a percentage, are calculated based on
--     the borders of the video excluding black bars added by Elastic
--     Transcoder, if any.
--
-- -   __Frame__: @HorizontalOffset@ and @VerticalOffset@ values are
--     calculated based on the borders of the video including black bars
--     added by Elastic Transcoder, if any. In addition, @MaxWidth@ and
--     @MaxHeight@, if specified as a percentage, are calculated based on
--     the borders of the video including black bars added by Elastic
--     Transcoder, if any.
--
-- 'maxHeight', 'presetWatermark_maxHeight' - The maximum height of the watermark in one of the following formats:
--
-- -   number of pixels (px): The minimum value is 16 pixels, and the
--     maximum value is the value of @MaxHeight@.
--
-- -   integer percentage (%): The range of valid values is 0 to 100. Use
--     the value of @Target@ to specify whether you want Elastic Transcoder
--     to include the black bars that are added by Elastic Transcoder, if
--     any, in the calculation.
--
-- If you specify the value in pixels, it must be less than or equal to the
-- value of @MaxHeight@.
--
-- 'maxWidth', 'presetWatermark_maxWidth' - The maximum width of the watermark in one of the following formats:
--
-- -   number of pixels (px): The minimum value is 16 pixels, and the
--     maximum value is the value of @MaxWidth@.
--
-- -   integer percentage (%): The range of valid values is 0 to 100. Use
--     the value of @Target@ to specify whether you want Elastic Transcoder
--     to include the black bars that are added by Elastic Transcoder, if
--     any, in the calculation.
--
--     If you specify the value in pixels, it must be less than or equal to
--     the value of @MaxWidth@.
newPresetWatermark ::
  PresetWatermark
newPresetWatermark =
  PresetWatermark'
    { horizontalAlign = Prelude.Nothing,
      horizontalOffset = Prelude.Nothing,
      sizingPolicy = Prelude.Nothing,
      id = Prelude.Nothing,
      verticalOffset = Prelude.Nothing,
      verticalAlign = Prelude.Nothing,
      opacity = Prelude.Nothing,
      target = Prelude.Nothing,
      maxHeight = Prelude.Nothing,
      maxWidth = Prelude.Nothing
    }

-- | The horizontal position of the watermark unless you specify a non-zero
-- value for @HorizontalOffset@:
--
-- -   __Left__: The left edge of the watermark is aligned with the left
--     border of the video.
--
-- -   __Right__: The right edge of the watermark is aligned with the right
--     border of the video.
--
-- -   __Center__: The watermark is centered between the left and right
--     borders.
presetWatermark_horizontalAlign :: Lens.Lens' PresetWatermark (Prelude.Maybe Prelude.Text)
presetWatermark_horizontalAlign = Lens.lens (\PresetWatermark' {horizontalAlign} -> horizontalAlign) (\s@PresetWatermark' {} a -> s {horizontalAlign = a} :: PresetWatermark)

-- | The amount by which you want the horizontal position of the watermark to
-- be offset from the position specified by HorizontalAlign:
--
-- -   number of pixels (px): The minimum value is 0 pixels, and the
--     maximum value is the value of MaxWidth.
--
-- -   integer percentage (%): The range of valid values is 0 to 100.
--
-- For example, if you specify Left for @HorizontalAlign@ and 5px for
-- @HorizontalOffset@, the left side of the watermark appears 5 pixels from
-- the left border of the output video.
--
-- @HorizontalOffset@ is only valid when the value of @HorizontalAlign@ is
-- @Left@ or @Right@. If you specify an offset that causes the watermark to
-- extend beyond the left or right border and Elastic Transcoder has not
-- added black bars, the watermark is cropped. If Elastic Transcoder has
-- added black bars, the watermark extends into the black bars. If the
-- watermark extends beyond the black bars, it is cropped.
--
-- Use the value of @Target@ to specify whether you want to include the
-- black bars that are added by Elastic Transcoder, if any, in the offset
-- calculation.
presetWatermark_horizontalOffset :: Lens.Lens' PresetWatermark (Prelude.Maybe Prelude.Text)
presetWatermark_horizontalOffset = Lens.lens (\PresetWatermark' {horizontalOffset} -> horizontalOffset) (\s@PresetWatermark' {} a -> s {horizontalOffset = a} :: PresetWatermark)

-- | A value that controls scaling of the watermark:
--
-- -   __Fit__: Elastic Transcoder scales the watermark so it matches the
--     value that you specified in either @MaxWidth@ or @MaxHeight@ without
--     exceeding the other value.
--
-- -   __Stretch__: Elastic Transcoder stretches the watermark to match the
--     values that you specified for @MaxWidth@ and @MaxHeight@. If the
--     relative proportions of the watermark and the values of @MaxWidth@
--     and @MaxHeight@ are different, the watermark will be distorted.
--
-- -   __ShrinkToFit__: Elastic Transcoder scales the watermark down so
--     that its dimensions match the values that you specified for at least
--     one of @MaxWidth@ and @MaxHeight@ without exceeding either value. If
--     you specify this option, Elastic Transcoder does not scale the
--     watermark up.
presetWatermark_sizingPolicy :: Lens.Lens' PresetWatermark (Prelude.Maybe Prelude.Text)
presetWatermark_sizingPolicy = Lens.lens (\PresetWatermark' {sizingPolicy} -> sizingPolicy) (\s@PresetWatermark' {} a -> s {sizingPolicy = a} :: PresetWatermark)

-- | A unique identifier for the settings for one watermark. The value of
-- @Id@ can be up to 40 characters long.
presetWatermark_id :: Lens.Lens' PresetWatermark (Prelude.Maybe Prelude.Text)
presetWatermark_id = Lens.lens (\PresetWatermark' {id} -> id) (\s@PresetWatermark' {} a -> s {id = a} :: PresetWatermark)

-- | @VerticalOffset@
--
-- The amount by which you want the vertical position of the watermark to
-- be offset from the position specified by VerticalAlign:
--
-- -   number of pixels (px): The minimum value is 0 pixels, and the
--     maximum value is the value of @MaxHeight@.
--
-- -   integer percentage (%): The range of valid values is 0 to 100.
--
-- For example, if you specify @Top@ for @VerticalAlign@ and @5px@ for
-- @VerticalOffset@, the top of the watermark appears 5 pixels from the top
-- border of the output video.
--
-- @VerticalOffset@ is only valid when the value of VerticalAlign is Top or
-- Bottom.
--
-- If you specify an offset that causes the watermark to extend beyond the
-- top or bottom border and Elastic Transcoder has not added black bars,
-- the watermark is cropped. If Elastic Transcoder has added black bars,
-- the watermark extends into the black bars. If the watermark extends
-- beyond the black bars, it is cropped.
--
-- Use the value of @Target@ to specify whether you want Elastic Transcoder
-- to include the black bars that are added by Elastic Transcoder, if any,
-- in the offset calculation.
presetWatermark_verticalOffset :: Lens.Lens' PresetWatermark (Prelude.Maybe Prelude.Text)
presetWatermark_verticalOffset = Lens.lens (\PresetWatermark' {verticalOffset} -> verticalOffset) (\s@PresetWatermark' {} a -> s {verticalOffset = a} :: PresetWatermark)

-- | The vertical position of the watermark unless you specify a non-zero
-- value for @VerticalOffset@:
--
-- -   __Top__: The top edge of the watermark is aligned with the top
--     border of the video.
--
-- -   __Bottom__: The bottom edge of the watermark is aligned with the
--     bottom border of the video.
--
-- -   __Center__: The watermark is centered between the top and bottom
--     borders.
presetWatermark_verticalAlign :: Lens.Lens' PresetWatermark (Prelude.Maybe Prelude.Text)
presetWatermark_verticalAlign = Lens.lens (\PresetWatermark' {verticalAlign} -> verticalAlign) (\s@PresetWatermark' {} a -> s {verticalAlign = a} :: PresetWatermark)

-- | A percentage that indicates how much you want a watermark to obscure the
-- video in the location where it appears. Valid values are 0 (the
-- watermark is invisible) to 100 (the watermark completely obscures the
-- video in the specified location). The datatype of @Opacity@ is float.
--
-- Elastic Transcoder supports transparent .png graphics. If you use a
-- transparent .png, the transparent portion of the video appears as if you
-- had specified a value of 0 for @Opacity@. The .jpg file format doesn\'t
-- support transparency.
presetWatermark_opacity :: Lens.Lens' PresetWatermark (Prelude.Maybe Prelude.Text)
presetWatermark_opacity = Lens.lens (\PresetWatermark' {opacity} -> opacity) (\s@PresetWatermark' {} a -> s {opacity = a} :: PresetWatermark)

-- | A value that determines how Elastic Transcoder interprets values that
-- you specified for @HorizontalOffset@, @VerticalOffset@, @MaxWidth@, and
-- @MaxHeight@:
--
-- -   __Content__: @HorizontalOffset@ and @VerticalOffset@ values are
--     calculated based on the borders of the video excluding black bars
--     added by Elastic Transcoder, if any. In addition, @MaxWidth@ and
--     @MaxHeight@, if specified as a percentage, are calculated based on
--     the borders of the video excluding black bars added by Elastic
--     Transcoder, if any.
--
-- -   __Frame__: @HorizontalOffset@ and @VerticalOffset@ values are
--     calculated based on the borders of the video including black bars
--     added by Elastic Transcoder, if any. In addition, @MaxWidth@ and
--     @MaxHeight@, if specified as a percentage, are calculated based on
--     the borders of the video including black bars added by Elastic
--     Transcoder, if any.
presetWatermark_target :: Lens.Lens' PresetWatermark (Prelude.Maybe Prelude.Text)
presetWatermark_target = Lens.lens (\PresetWatermark' {target} -> target) (\s@PresetWatermark' {} a -> s {target = a} :: PresetWatermark)

-- | The maximum height of the watermark in one of the following formats:
--
-- -   number of pixels (px): The minimum value is 16 pixels, and the
--     maximum value is the value of @MaxHeight@.
--
-- -   integer percentage (%): The range of valid values is 0 to 100. Use
--     the value of @Target@ to specify whether you want Elastic Transcoder
--     to include the black bars that are added by Elastic Transcoder, if
--     any, in the calculation.
--
-- If you specify the value in pixels, it must be less than or equal to the
-- value of @MaxHeight@.
presetWatermark_maxHeight :: Lens.Lens' PresetWatermark (Prelude.Maybe Prelude.Text)
presetWatermark_maxHeight = Lens.lens (\PresetWatermark' {maxHeight} -> maxHeight) (\s@PresetWatermark' {} a -> s {maxHeight = a} :: PresetWatermark)

-- | The maximum width of the watermark in one of the following formats:
--
-- -   number of pixels (px): The minimum value is 16 pixels, and the
--     maximum value is the value of @MaxWidth@.
--
-- -   integer percentage (%): The range of valid values is 0 to 100. Use
--     the value of @Target@ to specify whether you want Elastic Transcoder
--     to include the black bars that are added by Elastic Transcoder, if
--     any, in the calculation.
--
--     If you specify the value in pixels, it must be less than or equal to
--     the value of @MaxWidth@.
presetWatermark_maxWidth :: Lens.Lens' PresetWatermark (Prelude.Maybe Prelude.Text)
presetWatermark_maxWidth = Lens.lens (\PresetWatermark' {maxWidth} -> maxWidth) (\s@PresetWatermark' {} a -> s {maxWidth = a} :: PresetWatermark)

instance Prelude.FromJSON PresetWatermark where
  parseJSON =
    Prelude.withObject
      "PresetWatermark"
      ( \x ->
          PresetWatermark'
            Prelude.<$> (x Prelude..:? "HorizontalAlign")
            Prelude.<*> (x Prelude..:? "HorizontalOffset")
            Prelude.<*> (x Prelude..:? "SizingPolicy")
            Prelude.<*> (x Prelude..:? "Id")
            Prelude.<*> (x Prelude..:? "VerticalOffset")
            Prelude.<*> (x Prelude..:? "VerticalAlign")
            Prelude.<*> (x Prelude..:? "Opacity")
            Prelude.<*> (x Prelude..:? "Target")
            Prelude.<*> (x Prelude..:? "MaxHeight")
            Prelude.<*> (x Prelude..:? "MaxWidth")
      )

instance Prelude.Hashable PresetWatermark

instance Prelude.NFData PresetWatermark

instance Prelude.ToJSON PresetWatermark where
  toJSON PresetWatermark' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("HorizontalAlign" Prelude..=)
              Prelude.<$> horizontalAlign,
            ("HorizontalOffset" Prelude..=)
              Prelude.<$> horizontalOffset,
            ("SizingPolicy" Prelude..=) Prelude.<$> sizingPolicy,
            ("Id" Prelude..=) Prelude.<$> id,
            ("VerticalOffset" Prelude..=)
              Prelude.<$> verticalOffset,
            ("VerticalAlign" Prelude..=)
              Prelude.<$> verticalAlign,
            ("Opacity" Prelude..=) Prelude.<$> opacity,
            ("Target" Prelude..=) Prelude.<$> target,
            ("MaxHeight" Prelude..=) Prelude.<$> maxHeight,
            ("MaxWidth" Prelude..=) Prelude.<$> maxWidth
          ]
      )
