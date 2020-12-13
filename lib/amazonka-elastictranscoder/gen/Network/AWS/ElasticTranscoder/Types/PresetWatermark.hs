{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.PresetWatermark
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.PresetWatermark
  ( PresetWatermark (..),

    -- * Smart constructor
    mkPresetWatermark,

    -- * Lenses
    pwVerticalAlign,
    pwSizingPolicy,
    pwHorizontalOffset,
    pwMaxHeight,
    pwOpacity,
    pwVerticalOffset,
    pwMaxWidth,
    pwId,
    pwHorizontalAlign,
    pwTarget,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Settings for the size, location, and opacity of graphics that you want Elastic Transcoder to overlay over videos that are transcoded using this preset. You can specify settings for up to four watermarks. Watermarks appear in the specified size and location, and with the specified opacity for the duration of the transcoded video.
--
-- Watermarks can be in .png or .jpg format. If you want to display a watermark that is not rectangular, use the .png format, which supports transparency.
-- When you create a job that uses this preset, you specify the .png or .jpg graphics that you want Elastic Transcoder to include in the transcoded videos. You can specify fewer graphics in the job than you specify watermark settings in the preset, which allows you to use the same preset for up to four watermarks that have different dimensions.
--
-- /See:/ 'mkPresetWatermark' smart constructor.
data PresetWatermark = PresetWatermark'
  { -- | The vertical position of the watermark unless you specify a non-zero value for @VerticalOffset@ :
    --
    --
    --     * __Top__ : The top edge of the watermark is aligned with the top border of the video.
    --
    --
    --     * __Bottom__ : The bottom edge of the watermark is aligned with the bottom border of the video.
    --
    --
    --     * __Center__ : The watermark is centered between the top and bottom borders.
    verticalAlign :: Lude.Maybe Lude.Text,
    -- | A value that controls scaling of the watermark:
    --
    --
    --     * __Fit__ : Elastic Transcoder scales the watermark so it matches the value that you specified in either @MaxWidth@ or @MaxHeight@ without exceeding the other value.
    --
    --
    --     * __Stretch__ : Elastic Transcoder stretches the watermark to match the values that you specified for @MaxWidth@ and @MaxHeight@ . If the relative proportions of the watermark and the values of @MaxWidth@ and @MaxHeight@ are different, the watermark will be distorted.
    --
    --
    --     * __ShrinkToFit__ : Elastic Transcoder scales the watermark down so that its dimensions match the values that you specified for at least one of @MaxWidth@ and @MaxHeight@ without exceeding either value. If you specify this option, Elastic Transcoder does not scale the watermark up.
    sizingPolicy :: Lude.Maybe Lude.Text,
    -- | The amount by which you want the horizontal position of the watermark to be offset from the position specified by HorizontalAlign:
    --
    --
    --     * number of pixels (px): The minimum value is 0 pixels, and the maximum value is the value of MaxWidth.
    --
    --
    --     * integer percentage (%): The range of valid values is 0 to 100.
    --
    --
    -- For example, if you specify Left for @HorizontalAlign@ and 5px for @HorizontalOffset@ , the left side of the watermark appears 5 pixels from the left border of the output video.
    -- @HorizontalOffset@ is only valid when the value of @HorizontalAlign@ is @Left@ or @Right@ . If you specify an offset that causes the watermark to extend beyond the left or right border and Elastic Transcoder has not added black bars, the watermark is cropped. If Elastic Transcoder has added black bars, the watermark extends into the black bars. If the watermark extends beyond the black bars, it is cropped.
    -- Use the value of @Target@ to specify whether you want to include the black bars that are added by Elastic Transcoder, if any, in the offset calculation.
    horizontalOffset :: Lude.Maybe Lude.Text,
    -- | The maximum height of the watermark in one of the following formats:
    --
    --
    --     * number of pixels (px): The minimum value is 16 pixels, and the maximum value is the value of @MaxHeight@ .
    --
    --
    --     * integer percentage (%): The range of valid values is 0 to 100. Use the value of @Target@ to specify whether you want Elastic Transcoder to include the black bars that are added by Elastic Transcoder, if any, in the calculation.
    --
    --
    -- If you specify the value in pixels, it must be less than or equal to the value of @MaxHeight@ .
    maxHeight :: Lude.Maybe Lude.Text,
    -- | A percentage that indicates how much you want a watermark to obscure the video in the location where it appears. Valid values are 0 (the watermark is invisible) to 100 (the watermark completely obscures the video in the specified location). The datatype of @Opacity@ is float.
    --
    -- Elastic Transcoder supports transparent .png graphics. If you use a transparent .png, the transparent portion of the video appears as if you had specified a value of 0 for @Opacity@ . The .jpg file format doesn't support transparency.
    opacity :: Lude.Maybe Lude.Text,
    -- | @VerticalOffset@
    --
    -- The amount by which you want the vertical position of the watermark to be offset from the position specified by VerticalAlign:
    --
    --     * number of pixels (px): The minimum value is 0 pixels, and the maximum value is the value of @MaxHeight@ .
    --
    --
    --     * integer percentage (%): The range of valid values is 0 to 100.
    --
    --
    -- For example, if you specify @Top@ for @VerticalAlign@ and @5px@ for @VerticalOffset@ , the top of the watermark appears 5 pixels from the top border of the output video.
    -- @VerticalOffset@ is only valid when the value of VerticalAlign is Top or Bottom.
    -- If you specify an offset that causes the watermark to extend beyond the top or bottom border and Elastic Transcoder has not added black bars, the watermark is cropped. If Elastic Transcoder has added black bars, the watermark extends into the black bars. If the watermark extends beyond the black bars, it is cropped.
    -- Use the value of @Target@ to specify whether you want Elastic Transcoder to include the black bars that are added by Elastic Transcoder, if any, in the offset calculation.
    verticalOffset :: Lude.Maybe Lude.Text,
    -- | The maximum width of the watermark in one of the following formats:
    --
    --
    --     * number of pixels (px): The minimum value is 16 pixels, and the maximum value is the value of @MaxWidth@ .
    --
    --
    --     * integer percentage (%): The range of valid values is 0 to 100. Use the value of @Target@ to specify whether you want Elastic Transcoder to include the black bars that are added by Elastic Transcoder, if any, in the calculation.
    -- If you specify the value in pixels, it must be less than or equal to the value of @MaxWidth@ .
    maxWidth :: Lude.Maybe Lude.Text,
    -- | A unique identifier for the settings for one watermark. The value of @Id@ can be up to 40 characters long.
    id :: Lude.Maybe Lude.Text,
    -- | The horizontal position of the watermark unless you specify a non-zero value for @HorizontalOffset@ :
    --
    --
    --     * __Left__ : The left edge of the watermark is aligned with the left border of the video.
    --
    --
    --     * __Right__ : The right edge of the watermark is aligned with the right border of the video.
    --
    --
    --     * __Center__ : The watermark is centered between the left and right borders.
    horizontalAlign :: Lude.Maybe Lude.Text,
    -- | A value that determines how Elastic Transcoder interprets values that you specified for @HorizontalOffset@ , @VerticalOffset@ , @MaxWidth@ , and @MaxHeight@ :
    --
    --
    --     * __Content__ : @HorizontalOffset@ and @VerticalOffset@ values are calculated based on the borders of the video excluding black bars added by Elastic Transcoder, if any. In addition, @MaxWidth@ and @MaxHeight@ , if specified as a percentage, are calculated based on the borders of the video excluding black bars added by Elastic Transcoder, if any.
    --
    --
    --     * __Frame__ : @HorizontalOffset@ and @VerticalOffset@ values are calculated based on the borders of the video including black bars added by Elastic Transcoder, if any. In addition, @MaxWidth@ and @MaxHeight@ , if specified as a percentage, are calculated based on the borders of the video including black bars added by Elastic Transcoder, if any.
    target :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PresetWatermark' with the minimum fields required to make a request.
--
-- * 'verticalAlign' - The vertical position of the watermark unless you specify a non-zero value for @VerticalOffset@ :
--
--
--     * __Top__ : The top edge of the watermark is aligned with the top border of the video.
--
--
--     * __Bottom__ : The bottom edge of the watermark is aligned with the bottom border of the video.
--
--
--     * __Center__ : The watermark is centered between the top and bottom borders.
--
--
-- * 'sizingPolicy' - A value that controls scaling of the watermark:
--
--
--     * __Fit__ : Elastic Transcoder scales the watermark so it matches the value that you specified in either @MaxWidth@ or @MaxHeight@ without exceeding the other value.
--
--
--     * __Stretch__ : Elastic Transcoder stretches the watermark to match the values that you specified for @MaxWidth@ and @MaxHeight@ . If the relative proportions of the watermark and the values of @MaxWidth@ and @MaxHeight@ are different, the watermark will be distorted.
--
--
--     * __ShrinkToFit__ : Elastic Transcoder scales the watermark down so that its dimensions match the values that you specified for at least one of @MaxWidth@ and @MaxHeight@ without exceeding either value. If you specify this option, Elastic Transcoder does not scale the watermark up.
--
--
-- * 'horizontalOffset' - The amount by which you want the horizontal position of the watermark to be offset from the position specified by HorizontalAlign:
--
--
--     * number of pixels (px): The minimum value is 0 pixels, and the maximum value is the value of MaxWidth.
--
--
--     * integer percentage (%): The range of valid values is 0 to 100.
--
--
-- For example, if you specify Left for @HorizontalAlign@ and 5px for @HorizontalOffset@ , the left side of the watermark appears 5 pixels from the left border of the output video.
-- @HorizontalOffset@ is only valid when the value of @HorizontalAlign@ is @Left@ or @Right@ . If you specify an offset that causes the watermark to extend beyond the left or right border and Elastic Transcoder has not added black bars, the watermark is cropped. If Elastic Transcoder has added black bars, the watermark extends into the black bars. If the watermark extends beyond the black bars, it is cropped.
-- Use the value of @Target@ to specify whether you want to include the black bars that are added by Elastic Transcoder, if any, in the offset calculation.
-- * 'maxHeight' - The maximum height of the watermark in one of the following formats:
--
--
--     * number of pixels (px): The minimum value is 16 pixels, and the maximum value is the value of @MaxHeight@ .
--
--
--     * integer percentage (%): The range of valid values is 0 to 100. Use the value of @Target@ to specify whether you want Elastic Transcoder to include the black bars that are added by Elastic Transcoder, if any, in the calculation.
--
--
-- If you specify the value in pixels, it must be less than or equal to the value of @MaxHeight@ .
-- * 'opacity' - A percentage that indicates how much you want a watermark to obscure the video in the location where it appears. Valid values are 0 (the watermark is invisible) to 100 (the watermark completely obscures the video in the specified location). The datatype of @Opacity@ is float.
--
-- Elastic Transcoder supports transparent .png graphics. If you use a transparent .png, the transparent portion of the video appears as if you had specified a value of 0 for @Opacity@ . The .jpg file format doesn't support transparency.
-- * 'verticalOffset' - @VerticalOffset@
--
-- The amount by which you want the vertical position of the watermark to be offset from the position specified by VerticalAlign:
--
--     * number of pixels (px): The minimum value is 0 pixels, and the maximum value is the value of @MaxHeight@ .
--
--
--     * integer percentage (%): The range of valid values is 0 to 100.
--
--
-- For example, if you specify @Top@ for @VerticalAlign@ and @5px@ for @VerticalOffset@ , the top of the watermark appears 5 pixels from the top border of the output video.
-- @VerticalOffset@ is only valid when the value of VerticalAlign is Top or Bottom.
-- If you specify an offset that causes the watermark to extend beyond the top or bottom border and Elastic Transcoder has not added black bars, the watermark is cropped. If Elastic Transcoder has added black bars, the watermark extends into the black bars. If the watermark extends beyond the black bars, it is cropped.
-- Use the value of @Target@ to specify whether you want Elastic Transcoder to include the black bars that are added by Elastic Transcoder, if any, in the offset calculation.
-- * 'maxWidth' - The maximum width of the watermark in one of the following formats:
--
--
--     * number of pixels (px): The minimum value is 16 pixels, and the maximum value is the value of @MaxWidth@ .
--
--
--     * integer percentage (%): The range of valid values is 0 to 100. Use the value of @Target@ to specify whether you want Elastic Transcoder to include the black bars that are added by Elastic Transcoder, if any, in the calculation.
-- If you specify the value in pixels, it must be less than or equal to the value of @MaxWidth@ .
--
--
-- * 'id' - A unique identifier for the settings for one watermark. The value of @Id@ can be up to 40 characters long.
-- * 'horizontalAlign' - The horizontal position of the watermark unless you specify a non-zero value for @HorizontalOffset@ :
--
--
--     * __Left__ : The left edge of the watermark is aligned with the left border of the video.
--
--
--     * __Right__ : The right edge of the watermark is aligned with the right border of the video.
--
--
--     * __Center__ : The watermark is centered between the left and right borders.
--
--
-- * 'target' - A value that determines how Elastic Transcoder interprets values that you specified for @HorizontalOffset@ , @VerticalOffset@ , @MaxWidth@ , and @MaxHeight@ :
--
--
--     * __Content__ : @HorizontalOffset@ and @VerticalOffset@ values are calculated based on the borders of the video excluding black bars added by Elastic Transcoder, if any. In addition, @MaxWidth@ and @MaxHeight@ , if specified as a percentage, are calculated based on the borders of the video excluding black bars added by Elastic Transcoder, if any.
--
--
--     * __Frame__ : @HorizontalOffset@ and @VerticalOffset@ values are calculated based on the borders of the video including black bars added by Elastic Transcoder, if any. In addition, @MaxWidth@ and @MaxHeight@ , if specified as a percentage, are calculated based on the borders of the video including black bars added by Elastic Transcoder, if any.
mkPresetWatermark ::
  PresetWatermark
mkPresetWatermark =
  PresetWatermark'
    { verticalAlign = Lude.Nothing,
      sizingPolicy = Lude.Nothing,
      horizontalOffset = Lude.Nothing,
      maxHeight = Lude.Nothing,
      opacity = Lude.Nothing,
      verticalOffset = Lude.Nothing,
      maxWidth = Lude.Nothing,
      id = Lude.Nothing,
      horizontalAlign = Lude.Nothing,
      target = Lude.Nothing
    }

-- | The vertical position of the watermark unless you specify a non-zero value for @VerticalOffset@ :
--
--
--     * __Top__ : The top edge of the watermark is aligned with the top border of the video.
--
--
--     * __Bottom__ : The bottom edge of the watermark is aligned with the bottom border of the video.
--
--
--     * __Center__ : The watermark is centered between the top and bottom borders.
--
--
--
-- /Note:/ Consider using 'verticalAlign' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pwVerticalAlign :: Lens.Lens' PresetWatermark (Lude.Maybe Lude.Text)
pwVerticalAlign = Lens.lens (verticalAlign :: PresetWatermark -> Lude.Maybe Lude.Text) (\s a -> s {verticalAlign = a} :: PresetWatermark)
{-# DEPRECATED pwVerticalAlign "Use generic-lens or generic-optics with 'verticalAlign' instead." #-}

-- | A value that controls scaling of the watermark:
--
--
--     * __Fit__ : Elastic Transcoder scales the watermark so it matches the value that you specified in either @MaxWidth@ or @MaxHeight@ without exceeding the other value.
--
--
--     * __Stretch__ : Elastic Transcoder stretches the watermark to match the values that you specified for @MaxWidth@ and @MaxHeight@ . If the relative proportions of the watermark and the values of @MaxWidth@ and @MaxHeight@ are different, the watermark will be distorted.
--
--
--     * __ShrinkToFit__ : Elastic Transcoder scales the watermark down so that its dimensions match the values that you specified for at least one of @MaxWidth@ and @MaxHeight@ without exceeding either value. If you specify this option, Elastic Transcoder does not scale the watermark up.
--
--
--
-- /Note:/ Consider using 'sizingPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pwSizingPolicy :: Lens.Lens' PresetWatermark (Lude.Maybe Lude.Text)
pwSizingPolicy = Lens.lens (sizingPolicy :: PresetWatermark -> Lude.Maybe Lude.Text) (\s a -> s {sizingPolicy = a} :: PresetWatermark)
{-# DEPRECATED pwSizingPolicy "Use generic-lens or generic-optics with 'sizingPolicy' instead." #-}

-- | The amount by which you want the horizontal position of the watermark to be offset from the position specified by HorizontalAlign:
--
--
--     * number of pixels (px): The minimum value is 0 pixels, and the maximum value is the value of MaxWidth.
--
--
--     * integer percentage (%): The range of valid values is 0 to 100.
--
--
-- For example, if you specify Left for @HorizontalAlign@ and 5px for @HorizontalOffset@ , the left side of the watermark appears 5 pixels from the left border of the output video.
-- @HorizontalOffset@ is only valid when the value of @HorizontalAlign@ is @Left@ or @Right@ . If you specify an offset that causes the watermark to extend beyond the left or right border and Elastic Transcoder has not added black bars, the watermark is cropped. If Elastic Transcoder has added black bars, the watermark extends into the black bars. If the watermark extends beyond the black bars, it is cropped.
-- Use the value of @Target@ to specify whether you want to include the black bars that are added by Elastic Transcoder, if any, in the offset calculation.
--
-- /Note:/ Consider using 'horizontalOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pwHorizontalOffset :: Lens.Lens' PresetWatermark (Lude.Maybe Lude.Text)
pwHorizontalOffset = Lens.lens (horizontalOffset :: PresetWatermark -> Lude.Maybe Lude.Text) (\s a -> s {horizontalOffset = a} :: PresetWatermark)
{-# DEPRECATED pwHorizontalOffset "Use generic-lens or generic-optics with 'horizontalOffset' instead." #-}

-- | The maximum height of the watermark in one of the following formats:
--
--
--     * number of pixels (px): The minimum value is 16 pixels, and the maximum value is the value of @MaxHeight@ .
--
--
--     * integer percentage (%): The range of valid values is 0 to 100. Use the value of @Target@ to specify whether you want Elastic Transcoder to include the black bars that are added by Elastic Transcoder, if any, in the calculation.
--
--
-- If you specify the value in pixels, it must be less than or equal to the value of @MaxHeight@ .
--
-- /Note:/ Consider using 'maxHeight' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pwMaxHeight :: Lens.Lens' PresetWatermark (Lude.Maybe Lude.Text)
pwMaxHeight = Lens.lens (maxHeight :: PresetWatermark -> Lude.Maybe Lude.Text) (\s a -> s {maxHeight = a} :: PresetWatermark)
{-# DEPRECATED pwMaxHeight "Use generic-lens or generic-optics with 'maxHeight' instead." #-}

-- | A percentage that indicates how much you want a watermark to obscure the video in the location where it appears. Valid values are 0 (the watermark is invisible) to 100 (the watermark completely obscures the video in the specified location). The datatype of @Opacity@ is float.
--
-- Elastic Transcoder supports transparent .png graphics. If you use a transparent .png, the transparent portion of the video appears as if you had specified a value of 0 for @Opacity@ . The .jpg file format doesn't support transparency.
--
-- /Note:/ Consider using 'opacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pwOpacity :: Lens.Lens' PresetWatermark (Lude.Maybe Lude.Text)
pwOpacity = Lens.lens (opacity :: PresetWatermark -> Lude.Maybe Lude.Text) (\s a -> s {opacity = a} :: PresetWatermark)
{-# DEPRECATED pwOpacity "Use generic-lens or generic-optics with 'opacity' instead." #-}

-- | @VerticalOffset@
--
-- The amount by which you want the vertical position of the watermark to be offset from the position specified by VerticalAlign:
--
--     * number of pixels (px): The minimum value is 0 pixels, and the maximum value is the value of @MaxHeight@ .
--
--
--     * integer percentage (%): The range of valid values is 0 to 100.
--
--
-- For example, if you specify @Top@ for @VerticalAlign@ and @5px@ for @VerticalOffset@ , the top of the watermark appears 5 pixels from the top border of the output video.
-- @VerticalOffset@ is only valid when the value of VerticalAlign is Top or Bottom.
-- If you specify an offset that causes the watermark to extend beyond the top or bottom border and Elastic Transcoder has not added black bars, the watermark is cropped. If Elastic Transcoder has added black bars, the watermark extends into the black bars. If the watermark extends beyond the black bars, it is cropped.
-- Use the value of @Target@ to specify whether you want Elastic Transcoder to include the black bars that are added by Elastic Transcoder, if any, in the offset calculation.
--
-- /Note:/ Consider using 'verticalOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pwVerticalOffset :: Lens.Lens' PresetWatermark (Lude.Maybe Lude.Text)
pwVerticalOffset = Lens.lens (verticalOffset :: PresetWatermark -> Lude.Maybe Lude.Text) (\s a -> s {verticalOffset = a} :: PresetWatermark)
{-# DEPRECATED pwVerticalOffset "Use generic-lens or generic-optics with 'verticalOffset' instead." #-}

-- | The maximum width of the watermark in one of the following formats:
--
--
--     * number of pixels (px): The minimum value is 16 pixels, and the maximum value is the value of @MaxWidth@ .
--
--
--     * integer percentage (%): The range of valid values is 0 to 100. Use the value of @Target@ to specify whether you want Elastic Transcoder to include the black bars that are added by Elastic Transcoder, if any, in the calculation.
-- If you specify the value in pixels, it must be less than or equal to the value of @MaxWidth@ .
--
--
--
-- /Note:/ Consider using 'maxWidth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pwMaxWidth :: Lens.Lens' PresetWatermark (Lude.Maybe Lude.Text)
pwMaxWidth = Lens.lens (maxWidth :: PresetWatermark -> Lude.Maybe Lude.Text) (\s a -> s {maxWidth = a} :: PresetWatermark)
{-# DEPRECATED pwMaxWidth "Use generic-lens or generic-optics with 'maxWidth' instead." #-}

-- | A unique identifier for the settings for one watermark. The value of @Id@ can be up to 40 characters long.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pwId :: Lens.Lens' PresetWatermark (Lude.Maybe Lude.Text)
pwId = Lens.lens (id :: PresetWatermark -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: PresetWatermark)
{-# DEPRECATED pwId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The horizontal position of the watermark unless you specify a non-zero value for @HorizontalOffset@ :
--
--
--     * __Left__ : The left edge of the watermark is aligned with the left border of the video.
--
--
--     * __Right__ : The right edge of the watermark is aligned with the right border of the video.
--
--
--     * __Center__ : The watermark is centered between the left and right borders.
--
--
--
-- /Note:/ Consider using 'horizontalAlign' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pwHorizontalAlign :: Lens.Lens' PresetWatermark (Lude.Maybe Lude.Text)
pwHorizontalAlign = Lens.lens (horizontalAlign :: PresetWatermark -> Lude.Maybe Lude.Text) (\s a -> s {horizontalAlign = a} :: PresetWatermark)
{-# DEPRECATED pwHorizontalAlign "Use generic-lens or generic-optics with 'horizontalAlign' instead." #-}

-- | A value that determines how Elastic Transcoder interprets values that you specified for @HorizontalOffset@ , @VerticalOffset@ , @MaxWidth@ , and @MaxHeight@ :
--
--
--     * __Content__ : @HorizontalOffset@ and @VerticalOffset@ values are calculated based on the borders of the video excluding black bars added by Elastic Transcoder, if any. In addition, @MaxWidth@ and @MaxHeight@ , if specified as a percentage, are calculated based on the borders of the video excluding black bars added by Elastic Transcoder, if any.
--
--
--     * __Frame__ : @HorizontalOffset@ and @VerticalOffset@ values are calculated based on the borders of the video including black bars added by Elastic Transcoder, if any. In addition, @MaxWidth@ and @MaxHeight@ , if specified as a percentage, are calculated based on the borders of the video including black bars added by Elastic Transcoder, if any.
--
--
--
-- /Note:/ Consider using 'target' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pwTarget :: Lens.Lens' PresetWatermark (Lude.Maybe Lude.Text)
pwTarget = Lens.lens (target :: PresetWatermark -> Lude.Maybe Lude.Text) (\s a -> s {target = a} :: PresetWatermark)
{-# DEPRECATED pwTarget "Use generic-lens or generic-optics with 'target' instead." #-}

instance Lude.FromJSON PresetWatermark where
  parseJSON =
    Lude.withObject
      "PresetWatermark"
      ( \x ->
          PresetWatermark'
            Lude.<$> (x Lude..:? "VerticalAlign")
            Lude.<*> (x Lude..:? "SizingPolicy")
            Lude.<*> (x Lude..:? "HorizontalOffset")
            Lude.<*> (x Lude..:? "MaxHeight")
            Lude.<*> (x Lude..:? "Opacity")
            Lude.<*> (x Lude..:? "VerticalOffset")
            Lude.<*> (x Lude..:? "MaxWidth")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "HorizontalAlign")
            Lude.<*> (x Lude..:? "Target")
      )

instance Lude.ToJSON PresetWatermark where
  toJSON PresetWatermark' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("VerticalAlign" Lude..=) Lude.<$> verticalAlign,
            ("SizingPolicy" Lude..=) Lude.<$> sizingPolicy,
            ("HorizontalOffset" Lude..=) Lude.<$> horizontalOffset,
            ("MaxHeight" Lude..=) Lude.<$> maxHeight,
            ("Opacity" Lude..=) Lude.<$> opacity,
            ("VerticalOffset" Lude..=) Lude.<$> verticalOffset,
            ("MaxWidth" Lude..=) Lude.<$> maxWidth,
            ("Id" Lude..=) Lude.<$> id,
            ("HorizontalAlign" Lude..=) Lude.<$> horizontalAlign,
            ("Target" Lude..=) Lude.<$> target
          ]
      )
