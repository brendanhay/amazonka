{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.PresetWatermark
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticTranscoder.Types.PresetWatermark
  ( PresetWatermark (..)
  -- * Smart constructor
  , mkPresetWatermark
  -- * Lenses
  , pwHorizontalAlign
  , pwHorizontalOffset
  , pwId
  , pwMaxHeight
  , pwMaxWidth
  , pwOpacity
  , pwSizingPolicy
  , pwTarget
  , pwVerticalAlign
  , pwVerticalOffset
  ) where

import qualified Network.AWS.ElasticTranscoder.Types.HorizontalAlign as Types
import qualified Network.AWS.ElasticTranscoder.Types.Opacity as Types
import qualified Network.AWS.ElasticTranscoder.Types.PixelsOrPercent as Types
import qualified Network.AWS.ElasticTranscoder.Types.PresetWatermarkId as Types
import qualified Network.AWS.ElasticTranscoder.Types.SizingPolicy as Types
import qualified Network.AWS.ElasticTranscoder.Types.Target as Types
import qualified Network.AWS.ElasticTranscoder.Types.VerticalAlign as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Settings for the size, location, and opacity of graphics that you want Elastic Transcoder to overlay over videos that are transcoded using this preset. You can specify settings for up to four watermarks. Watermarks appear in the specified size and location, and with the specified opacity for the duration of the transcoded video.
--
-- Watermarks can be in .png or .jpg format. If you want to display a watermark that is not rectangular, use the .png format, which supports transparency.
-- When you create a job that uses this preset, you specify the .png or .jpg graphics that you want Elastic Transcoder to include in the transcoded videos. You can specify fewer graphics in the job than you specify watermark settings in the preset, which allows you to use the same preset for up to four watermarks that have different dimensions.
--
-- /See:/ 'mkPresetWatermark' smart constructor.
data PresetWatermark = PresetWatermark'
  { horizontalAlign :: Core.Maybe Types.HorizontalAlign
    -- ^ The horizontal position of the watermark unless you specify a non-zero value for @HorizontalOffset@ : 
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
  , horizontalOffset :: Core.Maybe Types.PixelsOrPercent
    -- ^ The amount by which you want the horizontal position of the watermark to be offset from the position specified by HorizontalAlign: 
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
  , id :: Core.Maybe Types.PresetWatermarkId
    -- ^ A unique identifier for the settings for one watermark. The value of @Id@ can be up to 40 characters long. 
  , maxHeight :: Core.Maybe Types.PixelsOrPercent
    -- ^ The maximum height of the watermark in one of the following formats: 
--
--
--     * number of pixels (px): The minimum value is 16 pixels, and the maximum value is the value of @MaxHeight@ .
--
--
--     * integer percentage (%): The range of valid values is 0 to 100. Use the value of @Target@ to specify whether you want Elastic Transcoder to include the black bars that are added by Elastic Transcoder, if any, in the calculation.
--
--
-- If you specify the value in pixels, it must be less than or equal to the value of @MaxHeight@ .
  , maxWidth :: Core.Maybe Types.PixelsOrPercent
    -- ^ The maximum width of the watermark in one of the following formats: 
--
--
--     * number of pixels (px): The minimum value is 16 pixels, and the maximum value is the value of @MaxWidth@ .
--
--
--     * integer percentage (%): The range of valid values is 0 to 100. Use the value of @Target@ to specify whether you want Elastic Transcoder to include the black bars that are added by Elastic Transcoder, if any, in the calculation.
-- If you specify the value in pixels, it must be less than or equal to the value of @MaxWidth@ .
--
--
  , opacity :: Core.Maybe Types.Opacity
    -- ^ A percentage that indicates how much you want a watermark to obscure the video in the location where it appears. Valid values are 0 (the watermark is invisible) to 100 (the watermark completely obscures the video in the specified location). The datatype of @Opacity@ is float.
--
-- Elastic Transcoder supports transparent .png graphics. If you use a transparent .png, the transparent portion of the video appears as if you had specified a value of 0 for @Opacity@ . The .jpg file format doesn't support transparency.
  , sizingPolicy :: Core.Maybe Types.SizingPolicy
    -- ^ A value that controls scaling of the watermark: 
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
  , target :: Core.Maybe Types.Target
    -- ^ A value that determines how Elastic Transcoder interprets values that you specified for @HorizontalOffset@ , @VerticalOffset@ , @MaxWidth@ , and @MaxHeight@ :
--
--
--     * __Content__ : @HorizontalOffset@ and @VerticalOffset@ values are calculated based on the borders of the video excluding black bars added by Elastic Transcoder, if any. In addition, @MaxWidth@ and @MaxHeight@ , if specified as a percentage, are calculated based on the borders of the video excluding black bars added by Elastic Transcoder, if any.
--
--
--     * __Frame__ : @HorizontalOffset@ and @VerticalOffset@ values are calculated based on the borders of the video including black bars added by Elastic Transcoder, if any. In addition, @MaxWidth@ and @MaxHeight@ , if specified as a percentage, are calculated based on the borders of the video including black bars added by Elastic Transcoder, if any.
--
--
  , verticalAlign :: Core.Maybe Types.VerticalAlign
    -- ^ The vertical position of the watermark unless you specify a non-zero value for @VerticalOffset@ : 
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
  , verticalOffset :: Core.Maybe Types.PixelsOrPercent
    -- ^ @VerticalOffset@ 
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PresetWatermark' value with any optional fields omitted.
mkPresetWatermark
    :: PresetWatermark
mkPresetWatermark
  = PresetWatermark'{horizontalAlign = Core.Nothing,
                     horizontalOffset = Core.Nothing, id = Core.Nothing,
                     maxHeight = Core.Nothing, maxWidth = Core.Nothing,
                     opacity = Core.Nothing, sizingPolicy = Core.Nothing,
                     target = Core.Nothing, verticalAlign = Core.Nothing,
                     verticalOffset = Core.Nothing}

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
pwHorizontalAlign :: Lens.Lens' PresetWatermark (Core.Maybe Types.HorizontalAlign)
pwHorizontalAlign = Lens.field @"horizontalAlign"
{-# INLINEABLE pwHorizontalAlign #-}
{-# DEPRECATED horizontalAlign "Use generic-lens or generic-optics with 'horizontalAlign' instead"  #-}

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
pwHorizontalOffset :: Lens.Lens' PresetWatermark (Core.Maybe Types.PixelsOrPercent)
pwHorizontalOffset = Lens.field @"horizontalOffset"
{-# INLINEABLE pwHorizontalOffset #-}
{-# DEPRECATED horizontalOffset "Use generic-lens or generic-optics with 'horizontalOffset' instead"  #-}

-- | A unique identifier for the settings for one watermark. The value of @Id@ can be up to 40 characters long. 
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pwId :: Lens.Lens' PresetWatermark (Core.Maybe Types.PresetWatermarkId)
pwId = Lens.field @"id"
{-# INLINEABLE pwId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

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
pwMaxHeight :: Lens.Lens' PresetWatermark (Core.Maybe Types.PixelsOrPercent)
pwMaxHeight = Lens.field @"maxHeight"
{-# INLINEABLE pwMaxHeight #-}
{-# DEPRECATED maxHeight "Use generic-lens or generic-optics with 'maxHeight' instead"  #-}

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
pwMaxWidth :: Lens.Lens' PresetWatermark (Core.Maybe Types.PixelsOrPercent)
pwMaxWidth = Lens.field @"maxWidth"
{-# INLINEABLE pwMaxWidth #-}
{-# DEPRECATED maxWidth "Use generic-lens or generic-optics with 'maxWidth' instead"  #-}

-- | A percentage that indicates how much you want a watermark to obscure the video in the location where it appears. Valid values are 0 (the watermark is invisible) to 100 (the watermark completely obscures the video in the specified location). The datatype of @Opacity@ is float.
--
-- Elastic Transcoder supports transparent .png graphics. If you use a transparent .png, the transparent portion of the video appears as if you had specified a value of 0 for @Opacity@ . The .jpg file format doesn't support transparency.
--
-- /Note:/ Consider using 'opacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pwOpacity :: Lens.Lens' PresetWatermark (Core.Maybe Types.Opacity)
pwOpacity = Lens.field @"opacity"
{-# INLINEABLE pwOpacity #-}
{-# DEPRECATED opacity "Use generic-lens or generic-optics with 'opacity' instead"  #-}

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
pwSizingPolicy :: Lens.Lens' PresetWatermark (Core.Maybe Types.SizingPolicy)
pwSizingPolicy = Lens.field @"sizingPolicy"
{-# INLINEABLE pwSizingPolicy #-}
{-# DEPRECATED sizingPolicy "Use generic-lens or generic-optics with 'sizingPolicy' instead"  #-}

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
pwTarget :: Lens.Lens' PresetWatermark (Core.Maybe Types.Target)
pwTarget = Lens.field @"target"
{-# INLINEABLE pwTarget #-}
{-# DEPRECATED target "Use generic-lens or generic-optics with 'target' instead"  #-}

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
pwVerticalAlign :: Lens.Lens' PresetWatermark (Core.Maybe Types.VerticalAlign)
pwVerticalAlign = Lens.field @"verticalAlign"
{-# INLINEABLE pwVerticalAlign #-}
{-# DEPRECATED verticalAlign "Use generic-lens or generic-optics with 'verticalAlign' instead"  #-}

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
pwVerticalOffset :: Lens.Lens' PresetWatermark (Core.Maybe Types.PixelsOrPercent)
pwVerticalOffset = Lens.field @"verticalOffset"
{-# INLINEABLE pwVerticalOffset #-}
{-# DEPRECATED verticalOffset "Use generic-lens or generic-optics with 'verticalOffset' instead"  #-}

instance Core.FromJSON PresetWatermark where
        toJSON PresetWatermark{..}
          = Core.object
              (Core.catMaybes
                 [("HorizontalAlign" Core..=) Core.<$> horizontalAlign,
                  ("HorizontalOffset" Core..=) Core.<$> horizontalOffset,
                  ("Id" Core..=) Core.<$> id,
                  ("MaxHeight" Core..=) Core.<$> maxHeight,
                  ("MaxWidth" Core..=) Core.<$> maxWidth,
                  ("Opacity" Core..=) Core.<$> opacity,
                  ("SizingPolicy" Core..=) Core.<$> sizingPolicy,
                  ("Target" Core..=) Core.<$> target,
                  ("VerticalAlign" Core..=) Core.<$> verticalAlign,
                  ("VerticalOffset" Core..=) Core.<$> verticalOffset])

instance Core.FromJSON PresetWatermark where
        parseJSON
          = Core.withObject "PresetWatermark" Core.$
              \ x ->
                PresetWatermark' Core.<$>
                  (x Core..:? "HorizontalAlign") Core.<*>
                    x Core..:? "HorizontalOffset"
                    Core.<*> x Core..:? "Id"
                    Core.<*> x Core..:? "MaxHeight"
                    Core.<*> x Core..:? "MaxWidth"
                    Core.<*> x Core..:? "Opacity"
                    Core.<*> x Core..:? "SizingPolicy"
                    Core.<*> x Core..:? "Target"
                    Core.<*> x Core..:? "VerticalAlign"
                    Core.<*> x Core..:? "VerticalOffset"
