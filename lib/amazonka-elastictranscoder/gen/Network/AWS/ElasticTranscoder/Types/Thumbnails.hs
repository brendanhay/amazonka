{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.Thumbnails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticTranscoder.Types.Thumbnails
  ( Thumbnails (..)
  -- * Smart constructor
  , mkThumbnails
  -- * Lenses
  , tAspectRatio
  , tFormat
  , tInterval
  , tMaxHeight
  , tMaxWidth
  , tPaddingPolicy
  , tResolution
  , tSizingPolicy
  ) where

import qualified Network.AWS.ElasticTranscoder.Types.AspectRatio as Types
import qualified Network.AWS.ElasticTranscoder.Types.Digits as Types
import qualified Network.AWS.ElasticTranscoder.Types.DigitsOrAuto as Types
import qualified Network.AWS.ElasticTranscoder.Types.JpgOrPng as Types
import qualified Network.AWS.ElasticTranscoder.Types.PaddingPolicy as Types
import qualified Network.AWS.ElasticTranscoder.Types.Resolution as Types
import qualified Network.AWS.ElasticTranscoder.Types.SizingPolicy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Thumbnails for videos.
--
-- /See:/ 'mkThumbnails' smart constructor.
data Thumbnails = Thumbnails'
  { aspectRatio :: Core.Maybe Types.AspectRatio
    -- ^ /Important:/ To better control resolution and aspect ratio of thumbnails, we recommend that you use the values @MaxWidth@ , @MaxHeight@ , @SizingPolicy@ , and @PaddingPolicy@ instead of @Resolution@ and @AspectRatio@ . The two groups of settings are mutually exclusive. Do not use them together.
--
-- The aspect ratio of thumbnails. Valid values include:
-- @auto@ , @1:1@ , @4:3@ , @3:2@ , @16:9@ 
-- If you specify @auto@ , Elastic Transcoder tries to preserve the aspect ratio of the video in the output file.
  , format :: Core.Maybe Types.JpgOrPng
    -- ^ The format of thumbnails, if any. Valid values are @jpg@ and @png@ . 
--
-- You specify whether you want Elastic Transcoder to create thumbnails when you create a job.
  , interval :: Core.Maybe Types.Digits
    -- ^ The approximate number of seconds between thumbnails. Specify an integer value.
  , maxHeight :: Core.Maybe Types.DigitsOrAuto
    -- ^ The maximum height of thumbnails in pixels. If you specify auto, Elastic Transcoder uses 1080 (Full HD) as the default value. If you specify a numeric value, enter an even integer between 32 and 3072.
  , maxWidth :: Core.Maybe Types.DigitsOrAuto
    -- ^ The maximum width of thumbnails in pixels. If you specify auto, Elastic Transcoder uses 1920 (Full HD) as the default value. If you specify a numeric value, enter an even integer between 32 and 4096.
  , paddingPolicy :: Core.Maybe Types.PaddingPolicy
    -- ^ When you set @PaddingPolicy@ to @Pad@ , Elastic Transcoder may add black bars to the top and bottom and/or left and right sides of thumbnails to make the total size of the thumbnails match the values that you specified for thumbnail @MaxWidth@ and @MaxHeight@ settings.
  , resolution :: Core.Maybe Types.Resolution
    -- ^ /Important:/ To better control resolution and aspect ratio of thumbnails, we recommend that you use the values @MaxWidth@ , @MaxHeight@ , @SizingPolicy@ , and @PaddingPolicy@ instead of @Resolution@ and @AspectRatio@ . The two groups of settings are mutually exclusive. Do not use them together.
--
-- The width and height of thumbnail files in pixels. Specify a value in the format @/width/ @ x @/height/ @ where both values are even integers. The values cannot exceed the width and height that you specified in the @Video:Resolution@ object.
  , sizingPolicy :: Core.Maybe Types.SizingPolicy
    -- ^ Specify one of the following values to control scaling of thumbnails:
--
--
--     * @Fit@ : Elastic Transcoder scales thumbnails so they match the value that you specified in thumbnail MaxWidth or MaxHeight settings without exceeding the other value. 
--
--
--     * @Fill@ : Elastic Transcoder scales thumbnails so they match the value that you specified in thumbnail @MaxWidth@ or @MaxHeight@ settings and matches or exceeds the other value. Elastic Transcoder centers the image in thumbnails and then crops in the dimension (if any) that exceeds the maximum value.
--
--
--     * @Stretch@ : Elastic Transcoder stretches thumbnails to match the values that you specified for thumbnail @MaxWidth@ and @MaxHeight@ settings. If the relative proportions of the input video and thumbnails are different, the thumbnails will be distorted.
--
--
--     * @Keep@ : Elastic Transcoder does not scale thumbnails. If either dimension of the input video exceeds the values that you specified for thumbnail @MaxWidth@ and @MaxHeight@ settings, Elastic Transcoder crops the thumbnails.
--
--
--     * @ShrinkToFit@ : Elastic Transcoder scales thumbnails down so that their dimensions match the values that you specified for at least one of thumbnail @MaxWidth@ and @MaxHeight@ without exceeding either value. If you specify this option, Elastic Transcoder does not scale thumbnails up.
--
--
--     * @ShrinkToFill@ : Elastic Transcoder scales thumbnails down so that their dimensions match the values that you specified for at least one of @MaxWidth@ and @MaxHeight@ without dropping below either value. If you specify this option, Elastic Transcoder does not scale thumbnails up.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Thumbnails' value with any optional fields omitted.
mkThumbnails
    :: Thumbnails
mkThumbnails
  = Thumbnails'{aspectRatio = Core.Nothing, format = Core.Nothing,
                interval = Core.Nothing, maxHeight = Core.Nothing,
                maxWidth = Core.Nothing, paddingPolicy = Core.Nothing,
                resolution = Core.Nothing, sizingPolicy = Core.Nothing}

-- | /Important:/ To better control resolution and aspect ratio of thumbnails, we recommend that you use the values @MaxWidth@ , @MaxHeight@ , @SizingPolicy@ , and @PaddingPolicy@ instead of @Resolution@ and @AspectRatio@ . The two groups of settings are mutually exclusive. Do not use them together.
--
-- The aspect ratio of thumbnails. Valid values include:
-- @auto@ , @1:1@ , @4:3@ , @3:2@ , @16:9@ 
-- If you specify @auto@ , Elastic Transcoder tries to preserve the aspect ratio of the video in the output file.
--
-- /Note:/ Consider using 'aspectRatio' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tAspectRatio :: Lens.Lens' Thumbnails (Core.Maybe Types.AspectRatio)
tAspectRatio = Lens.field @"aspectRatio"
{-# INLINEABLE tAspectRatio #-}
{-# DEPRECATED aspectRatio "Use generic-lens or generic-optics with 'aspectRatio' instead"  #-}

-- | The format of thumbnails, if any. Valid values are @jpg@ and @png@ . 
--
-- You specify whether you want Elastic Transcoder to create thumbnails when you create a job.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tFormat :: Lens.Lens' Thumbnails (Core.Maybe Types.JpgOrPng)
tFormat = Lens.field @"format"
{-# INLINEABLE tFormat #-}
{-# DEPRECATED format "Use generic-lens or generic-optics with 'format' instead"  #-}

-- | The approximate number of seconds between thumbnails. Specify an integer value.
--
-- /Note:/ Consider using 'interval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tInterval :: Lens.Lens' Thumbnails (Core.Maybe Types.Digits)
tInterval = Lens.field @"interval"
{-# INLINEABLE tInterval #-}
{-# DEPRECATED interval "Use generic-lens or generic-optics with 'interval' instead"  #-}

-- | The maximum height of thumbnails in pixels. If you specify auto, Elastic Transcoder uses 1080 (Full HD) as the default value. If you specify a numeric value, enter an even integer between 32 and 3072.
--
-- /Note:/ Consider using 'maxHeight' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tMaxHeight :: Lens.Lens' Thumbnails (Core.Maybe Types.DigitsOrAuto)
tMaxHeight = Lens.field @"maxHeight"
{-# INLINEABLE tMaxHeight #-}
{-# DEPRECATED maxHeight "Use generic-lens or generic-optics with 'maxHeight' instead"  #-}

-- | The maximum width of thumbnails in pixels. If you specify auto, Elastic Transcoder uses 1920 (Full HD) as the default value. If you specify a numeric value, enter an even integer between 32 and 4096.
--
-- /Note:/ Consider using 'maxWidth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tMaxWidth :: Lens.Lens' Thumbnails (Core.Maybe Types.DigitsOrAuto)
tMaxWidth = Lens.field @"maxWidth"
{-# INLINEABLE tMaxWidth #-}
{-# DEPRECATED maxWidth "Use generic-lens or generic-optics with 'maxWidth' instead"  #-}

-- | When you set @PaddingPolicy@ to @Pad@ , Elastic Transcoder may add black bars to the top and bottom and/or left and right sides of thumbnails to make the total size of the thumbnails match the values that you specified for thumbnail @MaxWidth@ and @MaxHeight@ settings.
--
-- /Note:/ Consider using 'paddingPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tPaddingPolicy :: Lens.Lens' Thumbnails (Core.Maybe Types.PaddingPolicy)
tPaddingPolicy = Lens.field @"paddingPolicy"
{-# INLINEABLE tPaddingPolicy #-}
{-# DEPRECATED paddingPolicy "Use generic-lens or generic-optics with 'paddingPolicy' instead"  #-}

-- | /Important:/ To better control resolution and aspect ratio of thumbnails, we recommend that you use the values @MaxWidth@ , @MaxHeight@ , @SizingPolicy@ , and @PaddingPolicy@ instead of @Resolution@ and @AspectRatio@ . The two groups of settings are mutually exclusive. Do not use them together.
--
-- The width and height of thumbnail files in pixels. Specify a value in the format @/width/ @ x @/height/ @ where both values are even integers. The values cannot exceed the width and height that you specified in the @Video:Resolution@ object.
--
-- /Note:/ Consider using 'resolution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tResolution :: Lens.Lens' Thumbnails (Core.Maybe Types.Resolution)
tResolution = Lens.field @"resolution"
{-# INLINEABLE tResolution #-}
{-# DEPRECATED resolution "Use generic-lens or generic-optics with 'resolution' instead"  #-}

-- | Specify one of the following values to control scaling of thumbnails:
--
--
--     * @Fit@ : Elastic Transcoder scales thumbnails so they match the value that you specified in thumbnail MaxWidth or MaxHeight settings without exceeding the other value. 
--
--
--     * @Fill@ : Elastic Transcoder scales thumbnails so they match the value that you specified in thumbnail @MaxWidth@ or @MaxHeight@ settings and matches or exceeds the other value. Elastic Transcoder centers the image in thumbnails and then crops in the dimension (if any) that exceeds the maximum value.
--
--
--     * @Stretch@ : Elastic Transcoder stretches thumbnails to match the values that you specified for thumbnail @MaxWidth@ and @MaxHeight@ settings. If the relative proportions of the input video and thumbnails are different, the thumbnails will be distorted.
--
--
--     * @Keep@ : Elastic Transcoder does not scale thumbnails. If either dimension of the input video exceeds the values that you specified for thumbnail @MaxWidth@ and @MaxHeight@ settings, Elastic Transcoder crops the thumbnails.
--
--
--     * @ShrinkToFit@ : Elastic Transcoder scales thumbnails down so that their dimensions match the values that you specified for at least one of thumbnail @MaxWidth@ and @MaxHeight@ without exceeding either value. If you specify this option, Elastic Transcoder does not scale thumbnails up.
--
--
--     * @ShrinkToFill@ : Elastic Transcoder scales thumbnails down so that their dimensions match the values that you specified for at least one of @MaxWidth@ and @MaxHeight@ without dropping below either value. If you specify this option, Elastic Transcoder does not scale thumbnails up.
--
--
--
-- /Note:/ Consider using 'sizingPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tSizingPolicy :: Lens.Lens' Thumbnails (Core.Maybe Types.SizingPolicy)
tSizingPolicy = Lens.field @"sizingPolicy"
{-# INLINEABLE tSizingPolicy #-}
{-# DEPRECATED sizingPolicy "Use generic-lens or generic-optics with 'sizingPolicy' instead"  #-}

instance Core.FromJSON Thumbnails where
        toJSON Thumbnails{..}
          = Core.object
              (Core.catMaybes
                 [("AspectRatio" Core..=) Core.<$> aspectRatio,
                  ("Format" Core..=) Core.<$> format,
                  ("Interval" Core..=) Core.<$> interval,
                  ("MaxHeight" Core..=) Core.<$> maxHeight,
                  ("MaxWidth" Core..=) Core.<$> maxWidth,
                  ("PaddingPolicy" Core..=) Core.<$> paddingPolicy,
                  ("Resolution" Core..=) Core.<$> resolution,
                  ("SizingPolicy" Core..=) Core.<$> sizingPolicy])

instance Core.FromJSON Thumbnails where
        parseJSON
          = Core.withObject "Thumbnails" Core.$
              \ x ->
                Thumbnails' Core.<$>
                  (x Core..:? "AspectRatio") Core.<*> x Core..:? "Format" Core.<*>
                    x Core..:? "Interval"
                    Core.<*> x Core..:? "MaxHeight"
                    Core.<*> x Core..:? "MaxWidth"
                    Core.<*> x Core..:? "PaddingPolicy"
                    Core.<*> x Core..:? "Resolution"
                    Core.<*> x Core..:? "SizingPolicy"
