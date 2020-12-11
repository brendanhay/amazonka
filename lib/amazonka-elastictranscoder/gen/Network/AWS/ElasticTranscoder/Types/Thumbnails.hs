-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.Thumbnails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.Thumbnails
  ( Thumbnails (..),

    -- * Smart constructor
    mkThumbnails,

    -- * Lenses
    tSizingPolicy,
    tFormat,
    tMaxHeight,
    tResolution,
    tAspectRatio,
    tPaddingPolicy,
    tInterval,
    tMaxWidth,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Thumbnails for videos.
--
-- /See:/ 'mkThumbnails' smart constructor.
data Thumbnails = Thumbnails'
  { sizingPolicy :: Lude.Maybe Lude.Text,
    format :: Lude.Maybe Lude.Text,
    maxHeight :: Lude.Maybe Lude.Text,
    resolution :: Lude.Maybe Lude.Text,
    aspectRatio :: Lude.Maybe Lude.Text,
    paddingPolicy :: Lude.Maybe Lude.Text,
    interval :: Lude.Maybe Lude.Text,
    maxWidth :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Thumbnails' with the minimum fields required to make a request.
--
-- * 'aspectRatio' - /Important:/ To better control resolution and aspect ratio of thumbnails, we recommend that you use the values @MaxWidth@ , @MaxHeight@ , @SizingPolicy@ , and @PaddingPolicy@ instead of @Resolution@ and @AspectRatio@ . The two groups of settings are mutually exclusive. Do not use them together.
--
-- The aspect ratio of thumbnails. Valid values include:
-- @auto@ , @1:1@ , @4:3@ , @3:2@ , @16:9@
-- If you specify @auto@ , Elastic Transcoder tries to preserve the aspect ratio of the video in the output file.
-- * 'format' - The format of thumbnails, if any. Valid values are @jpg@ and @png@ .
--
-- You specify whether you want Elastic Transcoder to create thumbnails when you create a job.
-- * 'interval' - The approximate number of seconds between thumbnails. Specify an integer value.
-- * 'maxHeight' - The maximum height of thumbnails in pixels. If you specify auto, Elastic Transcoder uses 1080 (Full HD) as the default value. If you specify a numeric value, enter an even integer between 32 and 3072.
-- * 'maxWidth' - The maximum width of thumbnails in pixels. If you specify auto, Elastic Transcoder uses 1920 (Full HD) as the default value. If you specify a numeric value, enter an even integer between 32 and 4096.
-- * 'paddingPolicy' - When you set @PaddingPolicy@ to @Pad@ , Elastic Transcoder may add black bars to the top and bottom and/or left and right sides of thumbnails to make the total size of the thumbnails match the values that you specified for thumbnail @MaxWidth@ and @MaxHeight@ settings.
-- * 'resolution' - /Important:/ To better control resolution and aspect ratio of thumbnails, we recommend that you use the values @MaxWidth@ , @MaxHeight@ , @SizingPolicy@ , and @PaddingPolicy@ instead of @Resolution@ and @AspectRatio@ . The two groups of settings are mutually exclusive. Do not use them together.
--
-- The width and height of thumbnail files in pixels. Specify a value in the format @/width/ @ x @/height/ @ where both values are even integers. The values cannot exceed the width and height that you specified in the @Video:Resolution@ object.
-- * 'sizingPolicy' - Specify one of the following values to control scaling of thumbnails:
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
mkThumbnails ::
  Thumbnails
mkThumbnails =
  Thumbnails'
    { sizingPolicy = Lude.Nothing,
      format = Lude.Nothing,
      maxHeight = Lude.Nothing,
      resolution = Lude.Nothing,
      aspectRatio = Lude.Nothing,
      paddingPolicy = Lude.Nothing,
      interval = Lude.Nothing,
      maxWidth = Lude.Nothing
    }

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
tSizingPolicy :: Lens.Lens' Thumbnails (Lude.Maybe Lude.Text)
tSizingPolicy = Lens.lens (sizingPolicy :: Thumbnails -> Lude.Maybe Lude.Text) (\s a -> s {sizingPolicy = a} :: Thumbnails)
{-# DEPRECATED tSizingPolicy "Use generic-lens or generic-optics with 'sizingPolicy' instead." #-}

-- | The format of thumbnails, if any. Valid values are @jpg@ and @png@ .
--
-- You specify whether you want Elastic Transcoder to create thumbnails when you create a job.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tFormat :: Lens.Lens' Thumbnails (Lude.Maybe Lude.Text)
tFormat = Lens.lens (format :: Thumbnails -> Lude.Maybe Lude.Text) (\s a -> s {format = a} :: Thumbnails)
{-# DEPRECATED tFormat "Use generic-lens or generic-optics with 'format' instead." #-}

-- | The maximum height of thumbnails in pixels. If you specify auto, Elastic Transcoder uses 1080 (Full HD) as the default value. If you specify a numeric value, enter an even integer between 32 and 3072.
--
-- /Note:/ Consider using 'maxHeight' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tMaxHeight :: Lens.Lens' Thumbnails (Lude.Maybe Lude.Text)
tMaxHeight = Lens.lens (maxHeight :: Thumbnails -> Lude.Maybe Lude.Text) (\s a -> s {maxHeight = a} :: Thumbnails)
{-# DEPRECATED tMaxHeight "Use generic-lens or generic-optics with 'maxHeight' instead." #-}

-- | /Important:/ To better control resolution and aspect ratio of thumbnails, we recommend that you use the values @MaxWidth@ , @MaxHeight@ , @SizingPolicy@ , and @PaddingPolicy@ instead of @Resolution@ and @AspectRatio@ . The two groups of settings are mutually exclusive. Do not use them together.
--
-- The width and height of thumbnail files in pixels. Specify a value in the format @/width/ @ x @/height/ @ where both values are even integers. The values cannot exceed the width and height that you specified in the @Video:Resolution@ object.
--
-- /Note:/ Consider using 'resolution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tResolution :: Lens.Lens' Thumbnails (Lude.Maybe Lude.Text)
tResolution = Lens.lens (resolution :: Thumbnails -> Lude.Maybe Lude.Text) (\s a -> s {resolution = a} :: Thumbnails)
{-# DEPRECATED tResolution "Use generic-lens or generic-optics with 'resolution' instead." #-}

-- | /Important:/ To better control resolution and aspect ratio of thumbnails, we recommend that you use the values @MaxWidth@ , @MaxHeight@ , @SizingPolicy@ , and @PaddingPolicy@ instead of @Resolution@ and @AspectRatio@ . The two groups of settings are mutually exclusive. Do not use them together.
--
-- The aspect ratio of thumbnails. Valid values include:
-- @auto@ , @1:1@ , @4:3@ , @3:2@ , @16:9@
-- If you specify @auto@ , Elastic Transcoder tries to preserve the aspect ratio of the video in the output file.
--
-- /Note:/ Consider using 'aspectRatio' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tAspectRatio :: Lens.Lens' Thumbnails (Lude.Maybe Lude.Text)
tAspectRatio = Lens.lens (aspectRatio :: Thumbnails -> Lude.Maybe Lude.Text) (\s a -> s {aspectRatio = a} :: Thumbnails)
{-# DEPRECATED tAspectRatio "Use generic-lens or generic-optics with 'aspectRatio' instead." #-}

-- | When you set @PaddingPolicy@ to @Pad@ , Elastic Transcoder may add black bars to the top and bottom and/or left and right sides of thumbnails to make the total size of the thumbnails match the values that you specified for thumbnail @MaxWidth@ and @MaxHeight@ settings.
--
-- /Note:/ Consider using 'paddingPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tPaddingPolicy :: Lens.Lens' Thumbnails (Lude.Maybe Lude.Text)
tPaddingPolicy = Lens.lens (paddingPolicy :: Thumbnails -> Lude.Maybe Lude.Text) (\s a -> s {paddingPolicy = a} :: Thumbnails)
{-# DEPRECATED tPaddingPolicy "Use generic-lens or generic-optics with 'paddingPolicy' instead." #-}

-- | The approximate number of seconds between thumbnails. Specify an integer value.
--
-- /Note:/ Consider using 'interval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tInterval :: Lens.Lens' Thumbnails (Lude.Maybe Lude.Text)
tInterval = Lens.lens (interval :: Thumbnails -> Lude.Maybe Lude.Text) (\s a -> s {interval = a} :: Thumbnails)
{-# DEPRECATED tInterval "Use generic-lens or generic-optics with 'interval' instead." #-}

-- | The maximum width of thumbnails in pixels. If you specify auto, Elastic Transcoder uses 1920 (Full HD) as the default value. If you specify a numeric value, enter an even integer between 32 and 4096.
--
-- /Note:/ Consider using 'maxWidth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tMaxWidth :: Lens.Lens' Thumbnails (Lude.Maybe Lude.Text)
tMaxWidth = Lens.lens (maxWidth :: Thumbnails -> Lude.Maybe Lude.Text) (\s a -> s {maxWidth = a} :: Thumbnails)
{-# DEPRECATED tMaxWidth "Use generic-lens or generic-optics with 'maxWidth' instead." #-}

instance Lude.FromJSON Thumbnails where
  parseJSON =
    Lude.withObject
      "Thumbnails"
      ( \x ->
          Thumbnails'
            Lude.<$> (x Lude..:? "SizingPolicy")
            Lude.<*> (x Lude..:? "Format")
            Lude.<*> (x Lude..:? "MaxHeight")
            Lude.<*> (x Lude..:? "Resolution")
            Lude.<*> (x Lude..:? "AspectRatio")
            Lude.<*> (x Lude..:? "PaddingPolicy")
            Lude.<*> (x Lude..:? "Interval")
            Lude.<*> (x Lude..:? "MaxWidth")
      )

instance Lude.ToJSON Thumbnails where
  toJSON Thumbnails' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SizingPolicy" Lude..=) Lude.<$> sizingPolicy,
            ("Format" Lude..=) Lude.<$> format,
            ("MaxHeight" Lude..=) Lude.<$> maxHeight,
            ("Resolution" Lude..=) Lude.<$> resolution,
            ("AspectRatio" Lude..=) Lude.<$> aspectRatio,
            ("PaddingPolicy" Lude..=) Lude.<$> paddingPolicy,
            ("Interval" Lude..=) Lude.<$> interval,
            ("MaxWidth" Lude..=) Lude.<$> maxWidth
          ]
      )
