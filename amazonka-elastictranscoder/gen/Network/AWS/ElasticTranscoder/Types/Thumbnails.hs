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
-- Module      : Network.AWS.ElasticTranscoder.Types.Thumbnails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.Thumbnails where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Thumbnails for videos.
--
-- /See:/ 'newThumbnails' smart constructor.
data Thumbnails = Thumbnails'
  { -- | The format of thumbnails, if any. Valid values are @jpg@ and @png@.
    --
    -- You specify whether you want Elastic Transcoder to create thumbnails
    -- when you create a job.
    format :: Prelude.Maybe Prelude.Text,
    -- | Specify one of the following values to control scaling of thumbnails:
    --
    -- -   @Fit@: Elastic Transcoder scales thumbnails so they match the value
    --     that you specified in thumbnail MaxWidth or MaxHeight settings
    --     without exceeding the other value.
    --
    -- -   @Fill@: Elastic Transcoder scales thumbnails so they match the value
    --     that you specified in thumbnail @MaxWidth@ or @MaxHeight@ settings
    --     and matches or exceeds the other value. Elastic Transcoder centers
    --     the image in thumbnails and then crops in the dimension (if any)
    --     that exceeds the maximum value.
    --
    -- -   @Stretch@: Elastic Transcoder stretches thumbnails to match the
    --     values that you specified for thumbnail @MaxWidth@ and @MaxHeight@
    --     settings. If the relative proportions of the input video and
    --     thumbnails are different, the thumbnails will be distorted.
    --
    -- -   @Keep@: Elastic Transcoder does not scale thumbnails. If either
    --     dimension of the input video exceeds the values that you specified
    --     for thumbnail @MaxWidth@ and @MaxHeight@ settings, Elastic
    --     Transcoder crops the thumbnails.
    --
    -- -   @ShrinkToFit@: Elastic Transcoder scales thumbnails down so that
    --     their dimensions match the values that you specified for at least
    --     one of thumbnail @MaxWidth@ and @MaxHeight@ without exceeding either
    --     value. If you specify this option, Elastic Transcoder does not scale
    --     thumbnails up.
    --
    -- -   @ShrinkToFill@: Elastic Transcoder scales thumbnails down so that
    --     their dimensions match the values that you specified for at least
    --     one of @MaxWidth@ and @MaxHeight@ without dropping below either
    --     value. If you specify this option, Elastic Transcoder does not scale
    --     thumbnails up.
    sizingPolicy :: Prelude.Maybe Prelude.Text,
    -- | The approximate number of seconds between thumbnails. Specify an integer
    -- value.
    interval :: Prelude.Maybe Prelude.Text,
    -- | To better control resolution and aspect ratio of thumbnails, we
    -- recommend that you use the values @MaxWidth@, @MaxHeight@,
    -- @SizingPolicy@, and @PaddingPolicy@ instead of @Resolution@ and
    -- @AspectRatio@. The two groups of settings are mutually exclusive. Do not
    -- use them together.
    --
    -- The aspect ratio of thumbnails. Valid values include:
    --
    -- @auto@, @1:1@, @4:3@, @3:2@, @16:9@
    --
    -- If you specify @auto@, Elastic Transcoder tries to preserve the aspect
    -- ratio of the video in the output file.
    aspectRatio :: Prelude.Maybe Prelude.Text,
    -- | When you set @PaddingPolicy@ to @Pad@, Elastic Transcoder may add black
    -- bars to the top and bottom and\/or left and right sides of thumbnails to
    -- make the total size of the thumbnails match the values that you
    -- specified for thumbnail @MaxWidth@ and @MaxHeight@ settings.
    paddingPolicy :: Prelude.Maybe Prelude.Text,
    -- | To better control resolution and aspect ratio of thumbnails, we
    -- recommend that you use the values @MaxWidth@, @MaxHeight@,
    -- @SizingPolicy@, and @PaddingPolicy@ instead of @Resolution@ and
    -- @AspectRatio@. The two groups of settings are mutually exclusive. Do not
    -- use them together.
    --
    -- The width and height of thumbnail files in pixels. Specify a value in
    -- the format @ width @ x @ height @ where both values are even integers.
    -- The values cannot exceed the width and height that you specified in the
    -- @Video:Resolution@ object.
    resolution :: Prelude.Maybe Prelude.Text,
    -- | The maximum height of thumbnails in pixels. If you specify auto, Elastic
    -- Transcoder uses 1080 (Full HD) as the default value. If you specify a
    -- numeric value, enter an even integer between 32 and 3072.
    maxHeight :: Prelude.Maybe Prelude.Text,
    -- | The maximum width of thumbnails in pixels. If you specify auto, Elastic
    -- Transcoder uses 1920 (Full HD) as the default value. If you specify a
    -- numeric value, enter an even integer between 32 and 4096.
    maxWidth :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Thumbnails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'format', 'thumbnails_format' - The format of thumbnails, if any. Valid values are @jpg@ and @png@.
--
-- You specify whether you want Elastic Transcoder to create thumbnails
-- when you create a job.
--
-- 'sizingPolicy', 'thumbnails_sizingPolicy' - Specify one of the following values to control scaling of thumbnails:
--
-- -   @Fit@: Elastic Transcoder scales thumbnails so they match the value
--     that you specified in thumbnail MaxWidth or MaxHeight settings
--     without exceeding the other value.
--
-- -   @Fill@: Elastic Transcoder scales thumbnails so they match the value
--     that you specified in thumbnail @MaxWidth@ or @MaxHeight@ settings
--     and matches or exceeds the other value. Elastic Transcoder centers
--     the image in thumbnails and then crops in the dimension (if any)
--     that exceeds the maximum value.
--
-- -   @Stretch@: Elastic Transcoder stretches thumbnails to match the
--     values that you specified for thumbnail @MaxWidth@ and @MaxHeight@
--     settings. If the relative proportions of the input video and
--     thumbnails are different, the thumbnails will be distorted.
--
-- -   @Keep@: Elastic Transcoder does not scale thumbnails. If either
--     dimension of the input video exceeds the values that you specified
--     for thumbnail @MaxWidth@ and @MaxHeight@ settings, Elastic
--     Transcoder crops the thumbnails.
--
-- -   @ShrinkToFit@: Elastic Transcoder scales thumbnails down so that
--     their dimensions match the values that you specified for at least
--     one of thumbnail @MaxWidth@ and @MaxHeight@ without exceeding either
--     value. If you specify this option, Elastic Transcoder does not scale
--     thumbnails up.
--
-- -   @ShrinkToFill@: Elastic Transcoder scales thumbnails down so that
--     their dimensions match the values that you specified for at least
--     one of @MaxWidth@ and @MaxHeight@ without dropping below either
--     value. If you specify this option, Elastic Transcoder does not scale
--     thumbnails up.
--
-- 'interval', 'thumbnails_interval' - The approximate number of seconds between thumbnails. Specify an integer
-- value.
--
-- 'aspectRatio', 'thumbnails_aspectRatio' - To better control resolution and aspect ratio of thumbnails, we
-- recommend that you use the values @MaxWidth@, @MaxHeight@,
-- @SizingPolicy@, and @PaddingPolicy@ instead of @Resolution@ and
-- @AspectRatio@. The two groups of settings are mutually exclusive. Do not
-- use them together.
--
-- The aspect ratio of thumbnails. Valid values include:
--
-- @auto@, @1:1@, @4:3@, @3:2@, @16:9@
--
-- If you specify @auto@, Elastic Transcoder tries to preserve the aspect
-- ratio of the video in the output file.
--
-- 'paddingPolicy', 'thumbnails_paddingPolicy' - When you set @PaddingPolicy@ to @Pad@, Elastic Transcoder may add black
-- bars to the top and bottom and\/or left and right sides of thumbnails to
-- make the total size of the thumbnails match the values that you
-- specified for thumbnail @MaxWidth@ and @MaxHeight@ settings.
--
-- 'resolution', 'thumbnails_resolution' - To better control resolution and aspect ratio of thumbnails, we
-- recommend that you use the values @MaxWidth@, @MaxHeight@,
-- @SizingPolicy@, and @PaddingPolicy@ instead of @Resolution@ and
-- @AspectRatio@. The two groups of settings are mutually exclusive. Do not
-- use them together.
--
-- The width and height of thumbnail files in pixels. Specify a value in
-- the format @ width @ x @ height @ where both values are even integers.
-- The values cannot exceed the width and height that you specified in the
-- @Video:Resolution@ object.
--
-- 'maxHeight', 'thumbnails_maxHeight' - The maximum height of thumbnails in pixels. If you specify auto, Elastic
-- Transcoder uses 1080 (Full HD) as the default value. If you specify a
-- numeric value, enter an even integer between 32 and 3072.
--
-- 'maxWidth', 'thumbnails_maxWidth' - The maximum width of thumbnails in pixels. If you specify auto, Elastic
-- Transcoder uses 1920 (Full HD) as the default value. If you specify a
-- numeric value, enter an even integer between 32 and 4096.
newThumbnails ::
  Thumbnails
newThumbnails =
  Thumbnails'
    { format = Prelude.Nothing,
      sizingPolicy = Prelude.Nothing,
      interval = Prelude.Nothing,
      aspectRatio = Prelude.Nothing,
      paddingPolicy = Prelude.Nothing,
      resolution = Prelude.Nothing,
      maxHeight = Prelude.Nothing,
      maxWidth = Prelude.Nothing
    }

-- | The format of thumbnails, if any. Valid values are @jpg@ and @png@.
--
-- You specify whether you want Elastic Transcoder to create thumbnails
-- when you create a job.
thumbnails_format :: Lens.Lens' Thumbnails (Prelude.Maybe Prelude.Text)
thumbnails_format = Lens.lens (\Thumbnails' {format} -> format) (\s@Thumbnails' {} a -> s {format = a} :: Thumbnails)

-- | Specify one of the following values to control scaling of thumbnails:
--
-- -   @Fit@: Elastic Transcoder scales thumbnails so they match the value
--     that you specified in thumbnail MaxWidth or MaxHeight settings
--     without exceeding the other value.
--
-- -   @Fill@: Elastic Transcoder scales thumbnails so they match the value
--     that you specified in thumbnail @MaxWidth@ or @MaxHeight@ settings
--     and matches or exceeds the other value. Elastic Transcoder centers
--     the image in thumbnails and then crops in the dimension (if any)
--     that exceeds the maximum value.
--
-- -   @Stretch@: Elastic Transcoder stretches thumbnails to match the
--     values that you specified for thumbnail @MaxWidth@ and @MaxHeight@
--     settings. If the relative proportions of the input video and
--     thumbnails are different, the thumbnails will be distorted.
--
-- -   @Keep@: Elastic Transcoder does not scale thumbnails. If either
--     dimension of the input video exceeds the values that you specified
--     for thumbnail @MaxWidth@ and @MaxHeight@ settings, Elastic
--     Transcoder crops the thumbnails.
--
-- -   @ShrinkToFit@: Elastic Transcoder scales thumbnails down so that
--     their dimensions match the values that you specified for at least
--     one of thumbnail @MaxWidth@ and @MaxHeight@ without exceeding either
--     value. If you specify this option, Elastic Transcoder does not scale
--     thumbnails up.
--
-- -   @ShrinkToFill@: Elastic Transcoder scales thumbnails down so that
--     their dimensions match the values that you specified for at least
--     one of @MaxWidth@ and @MaxHeight@ without dropping below either
--     value. If you specify this option, Elastic Transcoder does not scale
--     thumbnails up.
thumbnails_sizingPolicy :: Lens.Lens' Thumbnails (Prelude.Maybe Prelude.Text)
thumbnails_sizingPolicy = Lens.lens (\Thumbnails' {sizingPolicy} -> sizingPolicy) (\s@Thumbnails' {} a -> s {sizingPolicy = a} :: Thumbnails)

-- | The approximate number of seconds between thumbnails. Specify an integer
-- value.
thumbnails_interval :: Lens.Lens' Thumbnails (Prelude.Maybe Prelude.Text)
thumbnails_interval = Lens.lens (\Thumbnails' {interval} -> interval) (\s@Thumbnails' {} a -> s {interval = a} :: Thumbnails)

-- | To better control resolution and aspect ratio of thumbnails, we
-- recommend that you use the values @MaxWidth@, @MaxHeight@,
-- @SizingPolicy@, and @PaddingPolicy@ instead of @Resolution@ and
-- @AspectRatio@. The two groups of settings are mutually exclusive. Do not
-- use them together.
--
-- The aspect ratio of thumbnails. Valid values include:
--
-- @auto@, @1:1@, @4:3@, @3:2@, @16:9@
--
-- If you specify @auto@, Elastic Transcoder tries to preserve the aspect
-- ratio of the video in the output file.
thumbnails_aspectRatio :: Lens.Lens' Thumbnails (Prelude.Maybe Prelude.Text)
thumbnails_aspectRatio = Lens.lens (\Thumbnails' {aspectRatio} -> aspectRatio) (\s@Thumbnails' {} a -> s {aspectRatio = a} :: Thumbnails)

-- | When you set @PaddingPolicy@ to @Pad@, Elastic Transcoder may add black
-- bars to the top and bottom and\/or left and right sides of thumbnails to
-- make the total size of the thumbnails match the values that you
-- specified for thumbnail @MaxWidth@ and @MaxHeight@ settings.
thumbnails_paddingPolicy :: Lens.Lens' Thumbnails (Prelude.Maybe Prelude.Text)
thumbnails_paddingPolicy = Lens.lens (\Thumbnails' {paddingPolicy} -> paddingPolicy) (\s@Thumbnails' {} a -> s {paddingPolicy = a} :: Thumbnails)

-- | To better control resolution and aspect ratio of thumbnails, we
-- recommend that you use the values @MaxWidth@, @MaxHeight@,
-- @SizingPolicy@, and @PaddingPolicy@ instead of @Resolution@ and
-- @AspectRatio@. The two groups of settings are mutually exclusive. Do not
-- use them together.
--
-- The width and height of thumbnail files in pixels. Specify a value in
-- the format @ width @ x @ height @ where both values are even integers.
-- The values cannot exceed the width and height that you specified in the
-- @Video:Resolution@ object.
thumbnails_resolution :: Lens.Lens' Thumbnails (Prelude.Maybe Prelude.Text)
thumbnails_resolution = Lens.lens (\Thumbnails' {resolution} -> resolution) (\s@Thumbnails' {} a -> s {resolution = a} :: Thumbnails)

-- | The maximum height of thumbnails in pixels. If you specify auto, Elastic
-- Transcoder uses 1080 (Full HD) as the default value. If you specify a
-- numeric value, enter an even integer between 32 and 3072.
thumbnails_maxHeight :: Lens.Lens' Thumbnails (Prelude.Maybe Prelude.Text)
thumbnails_maxHeight = Lens.lens (\Thumbnails' {maxHeight} -> maxHeight) (\s@Thumbnails' {} a -> s {maxHeight = a} :: Thumbnails)

-- | The maximum width of thumbnails in pixels. If you specify auto, Elastic
-- Transcoder uses 1920 (Full HD) as the default value. If you specify a
-- numeric value, enter an even integer between 32 and 4096.
thumbnails_maxWidth :: Lens.Lens' Thumbnails (Prelude.Maybe Prelude.Text)
thumbnails_maxWidth = Lens.lens (\Thumbnails' {maxWidth} -> maxWidth) (\s@Thumbnails' {} a -> s {maxWidth = a} :: Thumbnails)

instance Prelude.FromJSON Thumbnails where
  parseJSON =
    Prelude.withObject
      "Thumbnails"
      ( \x ->
          Thumbnails'
            Prelude.<$> (x Prelude..:? "Format")
            Prelude.<*> (x Prelude..:? "SizingPolicy")
            Prelude.<*> (x Prelude..:? "Interval")
            Prelude.<*> (x Prelude..:? "AspectRatio")
            Prelude.<*> (x Prelude..:? "PaddingPolicy")
            Prelude.<*> (x Prelude..:? "Resolution")
            Prelude.<*> (x Prelude..:? "MaxHeight")
            Prelude.<*> (x Prelude..:? "MaxWidth")
      )

instance Prelude.Hashable Thumbnails

instance Prelude.NFData Thumbnails

instance Prelude.ToJSON Thumbnails where
  toJSON Thumbnails' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Format" Prelude..=) Prelude.<$> format,
            ("SizingPolicy" Prelude..=) Prelude.<$> sizingPolicy,
            ("Interval" Prelude..=) Prelude.<$> interval,
            ("AspectRatio" Prelude..=) Prelude.<$> aspectRatio,
            ("PaddingPolicy" Prelude..=)
              Prelude.<$> paddingPolicy,
            ("Resolution" Prelude..=) Prelude.<$> resolution,
            ("MaxHeight" Prelude..=) Prelude.<$> maxHeight,
            ("MaxWidth" Prelude..=) Prelude.<$> maxWidth
          ]
      )
