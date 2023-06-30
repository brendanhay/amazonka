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
-- Module      : Amazonka.ElasticTranscoder.Types.Thumbnails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticTranscoder.Types.Thumbnails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Thumbnails for videos.
--
-- /See:/ 'newThumbnails' smart constructor.
data Thumbnails = Thumbnails'
  { -- | To better control resolution and aspect ratio of thumbnails, we
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
    -- | The format of thumbnails, if any. Valid values are @jpg@ and @png@.
    --
    -- You specify whether you want Elastic Transcoder to create thumbnails
    -- when you create a job.
    format :: Prelude.Maybe Prelude.Text,
    -- | The approximate number of seconds between thumbnails. Specify an integer
    -- value.
    interval :: Prelude.Maybe Prelude.Text,
    -- | The maximum height of thumbnails in pixels. If you specify auto, Elastic
    -- Transcoder uses 1080 (Full HD) as the default value. If you specify a
    -- numeric value, enter an even integer between 32 and 3072.
    maxHeight :: Prelude.Maybe Prelude.Text,
    -- | The maximum width of thumbnails in pixels. If you specify auto, Elastic
    -- Transcoder uses 1920 (Full HD) as the default value. If you specify a
    -- numeric value, enter an even integer between 32 and 4096.
    maxWidth :: Prelude.Maybe Prelude.Text,
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
    -- the format @ @/@width@/@ @ x @ @/@height@/@ @ where both values are even
    -- integers. The values cannot exceed the width and height that you
    -- specified in the @Video:Resolution@ object.
    resolution :: Prelude.Maybe Prelude.Text,
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
    sizingPolicy :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Thumbnails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'format', 'thumbnails_format' - The format of thumbnails, if any. Valid values are @jpg@ and @png@.
--
-- You specify whether you want Elastic Transcoder to create thumbnails
-- when you create a job.
--
-- 'interval', 'thumbnails_interval' - The approximate number of seconds between thumbnails. Specify an integer
-- value.
--
-- 'maxHeight', 'thumbnails_maxHeight' - The maximum height of thumbnails in pixels. If you specify auto, Elastic
-- Transcoder uses 1080 (Full HD) as the default value. If you specify a
-- numeric value, enter an even integer between 32 and 3072.
--
-- 'maxWidth', 'thumbnails_maxWidth' - The maximum width of thumbnails in pixels. If you specify auto, Elastic
-- Transcoder uses 1920 (Full HD) as the default value. If you specify a
-- numeric value, enter an even integer between 32 and 4096.
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
-- the format @ @/@width@/@ @ x @ @/@height@/@ @ where both values are even
-- integers. The values cannot exceed the width and height that you
-- specified in the @Video:Resolution@ object.
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
newThumbnails ::
  Thumbnails
newThumbnails =
  Thumbnails'
    { aspectRatio = Prelude.Nothing,
      format = Prelude.Nothing,
      interval = Prelude.Nothing,
      maxHeight = Prelude.Nothing,
      maxWidth = Prelude.Nothing,
      paddingPolicy = Prelude.Nothing,
      resolution = Prelude.Nothing,
      sizingPolicy = Prelude.Nothing
    }

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

-- | The format of thumbnails, if any. Valid values are @jpg@ and @png@.
--
-- You specify whether you want Elastic Transcoder to create thumbnails
-- when you create a job.
thumbnails_format :: Lens.Lens' Thumbnails (Prelude.Maybe Prelude.Text)
thumbnails_format = Lens.lens (\Thumbnails' {format} -> format) (\s@Thumbnails' {} a -> s {format = a} :: Thumbnails)

-- | The approximate number of seconds between thumbnails. Specify an integer
-- value.
thumbnails_interval :: Lens.Lens' Thumbnails (Prelude.Maybe Prelude.Text)
thumbnails_interval = Lens.lens (\Thumbnails' {interval} -> interval) (\s@Thumbnails' {} a -> s {interval = a} :: Thumbnails)

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
-- the format @ @/@width@/@ @ x @ @/@height@/@ @ where both values are even
-- integers. The values cannot exceed the width and height that you
-- specified in the @Video:Resolution@ object.
thumbnails_resolution :: Lens.Lens' Thumbnails (Prelude.Maybe Prelude.Text)
thumbnails_resolution = Lens.lens (\Thumbnails' {resolution} -> resolution) (\s@Thumbnails' {} a -> s {resolution = a} :: Thumbnails)

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

instance Data.FromJSON Thumbnails where
  parseJSON =
    Data.withObject
      "Thumbnails"
      ( \x ->
          Thumbnails'
            Prelude.<$> (x Data..:? "AspectRatio")
            Prelude.<*> (x Data..:? "Format")
            Prelude.<*> (x Data..:? "Interval")
            Prelude.<*> (x Data..:? "MaxHeight")
            Prelude.<*> (x Data..:? "MaxWidth")
            Prelude.<*> (x Data..:? "PaddingPolicy")
            Prelude.<*> (x Data..:? "Resolution")
            Prelude.<*> (x Data..:? "SizingPolicy")
      )

instance Prelude.Hashable Thumbnails where
  hashWithSalt _salt Thumbnails' {..} =
    _salt
      `Prelude.hashWithSalt` aspectRatio
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` interval
      `Prelude.hashWithSalt` maxHeight
      `Prelude.hashWithSalt` maxWidth
      `Prelude.hashWithSalt` paddingPolicy
      `Prelude.hashWithSalt` resolution
      `Prelude.hashWithSalt` sizingPolicy

instance Prelude.NFData Thumbnails where
  rnf Thumbnails' {..} =
    Prelude.rnf aspectRatio
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf interval
      `Prelude.seq` Prelude.rnf maxHeight
      `Prelude.seq` Prelude.rnf maxWidth
      `Prelude.seq` Prelude.rnf paddingPolicy
      `Prelude.seq` Prelude.rnf resolution
      `Prelude.seq` Prelude.rnf sizingPolicy

instance Data.ToJSON Thumbnails where
  toJSON Thumbnails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AspectRatio" Data..=) Prelude.<$> aspectRatio,
            ("Format" Data..=) Prelude.<$> format,
            ("Interval" Data..=) Prelude.<$> interval,
            ("MaxHeight" Data..=) Prelude.<$> maxHeight,
            ("MaxWidth" Data..=) Prelude.<$> maxWidth,
            ("PaddingPolicy" Data..=) Prelude.<$> paddingPolicy,
            ("Resolution" Data..=) Prelude.<$> resolution,
            ("SizingPolicy" Data..=) Prelude.<$> sizingPolicy
          ]
      )
