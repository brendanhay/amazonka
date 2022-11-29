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
-- Module      : Amazonka.Glue.Types.S3DirectSourceAdditionalOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.S3DirectSourceAdditionalOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies additional connection options for the Amazon S3 data store.
--
-- /See:/ 'newS3DirectSourceAdditionalOptions' smart constructor.
data S3DirectSourceAdditionalOptions = S3DirectSourceAdditionalOptions'
  { -- | If enabled, specifies the sample path.
    samplePath :: Prelude.Maybe Prelude.Text,
    -- | Sets option to enable a sample path.
    enableSamplePath :: Prelude.Maybe Prelude.Bool,
    -- | Sets the upper limit for the target size of the dataset in bytes that
    -- will be processed.
    boundedSize :: Prelude.Maybe Prelude.Integer,
    -- | Sets the upper limit for the target number of files that will be
    -- processed.
    boundedFiles :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3DirectSourceAdditionalOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'samplePath', 's3DirectSourceAdditionalOptions_samplePath' - If enabled, specifies the sample path.
--
-- 'enableSamplePath', 's3DirectSourceAdditionalOptions_enableSamplePath' - Sets option to enable a sample path.
--
-- 'boundedSize', 's3DirectSourceAdditionalOptions_boundedSize' - Sets the upper limit for the target size of the dataset in bytes that
-- will be processed.
--
-- 'boundedFiles', 's3DirectSourceAdditionalOptions_boundedFiles' - Sets the upper limit for the target number of files that will be
-- processed.
newS3DirectSourceAdditionalOptions ::
  S3DirectSourceAdditionalOptions
newS3DirectSourceAdditionalOptions =
  S3DirectSourceAdditionalOptions'
    { samplePath =
        Prelude.Nothing,
      enableSamplePath = Prelude.Nothing,
      boundedSize = Prelude.Nothing,
      boundedFiles = Prelude.Nothing
    }

-- | If enabled, specifies the sample path.
s3DirectSourceAdditionalOptions_samplePath :: Lens.Lens' S3DirectSourceAdditionalOptions (Prelude.Maybe Prelude.Text)
s3DirectSourceAdditionalOptions_samplePath = Lens.lens (\S3DirectSourceAdditionalOptions' {samplePath} -> samplePath) (\s@S3DirectSourceAdditionalOptions' {} a -> s {samplePath = a} :: S3DirectSourceAdditionalOptions)

-- | Sets option to enable a sample path.
s3DirectSourceAdditionalOptions_enableSamplePath :: Lens.Lens' S3DirectSourceAdditionalOptions (Prelude.Maybe Prelude.Bool)
s3DirectSourceAdditionalOptions_enableSamplePath = Lens.lens (\S3DirectSourceAdditionalOptions' {enableSamplePath} -> enableSamplePath) (\s@S3DirectSourceAdditionalOptions' {} a -> s {enableSamplePath = a} :: S3DirectSourceAdditionalOptions)

-- | Sets the upper limit for the target size of the dataset in bytes that
-- will be processed.
s3DirectSourceAdditionalOptions_boundedSize :: Lens.Lens' S3DirectSourceAdditionalOptions (Prelude.Maybe Prelude.Integer)
s3DirectSourceAdditionalOptions_boundedSize = Lens.lens (\S3DirectSourceAdditionalOptions' {boundedSize} -> boundedSize) (\s@S3DirectSourceAdditionalOptions' {} a -> s {boundedSize = a} :: S3DirectSourceAdditionalOptions)

-- | Sets the upper limit for the target number of files that will be
-- processed.
s3DirectSourceAdditionalOptions_boundedFiles :: Lens.Lens' S3DirectSourceAdditionalOptions (Prelude.Maybe Prelude.Integer)
s3DirectSourceAdditionalOptions_boundedFiles = Lens.lens (\S3DirectSourceAdditionalOptions' {boundedFiles} -> boundedFiles) (\s@S3DirectSourceAdditionalOptions' {} a -> s {boundedFiles = a} :: S3DirectSourceAdditionalOptions)

instance
  Core.FromJSON
    S3DirectSourceAdditionalOptions
  where
  parseJSON =
    Core.withObject
      "S3DirectSourceAdditionalOptions"
      ( \x ->
          S3DirectSourceAdditionalOptions'
            Prelude.<$> (x Core..:? "SamplePath")
            Prelude.<*> (x Core..:? "EnableSamplePath")
            Prelude.<*> (x Core..:? "BoundedSize")
            Prelude.<*> (x Core..:? "BoundedFiles")
      )

instance
  Prelude.Hashable
    S3DirectSourceAdditionalOptions
  where
  hashWithSalt
    _salt
    S3DirectSourceAdditionalOptions' {..} =
      _salt `Prelude.hashWithSalt` samplePath
        `Prelude.hashWithSalt` enableSamplePath
        `Prelude.hashWithSalt` boundedSize
        `Prelude.hashWithSalt` boundedFiles

instance
  Prelude.NFData
    S3DirectSourceAdditionalOptions
  where
  rnf S3DirectSourceAdditionalOptions' {..} =
    Prelude.rnf samplePath
      `Prelude.seq` Prelude.rnf enableSamplePath
      `Prelude.seq` Prelude.rnf boundedSize
      `Prelude.seq` Prelude.rnf boundedFiles

instance Core.ToJSON S3DirectSourceAdditionalOptions where
  toJSON S3DirectSourceAdditionalOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SamplePath" Core..=) Prelude.<$> samplePath,
            ("EnableSamplePath" Core..=)
              Prelude.<$> enableSamplePath,
            ("BoundedSize" Core..=) Prelude.<$> boundedSize,
            ("BoundedFiles" Core..=) Prelude.<$> boundedFiles
          ]
      )
