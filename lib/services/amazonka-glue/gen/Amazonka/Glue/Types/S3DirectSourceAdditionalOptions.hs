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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies additional connection options for the Amazon S3 data store.
--
-- /See:/ 'newS3DirectSourceAdditionalOptions' smart constructor.
data S3DirectSourceAdditionalOptions = S3DirectSourceAdditionalOptions'
  { -- | Sets the upper limit for the target number of files that will be
    -- processed.
    boundedFiles :: Prelude.Maybe Prelude.Integer,
    -- | Sets the upper limit for the target size of the dataset in bytes that
    -- will be processed.
    boundedSize :: Prelude.Maybe Prelude.Integer,
    -- | Sets option to enable a sample path.
    enableSamplePath :: Prelude.Maybe Prelude.Bool,
    -- | If enabled, specifies the sample path.
    samplePath :: Prelude.Maybe Prelude.Text
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
-- 'boundedFiles', 's3DirectSourceAdditionalOptions_boundedFiles' - Sets the upper limit for the target number of files that will be
-- processed.
--
-- 'boundedSize', 's3DirectSourceAdditionalOptions_boundedSize' - Sets the upper limit for the target size of the dataset in bytes that
-- will be processed.
--
-- 'enableSamplePath', 's3DirectSourceAdditionalOptions_enableSamplePath' - Sets option to enable a sample path.
--
-- 'samplePath', 's3DirectSourceAdditionalOptions_samplePath' - If enabled, specifies the sample path.
newS3DirectSourceAdditionalOptions ::
  S3DirectSourceAdditionalOptions
newS3DirectSourceAdditionalOptions =
  S3DirectSourceAdditionalOptions'
    { boundedFiles =
        Prelude.Nothing,
      boundedSize = Prelude.Nothing,
      enableSamplePath = Prelude.Nothing,
      samplePath = Prelude.Nothing
    }

-- | Sets the upper limit for the target number of files that will be
-- processed.
s3DirectSourceAdditionalOptions_boundedFiles :: Lens.Lens' S3DirectSourceAdditionalOptions (Prelude.Maybe Prelude.Integer)
s3DirectSourceAdditionalOptions_boundedFiles = Lens.lens (\S3DirectSourceAdditionalOptions' {boundedFiles} -> boundedFiles) (\s@S3DirectSourceAdditionalOptions' {} a -> s {boundedFiles = a} :: S3DirectSourceAdditionalOptions)

-- | Sets the upper limit for the target size of the dataset in bytes that
-- will be processed.
s3DirectSourceAdditionalOptions_boundedSize :: Lens.Lens' S3DirectSourceAdditionalOptions (Prelude.Maybe Prelude.Integer)
s3DirectSourceAdditionalOptions_boundedSize = Lens.lens (\S3DirectSourceAdditionalOptions' {boundedSize} -> boundedSize) (\s@S3DirectSourceAdditionalOptions' {} a -> s {boundedSize = a} :: S3DirectSourceAdditionalOptions)

-- | Sets option to enable a sample path.
s3DirectSourceAdditionalOptions_enableSamplePath :: Lens.Lens' S3DirectSourceAdditionalOptions (Prelude.Maybe Prelude.Bool)
s3DirectSourceAdditionalOptions_enableSamplePath = Lens.lens (\S3DirectSourceAdditionalOptions' {enableSamplePath} -> enableSamplePath) (\s@S3DirectSourceAdditionalOptions' {} a -> s {enableSamplePath = a} :: S3DirectSourceAdditionalOptions)

-- | If enabled, specifies the sample path.
s3DirectSourceAdditionalOptions_samplePath :: Lens.Lens' S3DirectSourceAdditionalOptions (Prelude.Maybe Prelude.Text)
s3DirectSourceAdditionalOptions_samplePath = Lens.lens (\S3DirectSourceAdditionalOptions' {samplePath} -> samplePath) (\s@S3DirectSourceAdditionalOptions' {} a -> s {samplePath = a} :: S3DirectSourceAdditionalOptions)

instance
  Data.FromJSON
    S3DirectSourceAdditionalOptions
  where
  parseJSON =
    Data.withObject
      "S3DirectSourceAdditionalOptions"
      ( \x ->
          S3DirectSourceAdditionalOptions'
            Prelude.<$> (x Data..:? "BoundedFiles")
            Prelude.<*> (x Data..:? "BoundedSize")
            Prelude.<*> (x Data..:? "EnableSamplePath")
            Prelude.<*> (x Data..:? "SamplePath")
      )

instance
  Prelude.Hashable
    S3DirectSourceAdditionalOptions
  where
  hashWithSalt
    _salt
    S3DirectSourceAdditionalOptions' {..} =
      _salt `Prelude.hashWithSalt` boundedFiles
        `Prelude.hashWithSalt` boundedSize
        `Prelude.hashWithSalt` enableSamplePath
        `Prelude.hashWithSalt` samplePath

instance
  Prelude.NFData
    S3DirectSourceAdditionalOptions
  where
  rnf S3DirectSourceAdditionalOptions' {..} =
    Prelude.rnf boundedFiles
      `Prelude.seq` Prelude.rnf boundedSize
      `Prelude.seq` Prelude.rnf enableSamplePath
      `Prelude.seq` Prelude.rnf samplePath

instance Data.ToJSON S3DirectSourceAdditionalOptions where
  toJSON S3DirectSourceAdditionalOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BoundedFiles" Data..=) Prelude.<$> boundedFiles,
            ("BoundedSize" Data..=) Prelude.<$> boundedSize,
            ("EnableSamplePath" Data..=)
              Prelude.<$> enableSamplePath,
            ("SamplePath" Data..=) Prelude.<$> samplePath
          ]
      )
