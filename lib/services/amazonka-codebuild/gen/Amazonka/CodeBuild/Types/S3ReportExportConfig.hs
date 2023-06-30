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
-- Module      : Amazonka.CodeBuild.Types.S3ReportExportConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.S3ReportExportConfig where

import Amazonka.CodeBuild.Types.ReportPackagingType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the S3 bucket where the raw data of a report are
-- exported.
--
-- /See:/ 'newS3ReportExportConfig' smart constructor.
data S3ReportExportConfig = S3ReportExportConfig'
  { -- | The name of the S3 bucket where the raw data of a report are exported.
    bucket :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account identifier of the owner of the Amazon S3
    -- bucket. This allows report data to be exported to an Amazon S3 bucket
    -- that is owned by an account other than the account running the build.
    bucketOwner :: Prelude.Maybe Prelude.Text,
    -- | A boolean value that specifies if the results of a report are encrypted.
    encryptionDisabled :: Prelude.Maybe Prelude.Bool,
    -- | The encryption key for the report\'s encrypted raw data.
    encryptionKey :: Prelude.Maybe Prelude.Text,
    -- | The type of build output artifact to create. Valid values include:
    --
    -- -   @NONE@: CodeBuild creates the raw data in the output bucket. This is
    --     the default if packaging is not specified.
    --
    -- -   @ZIP@: CodeBuild creates a ZIP file with the raw data in the output
    --     bucket.
    packaging :: Prelude.Maybe ReportPackagingType,
    -- | The path to the exported report\'s raw data results.
    path :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3ReportExportConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucket', 's3ReportExportConfig_bucket' - The name of the S3 bucket where the raw data of a report are exported.
--
-- 'bucketOwner', 's3ReportExportConfig_bucketOwner' - The Amazon Web Services account identifier of the owner of the Amazon S3
-- bucket. This allows report data to be exported to an Amazon S3 bucket
-- that is owned by an account other than the account running the build.
--
-- 'encryptionDisabled', 's3ReportExportConfig_encryptionDisabled' - A boolean value that specifies if the results of a report are encrypted.
--
-- 'encryptionKey', 's3ReportExportConfig_encryptionKey' - The encryption key for the report\'s encrypted raw data.
--
-- 'packaging', 's3ReportExportConfig_packaging' - The type of build output artifact to create. Valid values include:
--
-- -   @NONE@: CodeBuild creates the raw data in the output bucket. This is
--     the default if packaging is not specified.
--
-- -   @ZIP@: CodeBuild creates a ZIP file with the raw data in the output
--     bucket.
--
-- 'path', 's3ReportExportConfig_path' - The path to the exported report\'s raw data results.
newS3ReportExportConfig ::
  S3ReportExportConfig
newS3ReportExportConfig =
  S3ReportExportConfig'
    { bucket = Prelude.Nothing,
      bucketOwner = Prelude.Nothing,
      encryptionDisabled = Prelude.Nothing,
      encryptionKey = Prelude.Nothing,
      packaging = Prelude.Nothing,
      path = Prelude.Nothing
    }

-- | The name of the S3 bucket where the raw data of a report are exported.
s3ReportExportConfig_bucket :: Lens.Lens' S3ReportExportConfig (Prelude.Maybe Prelude.Text)
s3ReportExportConfig_bucket = Lens.lens (\S3ReportExportConfig' {bucket} -> bucket) (\s@S3ReportExportConfig' {} a -> s {bucket = a} :: S3ReportExportConfig)

-- | The Amazon Web Services account identifier of the owner of the Amazon S3
-- bucket. This allows report data to be exported to an Amazon S3 bucket
-- that is owned by an account other than the account running the build.
s3ReportExportConfig_bucketOwner :: Lens.Lens' S3ReportExportConfig (Prelude.Maybe Prelude.Text)
s3ReportExportConfig_bucketOwner = Lens.lens (\S3ReportExportConfig' {bucketOwner} -> bucketOwner) (\s@S3ReportExportConfig' {} a -> s {bucketOwner = a} :: S3ReportExportConfig)

-- | A boolean value that specifies if the results of a report are encrypted.
s3ReportExportConfig_encryptionDisabled :: Lens.Lens' S3ReportExportConfig (Prelude.Maybe Prelude.Bool)
s3ReportExportConfig_encryptionDisabled = Lens.lens (\S3ReportExportConfig' {encryptionDisabled} -> encryptionDisabled) (\s@S3ReportExportConfig' {} a -> s {encryptionDisabled = a} :: S3ReportExportConfig)

-- | The encryption key for the report\'s encrypted raw data.
s3ReportExportConfig_encryptionKey :: Lens.Lens' S3ReportExportConfig (Prelude.Maybe Prelude.Text)
s3ReportExportConfig_encryptionKey = Lens.lens (\S3ReportExportConfig' {encryptionKey} -> encryptionKey) (\s@S3ReportExportConfig' {} a -> s {encryptionKey = a} :: S3ReportExportConfig)

-- | The type of build output artifact to create. Valid values include:
--
-- -   @NONE@: CodeBuild creates the raw data in the output bucket. This is
--     the default if packaging is not specified.
--
-- -   @ZIP@: CodeBuild creates a ZIP file with the raw data in the output
--     bucket.
s3ReportExportConfig_packaging :: Lens.Lens' S3ReportExportConfig (Prelude.Maybe ReportPackagingType)
s3ReportExportConfig_packaging = Lens.lens (\S3ReportExportConfig' {packaging} -> packaging) (\s@S3ReportExportConfig' {} a -> s {packaging = a} :: S3ReportExportConfig)

-- | The path to the exported report\'s raw data results.
s3ReportExportConfig_path :: Lens.Lens' S3ReportExportConfig (Prelude.Maybe Prelude.Text)
s3ReportExportConfig_path = Lens.lens (\S3ReportExportConfig' {path} -> path) (\s@S3ReportExportConfig' {} a -> s {path = a} :: S3ReportExportConfig)

instance Data.FromJSON S3ReportExportConfig where
  parseJSON =
    Data.withObject
      "S3ReportExportConfig"
      ( \x ->
          S3ReportExportConfig'
            Prelude.<$> (x Data..:? "bucket")
            Prelude.<*> (x Data..:? "bucketOwner")
            Prelude.<*> (x Data..:? "encryptionDisabled")
            Prelude.<*> (x Data..:? "encryptionKey")
            Prelude.<*> (x Data..:? "packaging")
            Prelude.<*> (x Data..:? "path")
      )

instance Prelude.Hashable S3ReportExportConfig where
  hashWithSalt _salt S3ReportExportConfig' {..} =
    _salt
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` bucketOwner
      `Prelude.hashWithSalt` encryptionDisabled
      `Prelude.hashWithSalt` encryptionKey
      `Prelude.hashWithSalt` packaging
      `Prelude.hashWithSalt` path

instance Prelude.NFData S3ReportExportConfig where
  rnf S3ReportExportConfig' {..} =
    Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf bucketOwner
      `Prelude.seq` Prelude.rnf encryptionDisabled
      `Prelude.seq` Prelude.rnf encryptionKey
      `Prelude.seq` Prelude.rnf packaging
      `Prelude.seq` Prelude.rnf path

instance Data.ToJSON S3ReportExportConfig where
  toJSON S3ReportExportConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("bucket" Data..=) Prelude.<$> bucket,
            ("bucketOwner" Data..=) Prelude.<$> bucketOwner,
            ("encryptionDisabled" Data..=)
              Prelude.<$> encryptionDisabled,
            ("encryptionKey" Data..=) Prelude.<$> encryptionKey,
            ("packaging" Data..=) Prelude.<$> packaging,
            ("path" Data..=) Prelude.<$> path
          ]
      )
