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
-- Module      : Network.AWS.CodeBuild.Types.S3ReportExportConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.S3ReportExportConfig where

import Network.AWS.CodeBuild.Types.ReportPackagingType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about the S3 bucket where the raw data of a report are
-- exported.
--
-- /See:/ 'newS3ReportExportConfig' smart constructor.
data S3ReportExportConfig = S3ReportExportConfig'
  { -- | The AWS account identifier of the owner of the Amazon S3 bucket. This
    -- allows report data to be exported to an Amazon S3 bucket that is owned
    -- by an account other than the account running the build.
    bucketOwner :: Core.Maybe Core.Text,
    -- | The encryption key for the report\'s encrypted raw data.
    encryptionKey :: Core.Maybe Core.Text,
    -- | The type of build output artifact to create. Valid values include:
    --
    -- -   @NONE@: AWS CodeBuild creates the raw data in the output bucket.
    --     This is the default if packaging is not specified.
    --
    -- -   @ZIP@: AWS CodeBuild creates a ZIP file with the raw data in the
    --     output bucket.
    packaging :: Core.Maybe ReportPackagingType,
    -- | A boolean value that specifies if the results of a report are encrypted.
    encryptionDisabled :: Core.Maybe Core.Bool,
    -- | The name of the S3 bucket where the raw data of a report are exported.
    bucket :: Core.Maybe Core.Text,
    -- | The path to the exported report\'s raw data results.
    path :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'S3ReportExportConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketOwner', 's3ReportExportConfig_bucketOwner' - The AWS account identifier of the owner of the Amazon S3 bucket. This
-- allows report data to be exported to an Amazon S3 bucket that is owned
-- by an account other than the account running the build.
--
-- 'encryptionKey', 's3ReportExportConfig_encryptionKey' - The encryption key for the report\'s encrypted raw data.
--
-- 'packaging', 's3ReportExportConfig_packaging' - The type of build output artifact to create. Valid values include:
--
-- -   @NONE@: AWS CodeBuild creates the raw data in the output bucket.
--     This is the default if packaging is not specified.
--
-- -   @ZIP@: AWS CodeBuild creates a ZIP file with the raw data in the
--     output bucket.
--
-- 'encryptionDisabled', 's3ReportExportConfig_encryptionDisabled' - A boolean value that specifies if the results of a report are encrypted.
--
-- 'bucket', 's3ReportExportConfig_bucket' - The name of the S3 bucket where the raw data of a report are exported.
--
-- 'path', 's3ReportExportConfig_path' - The path to the exported report\'s raw data results.
newS3ReportExportConfig ::
  S3ReportExportConfig
newS3ReportExportConfig =
  S3ReportExportConfig'
    { bucketOwner = Core.Nothing,
      encryptionKey = Core.Nothing,
      packaging = Core.Nothing,
      encryptionDisabled = Core.Nothing,
      bucket = Core.Nothing,
      path = Core.Nothing
    }

-- | The AWS account identifier of the owner of the Amazon S3 bucket. This
-- allows report data to be exported to an Amazon S3 bucket that is owned
-- by an account other than the account running the build.
s3ReportExportConfig_bucketOwner :: Lens.Lens' S3ReportExportConfig (Core.Maybe Core.Text)
s3ReportExportConfig_bucketOwner = Lens.lens (\S3ReportExportConfig' {bucketOwner} -> bucketOwner) (\s@S3ReportExportConfig' {} a -> s {bucketOwner = a} :: S3ReportExportConfig)

-- | The encryption key for the report\'s encrypted raw data.
s3ReportExportConfig_encryptionKey :: Lens.Lens' S3ReportExportConfig (Core.Maybe Core.Text)
s3ReportExportConfig_encryptionKey = Lens.lens (\S3ReportExportConfig' {encryptionKey} -> encryptionKey) (\s@S3ReportExportConfig' {} a -> s {encryptionKey = a} :: S3ReportExportConfig)

-- | The type of build output artifact to create. Valid values include:
--
-- -   @NONE@: AWS CodeBuild creates the raw data in the output bucket.
--     This is the default if packaging is not specified.
--
-- -   @ZIP@: AWS CodeBuild creates a ZIP file with the raw data in the
--     output bucket.
s3ReportExportConfig_packaging :: Lens.Lens' S3ReportExportConfig (Core.Maybe ReportPackagingType)
s3ReportExportConfig_packaging = Lens.lens (\S3ReportExportConfig' {packaging} -> packaging) (\s@S3ReportExportConfig' {} a -> s {packaging = a} :: S3ReportExportConfig)

-- | A boolean value that specifies if the results of a report are encrypted.
s3ReportExportConfig_encryptionDisabled :: Lens.Lens' S3ReportExportConfig (Core.Maybe Core.Bool)
s3ReportExportConfig_encryptionDisabled = Lens.lens (\S3ReportExportConfig' {encryptionDisabled} -> encryptionDisabled) (\s@S3ReportExportConfig' {} a -> s {encryptionDisabled = a} :: S3ReportExportConfig)

-- | The name of the S3 bucket where the raw data of a report are exported.
s3ReportExportConfig_bucket :: Lens.Lens' S3ReportExportConfig (Core.Maybe Core.Text)
s3ReportExportConfig_bucket = Lens.lens (\S3ReportExportConfig' {bucket} -> bucket) (\s@S3ReportExportConfig' {} a -> s {bucket = a} :: S3ReportExportConfig)

-- | The path to the exported report\'s raw data results.
s3ReportExportConfig_path :: Lens.Lens' S3ReportExportConfig (Core.Maybe Core.Text)
s3ReportExportConfig_path = Lens.lens (\S3ReportExportConfig' {path} -> path) (\s@S3ReportExportConfig' {} a -> s {path = a} :: S3ReportExportConfig)

instance Core.FromJSON S3ReportExportConfig where
  parseJSON =
    Core.withObject
      "S3ReportExportConfig"
      ( \x ->
          S3ReportExportConfig'
            Core.<$> (x Core..:? "bucketOwner")
            Core.<*> (x Core..:? "encryptionKey")
            Core.<*> (x Core..:? "packaging")
            Core.<*> (x Core..:? "encryptionDisabled")
            Core.<*> (x Core..:? "bucket")
            Core.<*> (x Core..:? "path")
      )

instance Core.Hashable S3ReportExportConfig

instance Core.NFData S3ReportExportConfig

instance Core.ToJSON S3ReportExportConfig where
  toJSON S3ReportExportConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("bucketOwner" Core..=) Core.<$> bucketOwner,
            ("encryptionKey" Core..=) Core.<$> encryptionKey,
            ("packaging" Core..=) Core.<$> packaging,
            ("encryptionDisabled" Core..=)
              Core.<$> encryptionDisabled,
            ("bucket" Core..=) Core.<$> bucket,
            ("path" Core..=) Core.<$> path
          ]
      )
