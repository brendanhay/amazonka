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
-- Module      : Network.AWS.LookoutMetrics.Types.S3SourceConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LookoutMetrics.Types.S3SourceConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LookoutMetrics.Types.FileFormatDescriptor
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about the configuration of the S3 bucket that
-- contains source files.
--
-- /See:/ 'newS3SourceConfig' smart constructor.
data S3SourceConfig = S3SourceConfig'
  { -- | A list of templated paths to the source files.
    templatedPathList :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A list of paths to the historical data files.
    historicalDataPathList :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Contains information about a source file\'s formatting.
    fileFormatDescriptor :: Prelude.Maybe FileFormatDescriptor,
    -- | The ARN of an IAM role that has read and write access permissions to the
    -- source S3 bucket.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3SourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templatedPathList', 's3SourceConfig_templatedPathList' - A list of templated paths to the source files.
--
-- 'historicalDataPathList', 's3SourceConfig_historicalDataPathList' - A list of paths to the historical data files.
--
-- 'fileFormatDescriptor', 's3SourceConfig_fileFormatDescriptor' - Contains information about a source file\'s formatting.
--
-- 'roleArn', 's3SourceConfig_roleArn' - The ARN of an IAM role that has read and write access permissions to the
-- source S3 bucket.
newS3SourceConfig ::
  -- | 'roleArn'
  Prelude.Text ->
  S3SourceConfig
newS3SourceConfig pRoleArn_ =
  S3SourceConfig'
    { templatedPathList =
        Prelude.Nothing,
      historicalDataPathList = Prelude.Nothing,
      fileFormatDescriptor = Prelude.Nothing,
      roleArn = pRoleArn_
    }

-- | A list of templated paths to the source files.
s3SourceConfig_templatedPathList :: Lens.Lens' S3SourceConfig (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
s3SourceConfig_templatedPathList = Lens.lens (\S3SourceConfig' {templatedPathList} -> templatedPathList) (\s@S3SourceConfig' {} a -> s {templatedPathList = a} :: S3SourceConfig) Prelude.. Lens.mapping Lens.coerced

-- | A list of paths to the historical data files.
s3SourceConfig_historicalDataPathList :: Lens.Lens' S3SourceConfig (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
s3SourceConfig_historicalDataPathList = Lens.lens (\S3SourceConfig' {historicalDataPathList} -> historicalDataPathList) (\s@S3SourceConfig' {} a -> s {historicalDataPathList = a} :: S3SourceConfig) Prelude.. Lens.mapping Lens.coerced

-- | Contains information about a source file\'s formatting.
s3SourceConfig_fileFormatDescriptor :: Lens.Lens' S3SourceConfig (Prelude.Maybe FileFormatDescriptor)
s3SourceConfig_fileFormatDescriptor = Lens.lens (\S3SourceConfig' {fileFormatDescriptor} -> fileFormatDescriptor) (\s@S3SourceConfig' {} a -> s {fileFormatDescriptor = a} :: S3SourceConfig)

-- | The ARN of an IAM role that has read and write access permissions to the
-- source S3 bucket.
s3SourceConfig_roleArn :: Lens.Lens' S3SourceConfig Prelude.Text
s3SourceConfig_roleArn = Lens.lens (\S3SourceConfig' {roleArn} -> roleArn) (\s@S3SourceConfig' {} a -> s {roleArn = a} :: S3SourceConfig)

instance Core.FromJSON S3SourceConfig where
  parseJSON =
    Core.withObject
      "S3SourceConfig"
      ( \x ->
          S3SourceConfig'
            Prelude.<$> (x Core..:? "TemplatedPathList")
            Prelude.<*> (x Core..:? "HistoricalDataPathList")
            Prelude.<*> (x Core..:? "FileFormatDescriptor")
            Prelude.<*> (x Core..: "RoleArn")
      )

instance Prelude.Hashable S3SourceConfig

instance Prelude.NFData S3SourceConfig

instance Core.ToJSON S3SourceConfig where
  toJSON S3SourceConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TemplatedPathList" Core..=)
              Prelude.<$> templatedPathList,
            ("HistoricalDataPathList" Core..=)
              Prelude.<$> historicalDataPathList,
            ("FileFormatDescriptor" Core..=)
              Prelude.<$> fileFormatDescriptor,
            Prelude.Just ("RoleArn" Core..= roleArn)
          ]
      )
