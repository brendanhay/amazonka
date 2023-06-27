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
-- Module      : Amazonka.CustomerProfiles.Types.S3ExportingConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.S3ExportingConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configuration information about the S3 bucket where Identity Resolution
-- Jobs write result files.
--
-- /See:/ 'newS3ExportingConfig' smart constructor.
data S3ExportingConfig = S3ExportingConfig'
  { -- | The S3 key name of the location where Identity Resolution Jobs write
    -- result files.
    s3KeyName :: Prelude.Maybe Prelude.Text,
    -- | The name of the S3 bucket where Identity Resolution Jobs write result
    -- files.
    s3BucketName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3ExportingConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3KeyName', 's3ExportingConfig_s3KeyName' - The S3 key name of the location where Identity Resolution Jobs write
-- result files.
--
-- 's3BucketName', 's3ExportingConfig_s3BucketName' - The name of the S3 bucket where Identity Resolution Jobs write result
-- files.
newS3ExportingConfig ::
  -- | 's3BucketName'
  Prelude.Text ->
  S3ExportingConfig
newS3ExportingConfig pS3BucketName_ =
  S3ExportingConfig'
    { s3KeyName = Prelude.Nothing,
      s3BucketName = pS3BucketName_
    }

-- | The S3 key name of the location where Identity Resolution Jobs write
-- result files.
s3ExportingConfig_s3KeyName :: Lens.Lens' S3ExportingConfig (Prelude.Maybe Prelude.Text)
s3ExportingConfig_s3KeyName = Lens.lens (\S3ExportingConfig' {s3KeyName} -> s3KeyName) (\s@S3ExportingConfig' {} a -> s {s3KeyName = a} :: S3ExportingConfig)

-- | The name of the S3 bucket where Identity Resolution Jobs write result
-- files.
s3ExportingConfig_s3BucketName :: Lens.Lens' S3ExportingConfig Prelude.Text
s3ExportingConfig_s3BucketName = Lens.lens (\S3ExportingConfig' {s3BucketName} -> s3BucketName) (\s@S3ExportingConfig' {} a -> s {s3BucketName = a} :: S3ExportingConfig)

instance Data.FromJSON S3ExportingConfig where
  parseJSON =
    Data.withObject
      "S3ExportingConfig"
      ( \x ->
          S3ExportingConfig'
            Prelude.<$> (x Data..:? "S3KeyName")
            Prelude.<*> (x Data..: "S3BucketName")
      )

instance Prelude.Hashable S3ExportingConfig where
  hashWithSalt _salt S3ExportingConfig' {..} =
    _salt
      `Prelude.hashWithSalt` s3KeyName
      `Prelude.hashWithSalt` s3BucketName

instance Prelude.NFData S3ExportingConfig where
  rnf S3ExportingConfig' {..} =
    Prelude.rnf s3KeyName
      `Prelude.seq` Prelude.rnf s3BucketName

instance Data.ToJSON S3ExportingConfig where
  toJSON S3ExportingConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("S3KeyName" Data..=) Prelude.<$> s3KeyName,
            Prelude.Just ("S3BucketName" Data..= s3BucketName)
          ]
      )
