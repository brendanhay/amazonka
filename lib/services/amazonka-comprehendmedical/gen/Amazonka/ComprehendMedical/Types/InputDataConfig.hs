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
-- Module      : Amazonka.ComprehendMedical.Types.InputDataConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComprehendMedical.Types.InputDataConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The input properties for an entities detection job. This includes the
-- name of the S3 bucket and the path to the files to be analyzed.
--
-- /See:/ 'newInputDataConfig' smart constructor.
data InputDataConfig = InputDataConfig'
  { -- | The path to the input data files in the S3 bucket.
    s3Key :: Prelude.Maybe Prelude.Text,
    -- | The URI of the S3 bucket that contains the input data. The bucket must
    -- be in the same region as the API endpoint that you are calling.
    --
    -- Each file in the document collection must be less than 40 KB. You can
    -- store a maximum of 30 GB in the bucket.
    s3Bucket :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputDataConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Key', 'inputDataConfig_s3Key' - The path to the input data files in the S3 bucket.
--
-- 's3Bucket', 'inputDataConfig_s3Bucket' - The URI of the S3 bucket that contains the input data. The bucket must
-- be in the same region as the API endpoint that you are calling.
--
-- Each file in the document collection must be less than 40 KB. You can
-- store a maximum of 30 GB in the bucket.
newInputDataConfig ::
  -- | 's3Bucket'
  Prelude.Text ->
  InputDataConfig
newInputDataConfig pS3Bucket_ =
  InputDataConfig'
    { s3Key = Prelude.Nothing,
      s3Bucket = pS3Bucket_
    }

-- | The path to the input data files in the S3 bucket.
inputDataConfig_s3Key :: Lens.Lens' InputDataConfig (Prelude.Maybe Prelude.Text)
inputDataConfig_s3Key = Lens.lens (\InputDataConfig' {s3Key} -> s3Key) (\s@InputDataConfig' {} a -> s {s3Key = a} :: InputDataConfig)

-- | The URI of the S3 bucket that contains the input data. The bucket must
-- be in the same region as the API endpoint that you are calling.
--
-- Each file in the document collection must be less than 40 KB. You can
-- store a maximum of 30 GB in the bucket.
inputDataConfig_s3Bucket :: Lens.Lens' InputDataConfig Prelude.Text
inputDataConfig_s3Bucket = Lens.lens (\InputDataConfig' {s3Bucket} -> s3Bucket) (\s@InputDataConfig' {} a -> s {s3Bucket = a} :: InputDataConfig)

instance Core.FromJSON InputDataConfig where
  parseJSON =
    Core.withObject
      "InputDataConfig"
      ( \x ->
          InputDataConfig'
            Prelude.<$> (x Core..:? "S3Key")
            Prelude.<*> (x Core..: "S3Bucket")
      )

instance Prelude.Hashable InputDataConfig where
  hashWithSalt _salt InputDataConfig' {..} =
    _salt `Prelude.hashWithSalt` s3Key
      `Prelude.hashWithSalt` s3Bucket

instance Prelude.NFData InputDataConfig where
  rnf InputDataConfig' {..} =
    Prelude.rnf s3Key
      `Prelude.seq` Prelude.rnf s3Bucket

instance Core.ToJSON InputDataConfig where
  toJSON InputDataConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("S3Key" Core..=) Prelude.<$> s3Key,
            Prelude.Just ("S3Bucket" Core..= s3Bucket)
          ]
      )
