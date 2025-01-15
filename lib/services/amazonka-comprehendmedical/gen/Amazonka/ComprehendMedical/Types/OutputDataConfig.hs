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
-- Module      : Amazonka.ComprehendMedical.Types.OutputDataConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComprehendMedical.Types.OutputDataConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The output properties for a detection job.
--
-- /See:/ 'newOutputDataConfig' smart constructor.
data OutputDataConfig = OutputDataConfig'
  { -- | The path to the output data files in the S3 bucket. Comprehend Medical;
    -- creates an output directory using the job ID so that the output from one
    -- job does not overwrite the output of another.
    s3Key :: Prelude.Maybe Prelude.Text,
    -- | When you use the @OutputDataConfig@ object with asynchronous operations,
    -- you specify the Amazon S3 location where you want to write the output
    -- data. The URI must be in the same region as the API endpoint that you
    -- are calling. The location is used as the prefix for the actual location
    -- of the output.
    s3Bucket :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OutputDataConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Key', 'outputDataConfig_s3Key' - The path to the output data files in the S3 bucket. Comprehend Medical;
-- creates an output directory using the job ID so that the output from one
-- job does not overwrite the output of another.
--
-- 's3Bucket', 'outputDataConfig_s3Bucket' - When you use the @OutputDataConfig@ object with asynchronous operations,
-- you specify the Amazon S3 location where you want to write the output
-- data. The URI must be in the same region as the API endpoint that you
-- are calling. The location is used as the prefix for the actual location
-- of the output.
newOutputDataConfig ::
  -- | 's3Bucket'
  Prelude.Text ->
  OutputDataConfig
newOutputDataConfig pS3Bucket_ =
  OutputDataConfig'
    { s3Key = Prelude.Nothing,
      s3Bucket = pS3Bucket_
    }

-- | The path to the output data files in the S3 bucket. Comprehend Medical;
-- creates an output directory using the job ID so that the output from one
-- job does not overwrite the output of another.
outputDataConfig_s3Key :: Lens.Lens' OutputDataConfig (Prelude.Maybe Prelude.Text)
outputDataConfig_s3Key = Lens.lens (\OutputDataConfig' {s3Key} -> s3Key) (\s@OutputDataConfig' {} a -> s {s3Key = a} :: OutputDataConfig)

-- | When you use the @OutputDataConfig@ object with asynchronous operations,
-- you specify the Amazon S3 location where you want to write the output
-- data. The URI must be in the same region as the API endpoint that you
-- are calling. The location is used as the prefix for the actual location
-- of the output.
outputDataConfig_s3Bucket :: Lens.Lens' OutputDataConfig Prelude.Text
outputDataConfig_s3Bucket = Lens.lens (\OutputDataConfig' {s3Bucket} -> s3Bucket) (\s@OutputDataConfig' {} a -> s {s3Bucket = a} :: OutputDataConfig)

instance Data.FromJSON OutputDataConfig where
  parseJSON =
    Data.withObject
      "OutputDataConfig"
      ( \x ->
          OutputDataConfig'
            Prelude.<$> (x Data..:? "S3Key")
            Prelude.<*> (x Data..: "S3Bucket")
      )

instance Prelude.Hashable OutputDataConfig where
  hashWithSalt _salt OutputDataConfig' {..} =
    _salt
      `Prelude.hashWithSalt` s3Key
      `Prelude.hashWithSalt` s3Bucket

instance Prelude.NFData OutputDataConfig where
  rnf OutputDataConfig' {..} =
    Prelude.rnf s3Key `Prelude.seq`
      Prelude.rnf s3Bucket

instance Data.ToJSON OutputDataConfig where
  toJSON OutputDataConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("S3Key" Data..=) Prelude.<$> s3Key,
            Prelude.Just ("S3Bucket" Data..= s3Bucket)
          ]
      )
