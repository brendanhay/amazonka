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
-- Module      : Amazonka.ComputeOptimizer.Types.S3DestinationConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.S3DestinationConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the destination Amazon Simple Storage Service (Amazon S3)
-- bucket name and key prefix for a recommendations export job.
--
-- You must create the destination Amazon S3 bucket for your
-- recommendations export before you create the export job. Compute
-- Optimizer does not create the S3 bucket for you. After you create the S3
-- bucket, ensure that it has the required permission policy to allow
-- Compute Optimizer to write the export file to it. If you plan to specify
-- an object prefix when you create the export job, you must include the
-- object prefix in the policy that you add to the S3 bucket. For more
-- information, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/create-s3-bucket-policy-for-compute-optimizer.html Amazon S3 Bucket Policy for Compute Optimizer>
-- in the /Compute Optimizer User Guide/.
--
-- /See:/ 'newS3DestinationConfig' smart constructor.
data S3DestinationConfig = S3DestinationConfig'
  { -- | The name of the Amazon S3 bucket to use as the destination for an export
    -- job.
    bucket :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 bucket prefix for an export job.
    keyPrefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3DestinationConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucket', 's3DestinationConfig_bucket' - The name of the Amazon S3 bucket to use as the destination for an export
-- job.
--
-- 'keyPrefix', 's3DestinationConfig_keyPrefix' - The Amazon S3 bucket prefix for an export job.
newS3DestinationConfig ::
  S3DestinationConfig
newS3DestinationConfig =
  S3DestinationConfig'
    { bucket = Prelude.Nothing,
      keyPrefix = Prelude.Nothing
    }

-- | The name of the Amazon S3 bucket to use as the destination for an export
-- job.
s3DestinationConfig_bucket :: Lens.Lens' S3DestinationConfig (Prelude.Maybe Prelude.Text)
s3DestinationConfig_bucket = Lens.lens (\S3DestinationConfig' {bucket} -> bucket) (\s@S3DestinationConfig' {} a -> s {bucket = a} :: S3DestinationConfig)

-- | The Amazon S3 bucket prefix for an export job.
s3DestinationConfig_keyPrefix :: Lens.Lens' S3DestinationConfig (Prelude.Maybe Prelude.Text)
s3DestinationConfig_keyPrefix = Lens.lens (\S3DestinationConfig' {keyPrefix} -> keyPrefix) (\s@S3DestinationConfig' {} a -> s {keyPrefix = a} :: S3DestinationConfig)

instance Prelude.Hashable S3DestinationConfig where
  hashWithSalt _salt S3DestinationConfig' {..} =
    _salt
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` keyPrefix

instance Prelude.NFData S3DestinationConfig where
  rnf S3DestinationConfig' {..} =
    Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf keyPrefix

instance Data.ToJSON S3DestinationConfig where
  toJSON S3DestinationConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("bucket" Data..=) Prelude.<$> bucket,
            ("keyPrefix" Data..=) Prelude.<$> keyPrefix
          ]
      )
