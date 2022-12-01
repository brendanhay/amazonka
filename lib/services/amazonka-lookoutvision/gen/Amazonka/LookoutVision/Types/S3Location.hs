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
-- Module      : Amazonka.LookoutVision.Types.S3Location
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutVision.Types.S3Location where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the location of training output or the output of a
-- model packaging job.
--
-- /See:/ 'newS3Location' smart constructor.
data S3Location = S3Location'
  { -- | The path of the folder, within the S3 bucket, that contains the output.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | The S3 bucket that contains the training or model packaging job output.
    -- If you are training a model, the bucket must in your AWS account. If you
    -- use an S3 bucket for a model packaging job, the S3 bucket must be in the
    -- same AWS Region and AWS account in which you use AWS IoT Greengrass.
    bucket :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3Location' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prefix', 's3Location_prefix' - The path of the folder, within the S3 bucket, that contains the output.
--
-- 'bucket', 's3Location_bucket' - The S3 bucket that contains the training or model packaging job output.
-- If you are training a model, the bucket must in your AWS account. If you
-- use an S3 bucket for a model packaging job, the S3 bucket must be in the
-- same AWS Region and AWS account in which you use AWS IoT Greengrass.
newS3Location ::
  -- | 'bucket'
  Prelude.Text ->
  S3Location
newS3Location pBucket_ =
  S3Location'
    { prefix = Prelude.Nothing,
      bucket = pBucket_
    }

-- | The path of the folder, within the S3 bucket, that contains the output.
s3Location_prefix :: Lens.Lens' S3Location (Prelude.Maybe Prelude.Text)
s3Location_prefix = Lens.lens (\S3Location' {prefix} -> prefix) (\s@S3Location' {} a -> s {prefix = a} :: S3Location)

-- | The S3 bucket that contains the training or model packaging job output.
-- If you are training a model, the bucket must in your AWS account. If you
-- use an S3 bucket for a model packaging job, the S3 bucket must be in the
-- same AWS Region and AWS account in which you use AWS IoT Greengrass.
s3Location_bucket :: Lens.Lens' S3Location Prelude.Text
s3Location_bucket = Lens.lens (\S3Location' {bucket} -> bucket) (\s@S3Location' {} a -> s {bucket = a} :: S3Location)

instance Core.FromJSON S3Location where
  parseJSON =
    Core.withObject
      "S3Location"
      ( \x ->
          S3Location'
            Prelude.<$> (x Core..:? "Prefix")
            Prelude.<*> (x Core..: "Bucket")
      )

instance Prelude.Hashable S3Location where
  hashWithSalt _salt S3Location' {..} =
    _salt `Prelude.hashWithSalt` prefix
      `Prelude.hashWithSalt` bucket

instance Prelude.NFData S3Location where
  rnf S3Location' {..} =
    Prelude.rnf prefix `Prelude.seq` Prelude.rnf bucket

instance Core.ToJSON S3Location where
  toJSON S3Location' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Prefix" Core..=) Prelude.<$> prefix,
            Prelude.Just ("Bucket" Core..= bucket)
          ]
      )
