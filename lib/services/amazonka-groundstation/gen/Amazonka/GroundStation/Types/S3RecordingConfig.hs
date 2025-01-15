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
-- Module      : Amazonka.GroundStation.Types.S3RecordingConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.S3RecordingConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about an S3 recording @Config@.
--
-- /See:/ 'newS3RecordingConfig' smart constructor.
data S3RecordingConfig = S3RecordingConfig'
  { -- | S3 Key prefix to prefice data files.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | ARN of the bucket to record to.
    bucketArn :: Prelude.Text,
    -- | ARN of the role Ground Station assumes to write data to the bucket.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3RecordingConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prefix', 's3RecordingConfig_prefix' - S3 Key prefix to prefice data files.
--
-- 'bucketArn', 's3RecordingConfig_bucketArn' - ARN of the bucket to record to.
--
-- 'roleArn', 's3RecordingConfig_roleArn' - ARN of the role Ground Station assumes to write data to the bucket.
newS3RecordingConfig ::
  -- | 'bucketArn'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  S3RecordingConfig
newS3RecordingConfig pBucketArn_ pRoleArn_ =
  S3RecordingConfig'
    { prefix = Prelude.Nothing,
      bucketArn = pBucketArn_,
      roleArn = pRoleArn_
    }

-- | S3 Key prefix to prefice data files.
s3RecordingConfig_prefix :: Lens.Lens' S3RecordingConfig (Prelude.Maybe Prelude.Text)
s3RecordingConfig_prefix = Lens.lens (\S3RecordingConfig' {prefix} -> prefix) (\s@S3RecordingConfig' {} a -> s {prefix = a} :: S3RecordingConfig)

-- | ARN of the bucket to record to.
s3RecordingConfig_bucketArn :: Lens.Lens' S3RecordingConfig Prelude.Text
s3RecordingConfig_bucketArn = Lens.lens (\S3RecordingConfig' {bucketArn} -> bucketArn) (\s@S3RecordingConfig' {} a -> s {bucketArn = a} :: S3RecordingConfig)

-- | ARN of the role Ground Station assumes to write data to the bucket.
s3RecordingConfig_roleArn :: Lens.Lens' S3RecordingConfig Prelude.Text
s3RecordingConfig_roleArn = Lens.lens (\S3RecordingConfig' {roleArn} -> roleArn) (\s@S3RecordingConfig' {} a -> s {roleArn = a} :: S3RecordingConfig)

instance Data.FromJSON S3RecordingConfig where
  parseJSON =
    Data.withObject
      "S3RecordingConfig"
      ( \x ->
          S3RecordingConfig'
            Prelude.<$> (x Data..:? "prefix")
            Prelude.<*> (x Data..: "bucketArn")
            Prelude.<*> (x Data..: "roleArn")
      )

instance Prelude.Hashable S3RecordingConfig where
  hashWithSalt _salt S3RecordingConfig' {..} =
    _salt
      `Prelude.hashWithSalt` prefix
      `Prelude.hashWithSalt` bucketArn
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData S3RecordingConfig where
  rnf S3RecordingConfig' {..} =
    Prelude.rnf prefix `Prelude.seq`
      Prelude.rnf bucketArn `Prelude.seq`
        Prelude.rnf roleArn

instance Data.ToJSON S3RecordingConfig where
  toJSON S3RecordingConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("prefix" Data..=) Prelude.<$> prefix,
            Prelude.Just ("bucketArn" Data..= bucketArn),
            Prelude.Just ("roleArn" Data..= roleArn)
          ]
      )
