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
-- Module      : Amazonka.LexV2Models.Types.S3BucketLogDestination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.S3BucketLogDestination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies an Amazon S3 bucket for logging audio conversations
--
-- /See:/ 'newS3BucketLogDestination' smart constructor.
data S3BucketLogDestination = S3BucketLogDestination'
  { -- | The Amazon Resource Name (ARN) of an AWS Key Management Service (KMS)
    -- key for encrypting audio log files stored in an S3 bucket.
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an Amazon S3 bucket where audio log
    -- files are stored.
    s3BucketArn :: Prelude.Text,
    -- | The S3 prefix to assign to audio log files.
    logPrefix :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3BucketLogDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyArn', 's3BucketLogDestination_kmsKeyArn' - The Amazon Resource Name (ARN) of an AWS Key Management Service (KMS)
-- key for encrypting audio log files stored in an S3 bucket.
--
-- 's3BucketArn', 's3BucketLogDestination_s3BucketArn' - The Amazon Resource Name (ARN) of an Amazon S3 bucket where audio log
-- files are stored.
--
-- 'logPrefix', 's3BucketLogDestination_logPrefix' - The S3 prefix to assign to audio log files.
newS3BucketLogDestination ::
  -- | 's3BucketArn'
  Prelude.Text ->
  -- | 'logPrefix'
  Prelude.Text ->
  S3BucketLogDestination
newS3BucketLogDestination pS3BucketArn_ pLogPrefix_ =
  S3BucketLogDestination'
    { kmsKeyArn =
        Prelude.Nothing,
      s3BucketArn = pS3BucketArn_,
      logPrefix = pLogPrefix_
    }

-- | The Amazon Resource Name (ARN) of an AWS Key Management Service (KMS)
-- key for encrypting audio log files stored in an S3 bucket.
s3BucketLogDestination_kmsKeyArn :: Lens.Lens' S3BucketLogDestination (Prelude.Maybe Prelude.Text)
s3BucketLogDestination_kmsKeyArn = Lens.lens (\S3BucketLogDestination' {kmsKeyArn} -> kmsKeyArn) (\s@S3BucketLogDestination' {} a -> s {kmsKeyArn = a} :: S3BucketLogDestination)

-- | The Amazon Resource Name (ARN) of an Amazon S3 bucket where audio log
-- files are stored.
s3BucketLogDestination_s3BucketArn :: Lens.Lens' S3BucketLogDestination Prelude.Text
s3BucketLogDestination_s3BucketArn = Lens.lens (\S3BucketLogDestination' {s3BucketArn} -> s3BucketArn) (\s@S3BucketLogDestination' {} a -> s {s3BucketArn = a} :: S3BucketLogDestination)

-- | The S3 prefix to assign to audio log files.
s3BucketLogDestination_logPrefix :: Lens.Lens' S3BucketLogDestination Prelude.Text
s3BucketLogDestination_logPrefix = Lens.lens (\S3BucketLogDestination' {logPrefix} -> logPrefix) (\s@S3BucketLogDestination' {} a -> s {logPrefix = a} :: S3BucketLogDestination)

instance Core.FromJSON S3BucketLogDestination where
  parseJSON =
    Core.withObject
      "S3BucketLogDestination"
      ( \x ->
          S3BucketLogDestination'
            Prelude.<$> (x Core..:? "kmsKeyArn")
            Prelude.<*> (x Core..: "s3BucketArn")
            Prelude.<*> (x Core..: "logPrefix")
      )

instance Prelude.Hashable S3BucketLogDestination where
  hashWithSalt _salt S3BucketLogDestination' {..} =
    _salt `Prelude.hashWithSalt` kmsKeyArn
      `Prelude.hashWithSalt` s3BucketArn
      `Prelude.hashWithSalt` logPrefix

instance Prelude.NFData S3BucketLogDestination where
  rnf S3BucketLogDestination' {..} =
    Prelude.rnf kmsKeyArn
      `Prelude.seq` Prelude.rnf s3BucketArn
      `Prelude.seq` Prelude.rnf logPrefix

instance Core.ToJSON S3BucketLogDestination where
  toJSON S3BucketLogDestination' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("kmsKeyArn" Core..=) Prelude.<$> kmsKeyArn,
            Prelude.Just ("s3BucketArn" Core..= s3BucketArn),
            Prelude.Just ("logPrefix" Core..= logPrefix)
          ]
      )
