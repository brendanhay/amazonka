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
-- Module      : Amazonka.MacieV2.Types.S3Destination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.S3Destination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies an S3 bucket to store data classification results in, and the
-- encryption settings to use when storing results in that bucket.
--
-- /See:/ 'newS3Destination' smart constructor.
data S3Destination = S3Destination'
  { -- | The path prefix to use in the path to the location in the bucket. This
    -- prefix specifies where to store classification results in the bucket.
    keyPrefix :: Prelude.Maybe Prelude.Text,
    -- | The name of the bucket.
    bucketName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the customer managed KMS key to use
    -- for encryption of the results. This must be the ARN of an existing,
    -- symmetric encryption KMS key that\'s in the same Amazon Web Services
    -- Region as the bucket.
    kmsKeyArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3Destination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyPrefix', 's3Destination_keyPrefix' - The path prefix to use in the path to the location in the bucket. This
-- prefix specifies where to store classification results in the bucket.
--
-- 'bucketName', 's3Destination_bucketName' - The name of the bucket.
--
-- 'kmsKeyArn', 's3Destination_kmsKeyArn' - The Amazon Resource Name (ARN) of the customer managed KMS key to use
-- for encryption of the results. This must be the ARN of an existing,
-- symmetric encryption KMS key that\'s in the same Amazon Web Services
-- Region as the bucket.
newS3Destination ::
  -- | 'bucketName'
  Prelude.Text ->
  -- | 'kmsKeyArn'
  Prelude.Text ->
  S3Destination
newS3Destination pBucketName_ pKmsKeyArn_ =
  S3Destination'
    { keyPrefix = Prelude.Nothing,
      bucketName = pBucketName_,
      kmsKeyArn = pKmsKeyArn_
    }

-- | The path prefix to use in the path to the location in the bucket. This
-- prefix specifies where to store classification results in the bucket.
s3Destination_keyPrefix :: Lens.Lens' S3Destination (Prelude.Maybe Prelude.Text)
s3Destination_keyPrefix = Lens.lens (\S3Destination' {keyPrefix} -> keyPrefix) (\s@S3Destination' {} a -> s {keyPrefix = a} :: S3Destination)

-- | The name of the bucket.
s3Destination_bucketName :: Lens.Lens' S3Destination Prelude.Text
s3Destination_bucketName = Lens.lens (\S3Destination' {bucketName} -> bucketName) (\s@S3Destination' {} a -> s {bucketName = a} :: S3Destination)

-- | The Amazon Resource Name (ARN) of the customer managed KMS key to use
-- for encryption of the results. This must be the ARN of an existing,
-- symmetric encryption KMS key that\'s in the same Amazon Web Services
-- Region as the bucket.
s3Destination_kmsKeyArn :: Lens.Lens' S3Destination Prelude.Text
s3Destination_kmsKeyArn = Lens.lens (\S3Destination' {kmsKeyArn} -> kmsKeyArn) (\s@S3Destination' {} a -> s {kmsKeyArn = a} :: S3Destination)

instance Core.FromJSON S3Destination where
  parseJSON =
    Core.withObject
      "S3Destination"
      ( \x ->
          S3Destination'
            Prelude.<$> (x Core..:? "keyPrefix")
            Prelude.<*> (x Core..: "bucketName")
            Prelude.<*> (x Core..: "kmsKeyArn")
      )

instance Prelude.Hashable S3Destination where
  hashWithSalt _salt S3Destination' {..} =
    _salt `Prelude.hashWithSalt` keyPrefix
      `Prelude.hashWithSalt` bucketName
      `Prelude.hashWithSalt` kmsKeyArn

instance Prelude.NFData S3Destination where
  rnf S3Destination' {..} =
    Prelude.rnf keyPrefix
      `Prelude.seq` Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf kmsKeyArn

instance Core.ToJSON S3Destination where
  toJSON S3Destination' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("keyPrefix" Core..=) Prelude.<$> keyPrefix,
            Prelude.Just ("bucketName" Core..= bucketName),
            Prelude.Just ("kmsKeyArn" Core..= kmsKeyArn)
          ]
      )
