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
-- Module      : Amazonka.DataBrew.Types.S3Location
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types.S3Location where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents an Amazon S3 location (bucket name, bucket owner, and object
-- key) where DataBrew can read input data, or write output from a job.
--
-- /See:/ 'newS3Location' smart constructor.
data S3Location = S3Location'
  { -- | The unique name of the object in the bucket.
    key :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID of the bucket owner.
    bucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 bucket name.
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
-- 'key', 's3Location_key' - The unique name of the object in the bucket.
--
-- 'bucketOwner', 's3Location_bucketOwner' - The Amazon Web Services account ID of the bucket owner.
--
-- 'bucket', 's3Location_bucket' - The Amazon S3 bucket name.
newS3Location ::
  -- | 'bucket'
  Prelude.Text ->
  S3Location
newS3Location pBucket_ =
  S3Location'
    { key = Prelude.Nothing,
      bucketOwner = Prelude.Nothing,
      bucket = pBucket_
    }

-- | The unique name of the object in the bucket.
s3Location_key :: Lens.Lens' S3Location (Prelude.Maybe Prelude.Text)
s3Location_key = Lens.lens (\S3Location' {key} -> key) (\s@S3Location' {} a -> s {key = a} :: S3Location)

-- | The Amazon Web Services account ID of the bucket owner.
s3Location_bucketOwner :: Lens.Lens' S3Location (Prelude.Maybe Prelude.Text)
s3Location_bucketOwner = Lens.lens (\S3Location' {bucketOwner} -> bucketOwner) (\s@S3Location' {} a -> s {bucketOwner = a} :: S3Location)

-- | The Amazon S3 bucket name.
s3Location_bucket :: Lens.Lens' S3Location Prelude.Text
s3Location_bucket = Lens.lens (\S3Location' {bucket} -> bucket) (\s@S3Location' {} a -> s {bucket = a} :: S3Location)

instance Core.FromJSON S3Location where
  parseJSON =
    Core.withObject
      "S3Location"
      ( \x ->
          S3Location'
            Prelude.<$> (x Core..:? "Key")
            Prelude.<*> (x Core..:? "BucketOwner")
            Prelude.<*> (x Core..: "Bucket")
      )

instance Prelude.Hashable S3Location where
  hashWithSalt _salt S3Location' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` bucketOwner
      `Prelude.hashWithSalt` bucket

instance Prelude.NFData S3Location where
  rnf S3Location' {..} =
    Prelude.rnf key
      `Prelude.seq` Prelude.rnf bucketOwner
      `Prelude.seq` Prelude.rnf bucket

instance Core.ToJSON S3Location where
  toJSON S3Location' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Key" Core..=) Prelude.<$> key,
            ("BucketOwner" Core..=) Prelude.<$> bucketOwner,
            Prelude.Just ("Bucket" Core..= bucket)
          ]
      )
