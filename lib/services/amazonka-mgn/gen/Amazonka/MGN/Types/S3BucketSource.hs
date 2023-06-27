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
-- Module      : Amazonka.MGN.Types.S3BucketSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.S3BucketSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | S3 bucket source.
--
-- /See:/ 'newS3BucketSource' smart constructor.
data S3BucketSource = S3BucketSource'
  { -- | S3 bucket source s3 bucket owner.
    s3BucketOwner :: Prelude.Maybe Prelude.Text,
    -- | S3 bucket source s3 bucket.
    s3Bucket :: Prelude.Text,
    -- | S3 bucket source s3 key.
    s3Key :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3BucketSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3BucketOwner', 's3BucketSource_s3BucketOwner' - S3 bucket source s3 bucket owner.
--
-- 's3Bucket', 's3BucketSource_s3Bucket' - S3 bucket source s3 bucket.
--
-- 's3Key', 's3BucketSource_s3Key' - S3 bucket source s3 key.
newS3BucketSource ::
  -- | 's3Bucket'
  Prelude.Text ->
  -- | 's3Key'
  Prelude.Text ->
  S3BucketSource
newS3BucketSource pS3Bucket_ pS3Key_ =
  S3BucketSource'
    { s3BucketOwner = Prelude.Nothing,
      s3Bucket = pS3Bucket_,
      s3Key = pS3Key_
    }

-- | S3 bucket source s3 bucket owner.
s3BucketSource_s3BucketOwner :: Lens.Lens' S3BucketSource (Prelude.Maybe Prelude.Text)
s3BucketSource_s3BucketOwner = Lens.lens (\S3BucketSource' {s3BucketOwner} -> s3BucketOwner) (\s@S3BucketSource' {} a -> s {s3BucketOwner = a} :: S3BucketSource)

-- | S3 bucket source s3 bucket.
s3BucketSource_s3Bucket :: Lens.Lens' S3BucketSource Prelude.Text
s3BucketSource_s3Bucket = Lens.lens (\S3BucketSource' {s3Bucket} -> s3Bucket) (\s@S3BucketSource' {} a -> s {s3Bucket = a} :: S3BucketSource)

-- | S3 bucket source s3 key.
s3BucketSource_s3Key :: Lens.Lens' S3BucketSource Prelude.Text
s3BucketSource_s3Key = Lens.lens (\S3BucketSource' {s3Key} -> s3Key) (\s@S3BucketSource' {} a -> s {s3Key = a} :: S3BucketSource)

instance Data.FromJSON S3BucketSource where
  parseJSON =
    Data.withObject
      "S3BucketSource"
      ( \x ->
          S3BucketSource'
            Prelude.<$> (x Data..:? "s3BucketOwner")
            Prelude.<*> (x Data..: "s3Bucket")
            Prelude.<*> (x Data..: "s3Key")
      )

instance Prelude.Hashable S3BucketSource where
  hashWithSalt _salt S3BucketSource' {..} =
    _salt
      `Prelude.hashWithSalt` s3BucketOwner
      `Prelude.hashWithSalt` s3Bucket
      `Prelude.hashWithSalt` s3Key

instance Prelude.NFData S3BucketSource where
  rnf S3BucketSource' {..} =
    Prelude.rnf s3BucketOwner
      `Prelude.seq` Prelude.rnf s3Bucket
      `Prelude.seq` Prelude.rnf s3Key

instance Data.ToJSON S3BucketSource where
  toJSON S3BucketSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("s3BucketOwner" Data..=) Prelude.<$> s3BucketOwner,
            Prelude.Just ("s3Bucket" Data..= s3Bucket),
            Prelude.Just ("s3Key" Data..= s3Key)
          ]
      )
