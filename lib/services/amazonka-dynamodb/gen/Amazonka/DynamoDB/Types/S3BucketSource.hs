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
-- Module      : Amazonka.DynamoDB.Types.S3BucketSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.S3BucketSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | The S3 bucket that is being imported from.
--
-- /See:/ 'newS3BucketSource' smart constructor.
data S3BucketSource = S3BucketSource'
  { -- | The account number of the S3 bucket that is being imported from. If the
    -- bucket is owned by the requester this is optional.
    s3BucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The key prefix shared by all S3 Objects that are being imported.
    s3KeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | The S3 bucket that is being imported from.
    s3Bucket :: Prelude.Text
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
-- 's3BucketOwner', 's3BucketSource_s3BucketOwner' - The account number of the S3 bucket that is being imported from. If the
-- bucket is owned by the requester this is optional.
--
-- 's3KeyPrefix', 's3BucketSource_s3KeyPrefix' - The key prefix shared by all S3 Objects that are being imported.
--
-- 's3Bucket', 's3BucketSource_s3Bucket' - The S3 bucket that is being imported from.
newS3BucketSource ::
  -- | 's3Bucket'
  Prelude.Text ->
  S3BucketSource
newS3BucketSource pS3Bucket_ =
  S3BucketSource'
    { s3BucketOwner = Prelude.Nothing,
      s3KeyPrefix = Prelude.Nothing,
      s3Bucket = pS3Bucket_
    }

-- | The account number of the S3 bucket that is being imported from. If the
-- bucket is owned by the requester this is optional.
s3BucketSource_s3BucketOwner :: Lens.Lens' S3BucketSource (Prelude.Maybe Prelude.Text)
s3BucketSource_s3BucketOwner = Lens.lens (\S3BucketSource' {s3BucketOwner} -> s3BucketOwner) (\s@S3BucketSource' {} a -> s {s3BucketOwner = a} :: S3BucketSource)

-- | The key prefix shared by all S3 Objects that are being imported.
s3BucketSource_s3KeyPrefix :: Lens.Lens' S3BucketSource (Prelude.Maybe Prelude.Text)
s3BucketSource_s3KeyPrefix = Lens.lens (\S3BucketSource' {s3KeyPrefix} -> s3KeyPrefix) (\s@S3BucketSource' {} a -> s {s3KeyPrefix = a} :: S3BucketSource)

-- | The S3 bucket that is being imported from.
s3BucketSource_s3Bucket :: Lens.Lens' S3BucketSource Prelude.Text
s3BucketSource_s3Bucket = Lens.lens (\S3BucketSource' {s3Bucket} -> s3Bucket) (\s@S3BucketSource' {} a -> s {s3Bucket = a} :: S3BucketSource)

instance Data.FromJSON S3BucketSource where
  parseJSON =
    Data.withObject
      "S3BucketSource"
      ( \x ->
          S3BucketSource'
            Prelude.<$> (x Data..:? "S3BucketOwner")
            Prelude.<*> (x Data..:? "S3KeyPrefix")
            Prelude.<*> (x Data..: "S3Bucket")
      )

instance Prelude.Hashable S3BucketSource where
  hashWithSalt _salt S3BucketSource' {..} =
    _salt
      `Prelude.hashWithSalt` s3BucketOwner
      `Prelude.hashWithSalt` s3KeyPrefix
      `Prelude.hashWithSalt` s3Bucket

instance Prelude.NFData S3BucketSource where
  rnf S3BucketSource' {..} =
    Prelude.rnf s3BucketOwner
      `Prelude.seq` Prelude.rnf s3KeyPrefix
      `Prelude.seq` Prelude.rnf s3Bucket

instance Data.ToJSON S3BucketSource where
  toJSON S3BucketSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("S3BucketOwner" Data..=) Prelude.<$> s3BucketOwner,
            ("S3KeyPrefix" Data..=) Prelude.<$> s3KeyPrefix,
            Prelude.Just ("S3Bucket" Data..= s3Bucket)
          ]
      )
