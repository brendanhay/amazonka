{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.S3.Types.Bucket
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Bucket where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal

-- | In terms of implementation, a Bucket is a resource. An Amazon S3 bucket
-- name is globally unique, and the namespace is shared by all AWS
-- accounts.
--
-- /See:/ 'newBucket' smart constructor.
data Bucket = Bucket'
  { -- | Date the bucket was created. This date can change when making changes to
    -- your bucket, such as editing its bucket policy.
    creationDate :: Prelude.ISO8601,
    -- | The name of the bucket.
    name :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Bucket' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDate', 'bucket_creationDate' - Date the bucket was created. This date can change when making changes to
-- your bucket, such as editing its bucket policy.
--
-- 'name', 'bucket_name' - The name of the bucket.
newBucket ::
  -- | 'creationDate'
  Prelude.UTCTime ->
  -- | 'name'
  BucketName ->
  Bucket
newBucket pCreationDate_ pName_ =
  Bucket'
    { creationDate =
        Prelude._Time Lens.# pCreationDate_,
      name = pName_
    }

-- | Date the bucket was created. This date can change when making changes to
-- your bucket, such as editing its bucket policy.
bucket_creationDate :: Lens.Lens' Bucket Prelude.UTCTime
bucket_creationDate = Lens.lens (\Bucket' {creationDate} -> creationDate) (\s@Bucket' {} a -> s {creationDate = a} :: Bucket) Prelude.. Prelude._Time

-- | The name of the bucket.
bucket_name :: Lens.Lens' Bucket BucketName
bucket_name = Lens.lens (\Bucket' {name} -> name) (\s@Bucket' {} a -> s {name = a} :: Bucket)

instance Prelude.FromXML Bucket where
  parseXML x =
    Bucket'
      Prelude.<$> (x Prelude..@ "CreationDate")
      Prelude.<*> (x Prelude..@ "Name")

instance Prelude.Hashable Bucket

instance Prelude.NFData Bucket
