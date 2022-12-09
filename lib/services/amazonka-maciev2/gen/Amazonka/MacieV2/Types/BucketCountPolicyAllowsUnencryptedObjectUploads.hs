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
-- Module      : Amazonka.MacieV2.Types.BucketCountPolicyAllowsUnencryptedObjectUploads
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.BucketCountPolicyAllowsUnencryptedObjectUploads where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the number of S3 buckets whose bucket
-- policies do or don\'t require server-side encryption of objects when
-- objects are uploaded to the buckets.
--
-- /See:/ 'newBucketCountPolicyAllowsUnencryptedObjectUploads' smart constructor.
data BucketCountPolicyAllowsUnencryptedObjectUploads = BucketCountPolicyAllowsUnencryptedObjectUploads'
  { -- | The total number of buckets that don\'t have a bucket policy or have a
    -- bucket policy that doesn\'t require server-side encryption of new
    -- objects. If a bucket policy exists, the policy doesn\'t require
    -- PutObject requests to include a valid server-side encryption header: the
    -- x-amz-server-side-encryption header with a value of AES256 or aws:kms,
    -- or the x-amz-server-side-encryption-customer-algorithm header with a
    -- value of AES256.
    allowsUnencryptedObjectUploads :: Prelude.Maybe Prelude.Integer,
    -- | The total number of buckets whose bucket policies require server-side
    -- encryption of new objects. PutObject requests for these buckets must
    -- include a valid server-side encryption header: the
    -- x-amz-server-side-encryption header with a value of AES256 or aws:kms,
    -- or the x-amz-server-side-encryption-customer-algorithm header with a
    -- value of AES256.
    deniesUnencryptedObjectUploads :: Prelude.Maybe Prelude.Integer,
    -- | The total number of buckets that Amazon Macie wasn\'t able to evaluate
    -- server-side encryption requirements for. Macie can\'t determine whether
    -- the bucket policies for these buckets require server-side encryption of
    -- new objects.
    unknown :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BucketCountPolicyAllowsUnencryptedObjectUploads' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowsUnencryptedObjectUploads', 'bucketCountPolicyAllowsUnencryptedObjectUploads_allowsUnencryptedObjectUploads' - The total number of buckets that don\'t have a bucket policy or have a
-- bucket policy that doesn\'t require server-side encryption of new
-- objects. If a bucket policy exists, the policy doesn\'t require
-- PutObject requests to include a valid server-side encryption header: the
-- x-amz-server-side-encryption header with a value of AES256 or aws:kms,
-- or the x-amz-server-side-encryption-customer-algorithm header with a
-- value of AES256.
--
-- 'deniesUnencryptedObjectUploads', 'bucketCountPolicyAllowsUnencryptedObjectUploads_deniesUnencryptedObjectUploads' - The total number of buckets whose bucket policies require server-side
-- encryption of new objects. PutObject requests for these buckets must
-- include a valid server-side encryption header: the
-- x-amz-server-side-encryption header with a value of AES256 or aws:kms,
-- or the x-amz-server-side-encryption-customer-algorithm header with a
-- value of AES256.
--
-- 'unknown', 'bucketCountPolicyAllowsUnencryptedObjectUploads_unknown' - The total number of buckets that Amazon Macie wasn\'t able to evaluate
-- server-side encryption requirements for. Macie can\'t determine whether
-- the bucket policies for these buckets require server-side encryption of
-- new objects.
newBucketCountPolicyAllowsUnencryptedObjectUploads ::
  BucketCountPolicyAllowsUnencryptedObjectUploads
newBucketCountPolicyAllowsUnencryptedObjectUploads =
  BucketCountPolicyAllowsUnencryptedObjectUploads'
    { allowsUnencryptedObjectUploads =
        Prelude.Nothing,
      deniesUnencryptedObjectUploads =
        Prelude.Nothing,
      unknown = Prelude.Nothing
    }

-- | The total number of buckets that don\'t have a bucket policy or have a
-- bucket policy that doesn\'t require server-side encryption of new
-- objects. If a bucket policy exists, the policy doesn\'t require
-- PutObject requests to include a valid server-side encryption header: the
-- x-amz-server-side-encryption header with a value of AES256 or aws:kms,
-- or the x-amz-server-side-encryption-customer-algorithm header with a
-- value of AES256.
bucketCountPolicyAllowsUnencryptedObjectUploads_allowsUnencryptedObjectUploads :: Lens.Lens' BucketCountPolicyAllowsUnencryptedObjectUploads (Prelude.Maybe Prelude.Integer)
bucketCountPolicyAllowsUnencryptedObjectUploads_allowsUnencryptedObjectUploads = Lens.lens (\BucketCountPolicyAllowsUnencryptedObjectUploads' {allowsUnencryptedObjectUploads} -> allowsUnencryptedObjectUploads) (\s@BucketCountPolicyAllowsUnencryptedObjectUploads' {} a -> s {allowsUnencryptedObjectUploads = a} :: BucketCountPolicyAllowsUnencryptedObjectUploads)

-- | The total number of buckets whose bucket policies require server-side
-- encryption of new objects. PutObject requests for these buckets must
-- include a valid server-side encryption header: the
-- x-amz-server-side-encryption header with a value of AES256 or aws:kms,
-- or the x-amz-server-side-encryption-customer-algorithm header with a
-- value of AES256.
bucketCountPolicyAllowsUnencryptedObjectUploads_deniesUnencryptedObjectUploads :: Lens.Lens' BucketCountPolicyAllowsUnencryptedObjectUploads (Prelude.Maybe Prelude.Integer)
bucketCountPolicyAllowsUnencryptedObjectUploads_deniesUnencryptedObjectUploads = Lens.lens (\BucketCountPolicyAllowsUnencryptedObjectUploads' {deniesUnencryptedObjectUploads} -> deniesUnencryptedObjectUploads) (\s@BucketCountPolicyAllowsUnencryptedObjectUploads' {} a -> s {deniesUnencryptedObjectUploads = a} :: BucketCountPolicyAllowsUnencryptedObjectUploads)

-- | The total number of buckets that Amazon Macie wasn\'t able to evaluate
-- server-side encryption requirements for. Macie can\'t determine whether
-- the bucket policies for these buckets require server-side encryption of
-- new objects.
bucketCountPolicyAllowsUnencryptedObjectUploads_unknown :: Lens.Lens' BucketCountPolicyAllowsUnencryptedObjectUploads (Prelude.Maybe Prelude.Integer)
bucketCountPolicyAllowsUnencryptedObjectUploads_unknown = Lens.lens (\BucketCountPolicyAllowsUnencryptedObjectUploads' {unknown} -> unknown) (\s@BucketCountPolicyAllowsUnencryptedObjectUploads' {} a -> s {unknown = a} :: BucketCountPolicyAllowsUnencryptedObjectUploads)

instance
  Data.FromJSON
    BucketCountPolicyAllowsUnencryptedObjectUploads
  where
  parseJSON =
    Data.withObject
      "BucketCountPolicyAllowsUnencryptedObjectUploads"
      ( \x ->
          BucketCountPolicyAllowsUnencryptedObjectUploads'
            Prelude.<$> (x Data..:? "allowsUnencryptedObjectUploads")
              Prelude.<*> (x Data..:? "deniesUnencryptedObjectUploads")
              Prelude.<*> (x Data..:? "unknown")
      )

instance
  Prelude.Hashable
    BucketCountPolicyAllowsUnencryptedObjectUploads
  where
  hashWithSalt
    _salt
    BucketCountPolicyAllowsUnencryptedObjectUploads' {..} =
      _salt
        `Prelude.hashWithSalt` allowsUnencryptedObjectUploads
        `Prelude.hashWithSalt` deniesUnencryptedObjectUploads
        `Prelude.hashWithSalt` unknown

instance
  Prelude.NFData
    BucketCountPolicyAllowsUnencryptedObjectUploads
  where
  rnf
    BucketCountPolicyAllowsUnencryptedObjectUploads' {..} =
      Prelude.rnf allowsUnencryptedObjectUploads
        `Prelude.seq` Prelude.rnf deniesUnencryptedObjectUploads
        `Prelude.seq` Prelude.rnf unknown
