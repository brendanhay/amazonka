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
-- Module      : Amazonka.MacieV2.Types.BucketLevelPermissions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.BucketLevelPermissions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.AccessControlList
import Amazonka.MacieV2.Types.BlockPublicAccess
import Amazonka.MacieV2.Types.BucketPolicy
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the bucket-level permissions settings for an
-- S3 bucket.
--
-- /See:/ 'newBucketLevelPermissions' smart constructor.
data BucketLevelPermissions = BucketLevelPermissions'
  { -- | The permissions settings of the access control list (ACL) for the
    -- bucket. This value is null if an ACL hasn\'t been defined for the
    -- bucket.
    accessControlList :: Prelude.Maybe AccessControlList,
    -- | The block public access settings for the bucket.
    blockPublicAccess :: Prelude.Maybe BlockPublicAccess,
    -- | The permissions settings of the bucket policy for the bucket. This value
    -- is null if a bucket policy hasn\'t been defined for the bucket.
    bucketPolicy :: Prelude.Maybe BucketPolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BucketLevelPermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessControlList', 'bucketLevelPermissions_accessControlList' - The permissions settings of the access control list (ACL) for the
-- bucket. This value is null if an ACL hasn\'t been defined for the
-- bucket.
--
-- 'blockPublicAccess', 'bucketLevelPermissions_blockPublicAccess' - The block public access settings for the bucket.
--
-- 'bucketPolicy', 'bucketLevelPermissions_bucketPolicy' - The permissions settings of the bucket policy for the bucket. This value
-- is null if a bucket policy hasn\'t been defined for the bucket.
newBucketLevelPermissions ::
  BucketLevelPermissions
newBucketLevelPermissions =
  BucketLevelPermissions'
    { accessControlList =
        Prelude.Nothing,
      blockPublicAccess = Prelude.Nothing,
      bucketPolicy = Prelude.Nothing
    }

-- | The permissions settings of the access control list (ACL) for the
-- bucket. This value is null if an ACL hasn\'t been defined for the
-- bucket.
bucketLevelPermissions_accessControlList :: Lens.Lens' BucketLevelPermissions (Prelude.Maybe AccessControlList)
bucketLevelPermissions_accessControlList = Lens.lens (\BucketLevelPermissions' {accessControlList} -> accessControlList) (\s@BucketLevelPermissions' {} a -> s {accessControlList = a} :: BucketLevelPermissions)

-- | The block public access settings for the bucket.
bucketLevelPermissions_blockPublicAccess :: Lens.Lens' BucketLevelPermissions (Prelude.Maybe BlockPublicAccess)
bucketLevelPermissions_blockPublicAccess = Lens.lens (\BucketLevelPermissions' {blockPublicAccess} -> blockPublicAccess) (\s@BucketLevelPermissions' {} a -> s {blockPublicAccess = a} :: BucketLevelPermissions)

-- | The permissions settings of the bucket policy for the bucket. This value
-- is null if a bucket policy hasn\'t been defined for the bucket.
bucketLevelPermissions_bucketPolicy :: Lens.Lens' BucketLevelPermissions (Prelude.Maybe BucketPolicy)
bucketLevelPermissions_bucketPolicy = Lens.lens (\BucketLevelPermissions' {bucketPolicy} -> bucketPolicy) (\s@BucketLevelPermissions' {} a -> s {bucketPolicy = a} :: BucketLevelPermissions)

instance Data.FromJSON BucketLevelPermissions where
  parseJSON =
    Data.withObject
      "BucketLevelPermissions"
      ( \x ->
          BucketLevelPermissions'
            Prelude.<$> (x Data..:? "accessControlList")
            Prelude.<*> (x Data..:? "blockPublicAccess")
            Prelude.<*> (x Data..:? "bucketPolicy")
      )

instance Prelude.Hashable BucketLevelPermissions where
  hashWithSalt _salt BucketLevelPermissions' {..} =
    _salt `Prelude.hashWithSalt` accessControlList
      `Prelude.hashWithSalt` blockPublicAccess
      `Prelude.hashWithSalt` bucketPolicy

instance Prelude.NFData BucketLevelPermissions where
  rnf BucketLevelPermissions' {..} =
    Prelude.rnf accessControlList
      `Prelude.seq` Prelude.rnf blockPublicAccess
      `Prelude.seq` Prelude.rnf bucketPolicy
