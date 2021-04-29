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
-- Module      : Network.AWS.GuardDuty.Types.BucketLevelPermissions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.BucketLevelPermissions where

import Network.AWS.GuardDuty.Types.AccessControlList
import Network.AWS.GuardDuty.Types.BlockPublicAccess
import Network.AWS.GuardDuty.Types.BucketPolicy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about the bucket level permissions for the S3
-- bucket.
--
-- /See:/ 'newBucketLevelPermissions' smart constructor.
data BucketLevelPermissions = BucketLevelPermissions'
  { -- | Contains information on which account level S3 Block Public Access
    -- settings are applied to the S3 bucket.
    blockPublicAccess :: Prelude.Maybe BlockPublicAccess,
    -- | Contains information on the bucket policies for the S3 bucket.
    bucketPolicy :: Prelude.Maybe BucketPolicy,
    -- | Contains information on how Access Control Policies are applied to the
    -- bucket.
    accessControlList :: Prelude.Maybe AccessControlList
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BucketLevelPermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blockPublicAccess', 'bucketLevelPermissions_blockPublicAccess' - Contains information on which account level S3 Block Public Access
-- settings are applied to the S3 bucket.
--
-- 'bucketPolicy', 'bucketLevelPermissions_bucketPolicy' - Contains information on the bucket policies for the S3 bucket.
--
-- 'accessControlList', 'bucketLevelPermissions_accessControlList' - Contains information on how Access Control Policies are applied to the
-- bucket.
newBucketLevelPermissions ::
  BucketLevelPermissions
newBucketLevelPermissions =
  BucketLevelPermissions'
    { blockPublicAccess =
        Prelude.Nothing,
      bucketPolicy = Prelude.Nothing,
      accessControlList = Prelude.Nothing
    }

-- | Contains information on which account level S3 Block Public Access
-- settings are applied to the S3 bucket.
bucketLevelPermissions_blockPublicAccess :: Lens.Lens' BucketLevelPermissions (Prelude.Maybe BlockPublicAccess)
bucketLevelPermissions_blockPublicAccess = Lens.lens (\BucketLevelPermissions' {blockPublicAccess} -> blockPublicAccess) (\s@BucketLevelPermissions' {} a -> s {blockPublicAccess = a} :: BucketLevelPermissions)

-- | Contains information on the bucket policies for the S3 bucket.
bucketLevelPermissions_bucketPolicy :: Lens.Lens' BucketLevelPermissions (Prelude.Maybe BucketPolicy)
bucketLevelPermissions_bucketPolicy = Lens.lens (\BucketLevelPermissions' {bucketPolicy} -> bucketPolicy) (\s@BucketLevelPermissions' {} a -> s {bucketPolicy = a} :: BucketLevelPermissions)

-- | Contains information on how Access Control Policies are applied to the
-- bucket.
bucketLevelPermissions_accessControlList :: Lens.Lens' BucketLevelPermissions (Prelude.Maybe AccessControlList)
bucketLevelPermissions_accessControlList = Lens.lens (\BucketLevelPermissions' {accessControlList} -> accessControlList) (\s@BucketLevelPermissions' {} a -> s {accessControlList = a} :: BucketLevelPermissions)

instance Prelude.FromJSON BucketLevelPermissions where
  parseJSON =
    Prelude.withObject
      "BucketLevelPermissions"
      ( \x ->
          BucketLevelPermissions'
            Prelude.<$> (x Prelude..:? "blockPublicAccess")
            Prelude.<*> (x Prelude..:? "bucketPolicy")
            Prelude.<*> (x Prelude..:? "accessControlList")
      )

instance Prelude.Hashable BucketLevelPermissions

instance Prelude.NFData BucketLevelPermissions
