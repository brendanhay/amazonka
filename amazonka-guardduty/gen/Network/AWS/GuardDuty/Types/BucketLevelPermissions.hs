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

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types.AccessControlList
import Network.AWS.GuardDuty.Types.BlockPublicAccess
import Network.AWS.GuardDuty.Types.BucketPolicy
import qualified Network.AWS.Lens as Lens

-- | Contains information about the bucket level permissions for the S3
-- bucket.
--
-- /See:/ 'newBucketLevelPermissions' smart constructor.
data BucketLevelPermissions = BucketLevelPermissions'
  { -- | Contains information on which account level S3 Block Public Access
    -- settings are applied to the S3 bucket.
    blockPublicAccess :: Core.Maybe BlockPublicAccess,
    -- | Contains information on the bucket policies for the S3 bucket.
    bucketPolicy :: Core.Maybe BucketPolicy,
    -- | Contains information on how Access Control Policies are applied to the
    -- bucket.
    accessControlList :: Core.Maybe AccessControlList
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      bucketPolicy = Core.Nothing,
      accessControlList = Core.Nothing
    }

-- | Contains information on which account level S3 Block Public Access
-- settings are applied to the S3 bucket.
bucketLevelPermissions_blockPublicAccess :: Lens.Lens' BucketLevelPermissions (Core.Maybe BlockPublicAccess)
bucketLevelPermissions_blockPublicAccess = Lens.lens (\BucketLevelPermissions' {blockPublicAccess} -> blockPublicAccess) (\s@BucketLevelPermissions' {} a -> s {blockPublicAccess = a} :: BucketLevelPermissions)

-- | Contains information on the bucket policies for the S3 bucket.
bucketLevelPermissions_bucketPolicy :: Lens.Lens' BucketLevelPermissions (Core.Maybe BucketPolicy)
bucketLevelPermissions_bucketPolicy = Lens.lens (\BucketLevelPermissions' {bucketPolicy} -> bucketPolicy) (\s@BucketLevelPermissions' {} a -> s {bucketPolicy = a} :: BucketLevelPermissions)

-- | Contains information on how Access Control Policies are applied to the
-- bucket.
bucketLevelPermissions_accessControlList :: Lens.Lens' BucketLevelPermissions (Core.Maybe AccessControlList)
bucketLevelPermissions_accessControlList = Lens.lens (\BucketLevelPermissions' {accessControlList} -> accessControlList) (\s@BucketLevelPermissions' {} a -> s {accessControlList = a} :: BucketLevelPermissions)

instance Core.FromJSON BucketLevelPermissions where
  parseJSON =
    Core.withObject
      "BucketLevelPermissions"
      ( \x ->
          BucketLevelPermissions'
            Core.<$> (x Core..:? "blockPublicAccess")
            Core.<*> (x Core..:? "bucketPolicy")
            Core.<*> (x Core..:? "accessControlList")
      )

instance Core.Hashable BucketLevelPermissions

instance Core.NFData BucketLevelPermissions
