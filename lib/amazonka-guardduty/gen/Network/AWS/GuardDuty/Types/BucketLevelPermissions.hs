{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.BucketLevelPermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.BucketLevelPermissions
  ( BucketLevelPermissions (..)
  -- * Smart constructor
  , mkBucketLevelPermissions
  -- * Lenses
  , blpAccessControlList
  , blpBlockPublicAccess
  , blpBucketPolicy
  ) where

import qualified Network.AWS.GuardDuty.Types.AccessControlList as Types
import qualified Network.AWS.GuardDuty.Types.BlockPublicAccess as Types
import qualified Network.AWS.GuardDuty.Types.BucketPolicy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the bucket level permissions for the S3 bucket.
--
-- /See:/ 'mkBucketLevelPermissions' smart constructor.
data BucketLevelPermissions = BucketLevelPermissions'
  { accessControlList :: Core.Maybe Types.AccessControlList
    -- ^ Contains information on how Access Control Policies are applied to the bucket.
  , blockPublicAccess :: Core.Maybe Types.BlockPublicAccess
    -- ^ Contains information on which account level S3 Block Public Access settings are applied to the S3 bucket.
  , bucketPolicy :: Core.Maybe Types.BucketPolicy
    -- ^ Contains information on the bucket policies for the S3 bucket.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BucketLevelPermissions' value with any optional fields omitted.
mkBucketLevelPermissions
    :: BucketLevelPermissions
mkBucketLevelPermissions
  = BucketLevelPermissions'{accessControlList = Core.Nothing,
                            blockPublicAccess = Core.Nothing, bucketPolicy = Core.Nothing}

-- | Contains information on how Access Control Policies are applied to the bucket.
--
-- /Note:/ Consider using 'accessControlList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blpAccessControlList :: Lens.Lens' BucketLevelPermissions (Core.Maybe Types.AccessControlList)
blpAccessControlList = Lens.field @"accessControlList"
{-# INLINEABLE blpAccessControlList #-}
{-# DEPRECATED accessControlList "Use generic-lens or generic-optics with 'accessControlList' instead"  #-}

-- | Contains information on which account level S3 Block Public Access settings are applied to the S3 bucket.
--
-- /Note:/ Consider using 'blockPublicAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blpBlockPublicAccess :: Lens.Lens' BucketLevelPermissions (Core.Maybe Types.BlockPublicAccess)
blpBlockPublicAccess = Lens.field @"blockPublicAccess"
{-# INLINEABLE blpBlockPublicAccess #-}
{-# DEPRECATED blockPublicAccess "Use generic-lens or generic-optics with 'blockPublicAccess' instead"  #-}

-- | Contains information on the bucket policies for the S3 bucket.
--
-- /Note:/ Consider using 'bucketPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blpBucketPolicy :: Lens.Lens' BucketLevelPermissions (Core.Maybe Types.BucketPolicy)
blpBucketPolicy = Lens.field @"bucketPolicy"
{-# INLINEABLE blpBucketPolicy #-}
{-# DEPRECATED bucketPolicy "Use generic-lens or generic-optics with 'bucketPolicy' instead"  #-}

instance Core.FromJSON BucketLevelPermissions where
        parseJSON
          = Core.withObject "BucketLevelPermissions" Core.$
              \ x ->
                BucketLevelPermissions' Core.<$>
                  (x Core..:? "accessControlList") Core.<*>
                    x Core..:? "blockPublicAccess"
                    Core.<*> x Core..:? "bucketPolicy"
