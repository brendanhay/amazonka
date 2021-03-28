{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.PermissionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.PermissionConfiguration
  ( PermissionConfiguration (..)
  -- * Smart constructor
  , mkPermissionConfiguration
  -- * Lenses
  , pcAccountLevelPermissions
  , pcBucketLevelPermissions
  ) where

import qualified Network.AWS.GuardDuty.Types.AccountLevelPermissions as Types
import qualified Network.AWS.GuardDuty.Types.BucketLevelPermissions as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about how permissions are configured for the S3 bucket.
--
-- /See:/ 'mkPermissionConfiguration' smart constructor.
data PermissionConfiguration = PermissionConfiguration'
  { accountLevelPermissions :: Core.Maybe Types.AccountLevelPermissions
    -- ^ Contains information about the account level permissions on the S3 bucket.
  , bucketLevelPermissions :: Core.Maybe Types.BucketLevelPermissions
    -- ^ Contains information about the bucket level permissions for the S3 bucket.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PermissionConfiguration' value with any optional fields omitted.
mkPermissionConfiguration
    :: PermissionConfiguration
mkPermissionConfiguration
  = PermissionConfiguration'{accountLevelPermissions = Core.Nothing,
                             bucketLevelPermissions = Core.Nothing}

-- | Contains information about the account level permissions on the S3 bucket.
--
-- /Note:/ Consider using 'accountLevelPermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcAccountLevelPermissions :: Lens.Lens' PermissionConfiguration (Core.Maybe Types.AccountLevelPermissions)
pcAccountLevelPermissions = Lens.field @"accountLevelPermissions"
{-# INLINEABLE pcAccountLevelPermissions #-}
{-# DEPRECATED accountLevelPermissions "Use generic-lens or generic-optics with 'accountLevelPermissions' instead"  #-}

-- | Contains information about the bucket level permissions for the S3 bucket.
--
-- /Note:/ Consider using 'bucketLevelPermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcBucketLevelPermissions :: Lens.Lens' PermissionConfiguration (Core.Maybe Types.BucketLevelPermissions)
pcBucketLevelPermissions = Lens.field @"bucketLevelPermissions"
{-# INLINEABLE pcBucketLevelPermissions #-}
{-# DEPRECATED bucketLevelPermissions "Use generic-lens or generic-optics with 'bucketLevelPermissions' instead"  #-}

instance Core.FromJSON PermissionConfiguration where
        parseJSON
          = Core.withObject "PermissionConfiguration" Core.$
              \ x ->
                PermissionConfiguration' Core.<$>
                  (x Core..:? "accountLevelPermissions") Core.<*>
                    x Core..:? "bucketLevelPermissions"
