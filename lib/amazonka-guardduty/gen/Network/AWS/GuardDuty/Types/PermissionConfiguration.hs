{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.PermissionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.PermissionConfiguration
  ( PermissionConfiguration (..),

    -- * Smart constructor
    mkPermissionConfiguration,

    -- * Lenses
    pcBucketLevelPermissions,
    pcAccountLevelPermissions,
  )
where

import Network.AWS.GuardDuty.Types.AccountLevelPermissions
import Network.AWS.GuardDuty.Types.BucketLevelPermissions
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about how permissions are configured for the S3 bucket.
--
-- /See:/ 'mkPermissionConfiguration' smart constructor.
data PermissionConfiguration = PermissionConfiguration'
  { -- | Contains information about the bucket level permissions for the S3 bucket.
    bucketLevelPermissions :: Lude.Maybe BucketLevelPermissions,
    -- | Contains information about the account level permissions on the S3 bucket.
    accountLevelPermissions :: Lude.Maybe AccountLevelPermissions
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PermissionConfiguration' with the minimum fields required to make a request.
--
-- * 'bucketLevelPermissions' - Contains information about the bucket level permissions for the S3 bucket.
-- * 'accountLevelPermissions' - Contains information about the account level permissions on the S3 bucket.
mkPermissionConfiguration ::
  PermissionConfiguration
mkPermissionConfiguration =
  PermissionConfiguration'
    { bucketLevelPermissions = Lude.Nothing,
      accountLevelPermissions = Lude.Nothing
    }

-- | Contains information about the bucket level permissions for the S3 bucket.
--
-- /Note:/ Consider using 'bucketLevelPermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcBucketLevelPermissions :: Lens.Lens' PermissionConfiguration (Lude.Maybe BucketLevelPermissions)
pcBucketLevelPermissions = Lens.lens (bucketLevelPermissions :: PermissionConfiguration -> Lude.Maybe BucketLevelPermissions) (\s a -> s {bucketLevelPermissions = a} :: PermissionConfiguration)
{-# DEPRECATED pcBucketLevelPermissions "Use generic-lens or generic-optics with 'bucketLevelPermissions' instead." #-}

-- | Contains information about the account level permissions on the S3 bucket.
--
-- /Note:/ Consider using 'accountLevelPermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcAccountLevelPermissions :: Lens.Lens' PermissionConfiguration (Lude.Maybe AccountLevelPermissions)
pcAccountLevelPermissions = Lens.lens (accountLevelPermissions :: PermissionConfiguration -> Lude.Maybe AccountLevelPermissions) (\s a -> s {accountLevelPermissions = a} :: PermissionConfiguration)
{-# DEPRECATED pcAccountLevelPermissions "Use generic-lens or generic-optics with 'accountLevelPermissions' instead." #-}

instance Lude.FromJSON PermissionConfiguration where
  parseJSON =
    Lude.withObject
      "PermissionConfiguration"
      ( \x ->
          PermissionConfiguration'
            Lude.<$> (x Lude..:? "bucketLevelPermissions")
            Lude.<*> (x Lude..:? "accountLevelPermissions")
      )
