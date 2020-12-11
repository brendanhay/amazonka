-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.BucketLevelPermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.BucketLevelPermissions
  ( BucketLevelPermissions (..),

    -- * Smart constructor
    mkBucketLevelPermissions,

    -- * Lenses
    blpAccessControlList,
    blpBlockPublicAccess,
    blpBucketPolicy,
  )
where

import Network.AWS.GuardDuty.Types.AccessControlList
import Network.AWS.GuardDuty.Types.BlockPublicAccess
import Network.AWS.GuardDuty.Types.BucketPolicy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the bucket level permissions for the S3 bucket.
--
-- /See:/ 'mkBucketLevelPermissions' smart constructor.
data BucketLevelPermissions = BucketLevelPermissions'
  { accessControlList ::
      Lude.Maybe AccessControlList,
    blockPublicAccess ::
      Lude.Maybe BlockPublicAccess,
    bucketPolicy :: Lude.Maybe BucketPolicy
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BucketLevelPermissions' with the minimum fields required to make a request.
--
-- * 'accessControlList' - Contains information on how Access Control Policies are applied to the bucket.
-- * 'blockPublicAccess' - Contains information on which account level S3 Block Public Access settings are applied to the S3 bucket.
-- * 'bucketPolicy' - Contains information on the bucket policies for the S3 bucket.
mkBucketLevelPermissions ::
  BucketLevelPermissions
mkBucketLevelPermissions =
  BucketLevelPermissions'
    { accessControlList = Lude.Nothing,
      blockPublicAccess = Lude.Nothing,
      bucketPolicy = Lude.Nothing
    }

-- | Contains information on how Access Control Policies are applied to the bucket.
--
-- /Note:/ Consider using 'accessControlList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blpAccessControlList :: Lens.Lens' BucketLevelPermissions (Lude.Maybe AccessControlList)
blpAccessControlList = Lens.lens (accessControlList :: BucketLevelPermissions -> Lude.Maybe AccessControlList) (\s a -> s {accessControlList = a} :: BucketLevelPermissions)
{-# DEPRECATED blpAccessControlList "Use generic-lens or generic-optics with 'accessControlList' instead." #-}

-- | Contains information on which account level S3 Block Public Access settings are applied to the S3 bucket.
--
-- /Note:/ Consider using 'blockPublicAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blpBlockPublicAccess :: Lens.Lens' BucketLevelPermissions (Lude.Maybe BlockPublicAccess)
blpBlockPublicAccess = Lens.lens (blockPublicAccess :: BucketLevelPermissions -> Lude.Maybe BlockPublicAccess) (\s a -> s {blockPublicAccess = a} :: BucketLevelPermissions)
{-# DEPRECATED blpBlockPublicAccess "Use generic-lens or generic-optics with 'blockPublicAccess' instead." #-}

-- | Contains information on the bucket policies for the S3 bucket.
--
-- /Note:/ Consider using 'bucketPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blpBucketPolicy :: Lens.Lens' BucketLevelPermissions (Lude.Maybe BucketPolicy)
blpBucketPolicy = Lens.lens (bucketPolicy :: BucketLevelPermissions -> Lude.Maybe BucketPolicy) (\s a -> s {bucketPolicy = a} :: BucketLevelPermissions)
{-# DEPRECATED blpBucketPolicy "Use generic-lens or generic-optics with 'bucketPolicy' instead." #-}

instance Lude.FromJSON BucketLevelPermissions where
  parseJSON =
    Lude.withObject
      "BucketLevelPermissions"
      ( \x ->
          BucketLevelPermissions'
            Lude.<$> (x Lude..:? "accessControlList")
            Lude.<*> (x Lude..:? "blockPublicAccess")
            Lude.<*> (x Lude..:? "bucketPolicy")
      )
