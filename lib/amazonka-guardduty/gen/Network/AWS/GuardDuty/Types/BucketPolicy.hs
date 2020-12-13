{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.BucketPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.BucketPolicy
  ( BucketPolicy (..),

    -- * Smart constructor
    mkBucketPolicy,

    -- * Lenses
    bpAllowsPublicWriteAccess,
    bpAllowsPublicReadAccess,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information on the current bucket policies for the S3 bucket.
--
-- /See:/ 'mkBucketPolicy' smart constructor.
data BucketPolicy = BucketPolicy'
  { -- | A value that indicates whether public write access for the bucket is enabled through a bucket policy.
    allowsPublicWriteAccess :: Lude.Maybe Lude.Bool,
    -- | A value that indicates whether public read access for the bucket is enabled through a bucket policy.
    allowsPublicReadAccess :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BucketPolicy' with the minimum fields required to make a request.
--
-- * 'allowsPublicWriteAccess' - A value that indicates whether public write access for the bucket is enabled through a bucket policy.
-- * 'allowsPublicReadAccess' - A value that indicates whether public read access for the bucket is enabled through a bucket policy.
mkBucketPolicy ::
  BucketPolicy
mkBucketPolicy =
  BucketPolicy'
    { allowsPublicWriteAccess = Lude.Nothing,
      allowsPublicReadAccess = Lude.Nothing
    }

-- | A value that indicates whether public write access for the bucket is enabled through a bucket policy.
--
-- /Note:/ Consider using 'allowsPublicWriteAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpAllowsPublicWriteAccess :: Lens.Lens' BucketPolicy (Lude.Maybe Lude.Bool)
bpAllowsPublicWriteAccess = Lens.lens (allowsPublicWriteAccess :: BucketPolicy -> Lude.Maybe Lude.Bool) (\s a -> s {allowsPublicWriteAccess = a} :: BucketPolicy)
{-# DEPRECATED bpAllowsPublicWriteAccess "Use generic-lens or generic-optics with 'allowsPublicWriteAccess' instead." #-}

-- | A value that indicates whether public read access for the bucket is enabled through a bucket policy.
--
-- /Note:/ Consider using 'allowsPublicReadAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpAllowsPublicReadAccess :: Lens.Lens' BucketPolicy (Lude.Maybe Lude.Bool)
bpAllowsPublicReadAccess = Lens.lens (allowsPublicReadAccess :: BucketPolicy -> Lude.Maybe Lude.Bool) (\s a -> s {allowsPublicReadAccess = a} :: BucketPolicy)
{-# DEPRECATED bpAllowsPublicReadAccess "Use generic-lens or generic-optics with 'allowsPublicReadAccess' instead." #-}

instance Lude.FromJSON BucketPolicy where
  parseJSON =
    Lude.withObject
      "BucketPolicy"
      ( \x ->
          BucketPolicy'
            Lude.<$> (x Lude..:? "allowsPublicWriteAccess")
            Lude.<*> (x Lude..:? "allowsPublicReadAccess")
      )
