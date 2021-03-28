{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.BucketPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.BucketPolicy
  ( BucketPolicy (..)
  -- * Smart constructor
  , mkBucketPolicy
  -- * Lenses
  , bpAllowsPublicReadAccess
  , bpAllowsPublicWriteAccess
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information on the current bucket policies for the S3 bucket.
--
-- /See:/ 'mkBucketPolicy' smart constructor.
data BucketPolicy = BucketPolicy'
  { allowsPublicReadAccess :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether public read access for the bucket is enabled through a bucket policy.
  , allowsPublicWriteAccess :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether public write access for the bucket is enabled through a bucket policy.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BucketPolicy' value with any optional fields omitted.
mkBucketPolicy
    :: BucketPolicy
mkBucketPolicy
  = BucketPolicy'{allowsPublicReadAccess = Core.Nothing,
                  allowsPublicWriteAccess = Core.Nothing}

-- | A value that indicates whether public read access for the bucket is enabled through a bucket policy.
--
-- /Note:/ Consider using 'allowsPublicReadAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpAllowsPublicReadAccess :: Lens.Lens' BucketPolicy (Core.Maybe Core.Bool)
bpAllowsPublicReadAccess = Lens.field @"allowsPublicReadAccess"
{-# INLINEABLE bpAllowsPublicReadAccess #-}
{-# DEPRECATED allowsPublicReadAccess "Use generic-lens or generic-optics with 'allowsPublicReadAccess' instead"  #-}

-- | A value that indicates whether public write access for the bucket is enabled through a bucket policy.
--
-- /Note:/ Consider using 'allowsPublicWriteAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpAllowsPublicWriteAccess :: Lens.Lens' BucketPolicy (Core.Maybe Core.Bool)
bpAllowsPublicWriteAccess = Lens.field @"allowsPublicWriteAccess"
{-# INLINEABLE bpAllowsPublicWriteAccess #-}
{-# DEPRECATED allowsPublicWriteAccess "Use generic-lens or generic-optics with 'allowsPublicWriteAccess' instead"  #-}

instance Core.FromJSON BucketPolicy where
        parseJSON
          = Core.withObject "BucketPolicy" Core.$
              \ x ->
                BucketPolicy' Core.<$>
                  (x Core..:? "allowsPublicReadAccess") Core.<*>
                    x Core..:? "allowsPublicWriteAccess"
