{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Types.BucketInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearchDomains.Types.BucketInfo
  ( BucketInfo (..),

    -- * Smart constructor
    mkBucketInfo,

    -- * Lenses
    biBuckets,
  )
where

import qualified Network.AWS.CloudSearchDomains.Types.Bucket as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A container for the calculated facet values and counts.
--
-- /See:/ 'mkBucketInfo' smart constructor.
newtype BucketInfo = BucketInfo'
  { -- | A list of the calculated facet values and counts.
    buckets :: Core.Maybe [Types.Bucket]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BucketInfo' value with any optional fields omitted.
mkBucketInfo ::
  BucketInfo
mkBucketInfo = BucketInfo' {buckets = Core.Nothing}

-- | A list of the calculated facet values and counts.
--
-- /Note:/ Consider using 'buckets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
biBuckets :: Lens.Lens' BucketInfo (Core.Maybe [Types.Bucket])
biBuckets = Lens.field @"buckets"
{-# DEPRECATED biBuckets "Use generic-lens or generic-optics with 'buckets' instead." #-}

instance Core.FromJSON BucketInfo where
  parseJSON =
    Core.withObject "BucketInfo" Core.$
      \x -> BucketInfo' Core.<$> (x Core..:? "buckets")
