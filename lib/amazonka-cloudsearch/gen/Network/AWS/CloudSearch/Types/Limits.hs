{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.Limits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudSearch.Types.Limits
  ( Limits (..)
  -- * Smart constructor
  , mkLimits
  -- * Lenses
  , lMaximumReplicationCount
  , lMaximumPartitionCount
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | /See:/ 'mkLimits' smart constructor.
data Limits = Limits'
  { maximumReplicationCount :: Core.Natural
  , maximumPartitionCount :: Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Limits' value with any optional fields omitted.
mkLimits
    :: Core.Natural -- ^ 'maximumReplicationCount'
    -> Core.Natural -- ^ 'maximumPartitionCount'
    -> Limits
mkLimits maximumReplicationCount maximumPartitionCount
  = Limits'{maximumReplicationCount, maximumPartitionCount}

-- | Undocumented field.
--
-- /Note:/ Consider using 'maximumReplicationCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lMaximumReplicationCount :: Lens.Lens' Limits Core.Natural
lMaximumReplicationCount = Lens.field @"maximumReplicationCount"
{-# INLINEABLE lMaximumReplicationCount #-}
{-# DEPRECATED maximumReplicationCount "Use generic-lens or generic-optics with 'maximumReplicationCount' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'maximumPartitionCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lMaximumPartitionCount :: Lens.Lens' Limits Core.Natural
lMaximumPartitionCount = Lens.field @"maximumPartitionCount"
{-# INLINEABLE lMaximumPartitionCount #-}
{-# DEPRECATED maximumPartitionCount "Use generic-lens or generic-optics with 'maximumPartitionCount' instead"  #-}

instance Core.FromXML Limits where
        parseXML x
          = Limits' Core.<$>
              (x Core..@ "MaximumReplicationCount") Core.<*>
                x Core..@ "MaximumPartitionCount"
