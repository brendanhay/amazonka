{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.ComputeCapacityStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppStream.Types.ComputeCapacityStatus
  ( ComputeCapacityStatus (..)
  -- * Smart constructor
  , mkComputeCapacityStatus
  -- * Lenses
  , ccsDesired
  , ccsAvailable
  , ccsInUse
  , ccsRunning
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the capacity status for a fleet.
--
-- /See:/ 'mkComputeCapacityStatus' smart constructor.
data ComputeCapacityStatus = ComputeCapacityStatus'
  { desired :: Core.Int
    -- ^ The desired number of streaming instances.
  , available :: Core.Maybe Core.Int
    -- ^ The number of currently available instances that can be used to stream sessions.
  , inUse :: Core.Maybe Core.Int
    -- ^ The number of instances in use for streaming.
  , running :: Core.Maybe Core.Int
    -- ^ The total number of simultaneous streaming instances that are running.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ComputeCapacityStatus' value with any optional fields omitted.
mkComputeCapacityStatus
    :: Core.Int -- ^ 'desired'
    -> ComputeCapacityStatus
mkComputeCapacityStatus desired
  = ComputeCapacityStatus'{desired, available = Core.Nothing,
                           inUse = Core.Nothing, running = Core.Nothing}

-- | The desired number of streaming instances.
--
-- /Note:/ Consider using 'desired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsDesired :: Lens.Lens' ComputeCapacityStatus Core.Int
ccsDesired = Lens.field @"desired"
{-# INLINEABLE ccsDesired #-}
{-# DEPRECATED desired "Use generic-lens or generic-optics with 'desired' instead"  #-}

-- | The number of currently available instances that can be used to stream sessions.
--
-- /Note:/ Consider using 'available' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsAvailable :: Lens.Lens' ComputeCapacityStatus (Core.Maybe Core.Int)
ccsAvailable = Lens.field @"available"
{-# INLINEABLE ccsAvailable #-}
{-# DEPRECATED available "Use generic-lens or generic-optics with 'available' instead"  #-}

-- | The number of instances in use for streaming.
--
-- /Note:/ Consider using 'inUse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsInUse :: Lens.Lens' ComputeCapacityStatus (Core.Maybe Core.Int)
ccsInUse = Lens.field @"inUse"
{-# INLINEABLE ccsInUse #-}
{-# DEPRECATED inUse "Use generic-lens or generic-optics with 'inUse' instead"  #-}

-- | The total number of simultaneous streaming instances that are running.
--
-- /Note:/ Consider using 'running' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsRunning :: Lens.Lens' ComputeCapacityStatus (Core.Maybe Core.Int)
ccsRunning = Lens.field @"running"
{-# INLINEABLE ccsRunning #-}
{-# DEPRECATED running "Use generic-lens or generic-optics with 'running' instead"  #-}

instance Core.FromJSON ComputeCapacityStatus where
        parseJSON
          = Core.withObject "ComputeCapacityStatus" Core.$
              \ x ->
                ComputeCapacityStatus' Core.<$>
                  (x Core..: "Desired") Core.<*> x Core..:? "Available" Core.<*>
                    x Core..:? "InUse"
                    Core.<*> x Core..:? "Running"
