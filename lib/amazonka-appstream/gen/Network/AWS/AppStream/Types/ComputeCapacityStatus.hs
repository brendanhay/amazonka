{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.ComputeCapacityStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.ComputeCapacityStatus
  ( ComputeCapacityStatus (..),

    -- * Smart constructor
    mkComputeCapacityStatus,

    -- * Lenses
    ccsDesired,
    ccsAvailable,
    ccsInUse,
    ccsRunning,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the capacity status for a fleet.
--
-- /See:/ 'mkComputeCapacityStatus' smart constructor.
data ComputeCapacityStatus = ComputeCapacityStatus'
  { -- | The desired number of streaming instances.
    desired :: Core.Int,
    -- | The number of currently available instances that can be used to stream sessions.
    available :: Core.Maybe Core.Int,
    -- | The number of instances in use for streaming.
    inUse :: Core.Maybe Core.Int,
    -- | The total number of simultaneous streaming instances that are running.
    running :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ComputeCapacityStatus' value with any optional fields omitted.
mkComputeCapacityStatus ::
  -- | 'desired'
  Core.Int ->
  ComputeCapacityStatus
mkComputeCapacityStatus desired =
  ComputeCapacityStatus'
    { desired,
      available = Core.Nothing,
      inUse = Core.Nothing,
      running = Core.Nothing
    }

-- | The desired number of streaming instances.
--
-- /Note:/ Consider using 'desired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsDesired :: Lens.Lens' ComputeCapacityStatus Core.Int
ccsDesired = Lens.field @"desired"
{-# DEPRECATED ccsDesired "Use generic-lens or generic-optics with 'desired' instead." #-}

-- | The number of currently available instances that can be used to stream sessions.
--
-- /Note:/ Consider using 'available' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsAvailable :: Lens.Lens' ComputeCapacityStatus (Core.Maybe Core.Int)
ccsAvailable = Lens.field @"available"
{-# DEPRECATED ccsAvailable "Use generic-lens or generic-optics with 'available' instead." #-}

-- | The number of instances in use for streaming.
--
-- /Note:/ Consider using 'inUse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsInUse :: Lens.Lens' ComputeCapacityStatus (Core.Maybe Core.Int)
ccsInUse = Lens.field @"inUse"
{-# DEPRECATED ccsInUse "Use generic-lens or generic-optics with 'inUse' instead." #-}

-- | The total number of simultaneous streaming instances that are running.
--
-- /Note:/ Consider using 'running' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsRunning :: Lens.Lens' ComputeCapacityStatus (Core.Maybe Core.Int)
ccsRunning = Lens.field @"running"
{-# DEPRECATED ccsRunning "Use generic-lens or generic-optics with 'running' instead." #-}

instance Core.FromJSON ComputeCapacityStatus where
  parseJSON =
    Core.withObject "ComputeCapacityStatus" Core.$
      \x ->
        ComputeCapacityStatus'
          Core.<$> (x Core..: "Desired")
          Core.<*> (x Core..:? "Available")
          Core.<*> (x Core..:? "InUse")
          Core.<*> (x Core..:? "Running")
