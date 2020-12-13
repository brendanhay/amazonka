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
    ccsInUse,
    ccsDesired,
    ccsRunning,
    ccsAvailable,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the capacity status for a fleet.
--
-- /See:/ 'mkComputeCapacityStatus' smart constructor.
data ComputeCapacityStatus = ComputeCapacityStatus'
  { -- | The number of instances in use for streaming.
    inUse :: Lude.Maybe Lude.Int,
    -- | The desired number of streaming instances.
    desired :: Lude.Int,
    -- | The total number of simultaneous streaming instances that are running.
    running :: Lude.Maybe Lude.Int,
    -- | The number of currently available instances that can be used to stream sessions.
    available :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ComputeCapacityStatus' with the minimum fields required to make a request.
--
-- * 'inUse' - The number of instances in use for streaming.
-- * 'desired' - The desired number of streaming instances.
-- * 'running' - The total number of simultaneous streaming instances that are running.
-- * 'available' - The number of currently available instances that can be used to stream sessions.
mkComputeCapacityStatus ::
  -- | 'desired'
  Lude.Int ->
  ComputeCapacityStatus
mkComputeCapacityStatus pDesired_ =
  ComputeCapacityStatus'
    { inUse = Lude.Nothing,
      desired = pDesired_,
      running = Lude.Nothing,
      available = Lude.Nothing
    }

-- | The number of instances in use for streaming.
--
-- /Note:/ Consider using 'inUse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsInUse :: Lens.Lens' ComputeCapacityStatus (Lude.Maybe Lude.Int)
ccsInUse = Lens.lens (inUse :: ComputeCapacityStatus -> Lude.Maybe Lude.Int) (\s a -> s {inUse = a} :: ComputeCapacityStatus)
{-# DEPRECATED ccsInUse "Use generic-lens or generic-optics with 'inUse' instead." #-}

-- | The desired number of streaming instances.
--
-- /Note:/ Consider using 'desired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsDesired :: Lens.Lens' ComputeCapacityStatus Lude.Int
ccsDesired = Lens.lens (desired :: ComputeCapacityStatus -> Lude.Int) (\s a -> s {desired = a} :: ComputeCapacityStatus)
{-# DEPRECATED ccsDesired "Use generic-lens or generic-optics with 'desired' instead." #-}

-- | The total number of simultaneous streaming instances that are running.
--
-- /Note:/ Consider using 'running' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsRunning :: Lens.Lens' ComputeCapacityStatus (Lude.Maybe Lude.Int)
ccsRunning = Lens.lens (running :: ComputeCapacityStatus -> Lude.Maybe Lude.Int) (\s a -> s {running = a} :: ComputeCapacityStatus)
{-# DEPRECATED ccsRunning "Use generic-lens or generic-optics with 'running' instead." #-}

-- | The number of currently available instances that can be used to stream sessions.
--
-- /Note:/ Consider using 'available' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsAvailable :: Lens.Lens' ComputeCapacityStatus (Lude.Maybe Lude.Int)
ccsAvailable = Lens.lens (available :: ComputeCapacityStatus -> Lude.Maybe Lude.Int) (\s a -> s {available = a} :: ComputeCapacityStatus)
{-# DEPRECATED ccsAvailable "Use generic-lens or generic-optics with 'available' instead." #-}

instance Lude.FromJSON ComputeCapacityStatus where
  parseJSON =
    Lude.withObject
      "ComputeCapacityStatus"
      ( \x ->
          ComputeCapacityStatus'
            Lude.<$> (x Lude..:? "InUse")
            Lude.<*> (x Lude..: "Desired")
            Lude.<*> (x Lude..:? "Running")
            Lude.<*> (x Lude..:? "Available")
      )
