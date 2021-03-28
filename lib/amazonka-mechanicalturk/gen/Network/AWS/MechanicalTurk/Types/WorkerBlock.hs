{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.WorkerBlock
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MechanicalTurk.Types.WorkerBlock
  ( WorkerBlock (..)
  -- * Smart constructor
  , mkWorkerBlock
  -- * Lenses
  , wbReason
  , wbWorkerId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types.WorkerId as Types
import qualified Network.AWS.Prelude as Core

-- | The WorkerBlock data structure represents a Worker who has been blocked. It has two elements: the WorkerId and the Reason for the block. 
--
-- /See:/ 'mkWorkerBlock' smart constructor.
data WorkerBlock = WorkerBlock'
  { reason :: Core.Maybe Core.Text
    -- ^ A message explaining the reason the Worker was blocked. 
  , workerId :: Core.Maybe Types.WorkerId
    -- ^ The ID of the Worker who accepted the HIT.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WorkerBlock' value with any optional fields omitted.
mkWorkerBlock
    :: WorkerBlock
mkWorkerBlock
  = WorkerBlock'{reason = Core.Nothing, workerId = Core.Nothing}

-- | A message explaining the reason the Worker was blocked. 
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wbReason :: Lens.Lens' WorkerBlock (Core.Maybe Core.Text)
wbReason = Lens.field @"reason"
{-# INLINEABLE wbReason #-}
{-# DEPRECATED reason "Use generic-lens or generic-optics with 'reason' instead"  #-}

-- | The ID of the Worker who accepted the HIT.
--
-- /Note:/ Consider using 'workerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wbWorkerId :: Lens.Lens' WorkerBlock (Core.Maybe Types.WorkerId)
wbWorkerId = Lens.field @"workerId"
{-# INLINEABLE wbWorkerId #-}
{-# DEPRECATED workerId "Use generic-lens or generic-optics with 'workerId' instead"  #-}

instance Core.FromJSON WorkerBlock where
        parseJSON
          = Core.withObject "WorkerBlock" Core.$
              \ x ->
                WorkerBlock' Core.<$>
                  (x Core..:? "Reason") Core.<*> x Core..:? "WorkerId"
