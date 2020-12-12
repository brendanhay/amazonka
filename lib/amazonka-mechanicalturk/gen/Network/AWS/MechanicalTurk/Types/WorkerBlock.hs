{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.WorkerBlock
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.WorkerBlock
  ( WorkerBlock (..),

    -- * Smart constructor
    mkWorkerBlock,

    -- * Lenses
    wbReason,
    wbWorkerId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The WorkerBlock data structure represents a Worker who has been blocked. It has two elements: the WorkerId and the Reason for the block.
--
-- /See:/ 'mkWorkerBlock' smart constructor.
data WorkerBlock = WorkerBlock'
  { reason :: Lude.Maybe Lude.Text,
    workerId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WorkerBlock' with the minimum fields required to make a request.
--
-- * 'reason' - A message explaining the reason the Worker was blocked.
-- * 'workerId' - The ID of the Worker who accepted the HIT.
mkWorkerBlock ::
  WorkerBlock
mkWorkerBlock =
  WorkerBlock' {reason = Lude.Nothing, workerId = Lude.Nothing}

-- | A message explaining the reason the Worker was blocked.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wbReason :: Lens.Lens' WorkerBlock (Lude.Maybe Lude.Text)
wbReason = Lens.lens (reason :: WorkerBlock -> Lude.Maybe Lude.Text) (\s a -> s {reason = a} :: WorkerBlock)
{-# DEPRECATED wbReason "Use generic-lens or generic-optics with 'reason' instead." #-}

-- | The ID of the Worker who accepted the HIT.
--
-- /Note:/ Consider using 'workerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wbWorkerId :: Lens.Lens' WorkerBlock (Lude.Maybe Lude.Text)
wbWorkerId = Lens.lens (workerId :: WorkerBlock -> Lude.Maybe Lude.Text) (\s a -> s {workerId = a} :: WorkerBlock)
{-# DEPRECATED wbWorkerId "Use generic-lens or generic-optics with 'workerId' instead." #-}

instance Lude.FromJSON WorkerBlock where
  parseJSON =
    Lude.withObject
      "WorkerBlock"
      ( \x ->
          WorkerBlock'
            Lude.<$> (x Lude..:? "Reason") Lude.<*> (x Lude..:? "WorkerId")
      )
