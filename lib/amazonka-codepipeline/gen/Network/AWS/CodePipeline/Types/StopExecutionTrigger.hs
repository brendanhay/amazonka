-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.StopExecutionTrigger
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.StopExecutionTrigger
  ( StopExecutionTrigger (..),

    -- * Smart constructor
    mkStopExecutionTrigger,

    -- * Lenses
    setReason,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The interaction that stopped a pipeline execution.
--
-- /See:/ 'mkStopExecutionTrigger' smart constructor.
newtype StopExecutionTrigger = StopExecutionTrigger'
  { reason ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopExecutionTrigger' with the minimum fields required to make a request.
--
-- * 'reason' - The user-specified reason the pipeline was stopped.
mkStopExecutionTrigger ::
  StopExecutionTrigger
mkStopExecutionTrigger =
  StopExecutionTrigger' {reason = Lude.Nothing}

-- | The user-specified reason the pipeline was stopped.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
setReason :: Lens.Lens' StopExecutionTrigger (Lude.Maybe Lude.Text)
setReason = Lens.lens (reason :: StopExecutionTrigger -> Lude.Maybe Lude.Text) (\s a -> s {reason = a} :: StopExecutionTrigger)
{-# DEPRECATED setReason "Use generic-lens or generic-optics with 'reason' instead." #-}

instance Lude.FromJSON StopExecutionTrigger where
  parseJSON =
    Lude.withObject
      "StopExecutionTrigger"
      (\x -> StopExecutionTrigger' Lude.<$> (x Lude..:? "reason"))
