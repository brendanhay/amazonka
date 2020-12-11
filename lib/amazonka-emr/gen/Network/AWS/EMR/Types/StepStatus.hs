-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.StepStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.StepStatus
  ( StepStatus (..),

    -- * Smart constructor
    mkStepStatus,

    -- * Lenses
    ssState,
    ssFailureDetails,
    ssStateChangeReason,
    ssTimeline,
  )
where

import Network.AWS.EMR.Types.FailureDetails
import Network.AWS.EMR.Types.StepState
import Network.AWS.EMR.Types.StepStateChangeReason
import Network.AWS.EMR.Types.StepTimeline
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The execution status details of the cluster step.
--
-- /See:/ 'mkStepStatus' smart constructor.
data StepStatus = StepStatus'
  { state :: Lude.Maybe StepState,
    failureDetails :: Lude.Maybe FailureDetails,
    stateChangeReason :: Lude.Maybe StepStateChangeReason,
    timeline :: Lude.Maybe StepTimeline
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StepStatus' with the minimum fields required to make a request.
--
-- * 'failureDetails' - The details for the step failure including reason, message, and log file path where the root cause was identified.
-- * 'state' - The execution state of the cluster step.
-- * 'stateChangeReason' - The reason for the step execution status change.
-- * 'timeline' - The timeline of the cluster step status over time.
mkStepStatus ::
  StepStatus
mkStepStatus =
  StepStatus'
    { state = Lude.Nothing,
      failureDetails = Lude.Nothing,
      stateChangeReason = Lude.Nothing,
      timeline = Lude.Nothing
    }

-- | The execution state of the cluster step.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssState :: Lens.Lens' StepStatus (Lude.Maybe StepState)
ssState = Lens.lens (state :: StepStatus -> Lude.Maybe StepState) (\s a -> s {state = a} :: StepStatus)
{-# DEPRECATED ssState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The details for the step failure including reason, message, and log file path where the root cause was identified.
--
-- /Note:/ Consider using 'failureDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssFailureDetails :: Lens.Lens' StepStatus (Lude.Maybe FailureDetails)
ssFailureDetails = Lens.lens (failureDetails :: StepStatus -> Lude.Maybe FailureDetails) (\s a -> s {failureDetails = a} :: StepStatus)
{-# DEPRECATED ssFailureDetails "Use generic-lens or generic-optics with 'failureDetails' instead." #-}

-- | The reason for the step execution status change.
--
-- /Note:/ Consider using 'stateChangeReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStateChangeReason :: Lens.Lens' StepStatus (Lude.Maybe StepStateChangeReason)
ssStateChangeReason = Lens.lens (stateChangeReason :: StepStatus -> Lude.Maybe StepStateChangeReason) (\s a -> s {stateChangeReason = a} :: StepStatus)
{-# DEPRECATED ssStateChangeReason "Use generic-lens or generic-optics with 'stateChangeReason' instead." #-}

-- | The timeline of the cluster step status over time.
--
-- /Note:/ Consider using 'timeline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssTimeline :: Lens.Lens' StepStatus (Lude.Maybe StepTimeline)
ssTimeline = Lens.lens (timeline :: StepStatus -> Lude.Maybe StepTimeline) (\s a -> s {timeline = a} :: StepStatus)
{-# DEPRECATED ssTimeline "Use generic-lens or generic-optics with 'timeline' instead." #-}

instance Lude.FromJSON StepStatus where
  parseJSON =
    Lude.withObject
      "StepStatus"
      ( \x ->
          StepStatus'
            Lude.<$> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "FailureDetails")
            Lude.<*> (x Lude..:? "StateChangeReason")
            Lude.<*> (x Lude..:? "Timeline")
      )
