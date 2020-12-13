{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.RemediationExecutionStep
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.RemediationExecutionStep
  ( RemediationExecutionStep (..),

    -- * Smart constructor
    mkRemediationExecutionStep,

    -- * Lenses
    resState,
    resStartTime,
    resName,
    resStopTime,
    resErrorMessage,
  )
where

import Network.AWS.Config.Types.RemediationExecutionStepState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Name of the step from the SSM document.
--
-- /See:/ 'mkRemediationExecutionStep' smart constructor.
data RemediationExecutionStep = RemediationExecutionStep'
  { -- | The valid status of the step.
    state :: Lude.Maybe RemediationExecutionStepState,
    -- | The time when the step started.
    startTime :: Lude.Maybe Lude.Timestamp,
    -- | The details of the step.
    name :: Lude.Maybe Lude.Text,
    -- | The time when the step stopped.
    stopTime :: Lude.Maybe Lude.Timestamp,
    -- | An error message if the step was interrupted during execution.
    errorMessage :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemediationExecutionStep' with the minimum fields required to make a request.
--
-- * 'state' - The valid status of the step.
-- * 'startTime' - The time when the step started.
-- * 'name' - The details of the step.
-- * 'stopTime' - The time when the step stopped.
-- * 'errorMessage' - An error message if the step was interrupted during execution.
mkRemediationExecutionStep ::
  RemediationExecutionStep
mkRemediationExecutionStep =
  RemediationExecutionStep'
    { state = Lude.Nothing,
      startTime = Lude.Nothing,
      name = Lude.Nothing,
      stopTime = Lude.Nothing,
      errorMessage = Lude.Nothing
    }

-- | The valid status of the step.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
resState :: Lens.Lens' RemediationExecutionStep (Lude.Maybe RemediationExecutionStepState)
resState = Lens.lens (state :: RemediationExecutionStep -> Lude.Maybe RemediationExecutionStepState) (\s a -> s {state = a} :: RemediationExecutionStep)
{-# DEPRECATED resState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The time when the step started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
resStartTime :: Lens.Lens' RemediationExecutionStep (Lude.Maybe Lude.Timestamp)
resStartTime = Lens.lens (startTime :: RemediationExecutionStep -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: RemediationExecutionStep)
{-# DEPRECATED resStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The details of the step.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
resName :: Lens.Lens' RemediationExecutionStep (Lude.Maybe Lude.Text)
resName = Lens.lens (name :: RemediationExecutionStep -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: RemediationExecutionStep)
{-# DEPRECATED resName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The time when the step stopped.
--
-- /Note:/ Consider using 'stopTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
resStopTime :: Lens.Lens' RemediationExecutionStep (Lude.Maybe Lude.Timestamp)
resStopTime = Lens.lens (stopTime :: RemediationExecutionStep -> Lude.Maybe Lude.Timestamp) (\s a -> s {stopTime = a} :: RemediationExecutionStep)
{-# DEPRECATED resStopTime "Use generic-lens or generic-optics with 'stopTime' instead." #-}

-- | An error message if the step was interrupted during execution.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
resErrorMessage :: Lens.Lens' RemediationExecutionStep (Lude.Maybe Lude.Text)
resErrorMessage = Lens.lens (errorMessage :: RemediationExecutionStep -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: RemediationExecutionStep)
{-# DEPRECATED resErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

instance Lude.FromJSON RemediationExecutionStep where
  parseJSON =
    Lude.withObject
      "RemediationExecutionStep"
      ( \x ->
          RemediationExecutionStep'
            Lude.<$> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "StartTime")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "StopTime")
            Lude.<*> (x Lude..:? "ErrorMessage")
      )
