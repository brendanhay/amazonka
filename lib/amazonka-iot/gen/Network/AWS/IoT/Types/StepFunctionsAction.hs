{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.StepFunctionsAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.StepFunctionsAction
  ( StepFunctionsAction (..),

    -- * Smart constructor
    mkStepFunctionsAction,

    -- * Lenses
    sfaExecutionNamePrefix,
    sfaStateMachineName,
    sfaRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Starts execution of a Step Functions state machine.
--
-- /See:/ 'mkStepFunctionsAction' smart constructor.
data StepFunctionsAction = StepFunctionsAction'
  { executionNamePrefix ::
      Lude.Maybe Lude.Text,
    stateMachineName :: Lude.Text,
    roleARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StepFunctionsAction' with the minimum fields required to make a request.
--
-- * 'executionNamePrefix' - (Optional) A name will be given to the state machine execution consisting of this prefix followed by a UUID. Step Functions automatically creates a unique name for each state machine execution if one is not provided.
-- * 'roleARN' - The ARN of the role that grants IoT permission to start execution of a state machine ("Action":"states:StartExecution").
-- * 'stateMachineName' - The name of the Step Functions state machine whose execution will be started.
mkStepFunctionsAction ::
  -- | 'stateMachineName'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  StepFunctionsAction
mkStepFunctionsAction pStateMachineName_ pRoleARN_ =
  StepFunctionsAction'
    { executionNamePrefix = Lude.Nothing,
      stateMachineName = pStateMachineName_,
      roleARN = pRoleARN_
    }

-- | (Optional) A name will be given to the state machine execution consisting of this prefix followed by a UUID. Step Functions automatically creates a unique name for each state machine execution if one is not provided.
--
-- /Note:/ Consider using 'executionNamePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfaExecutionNamePrefix :: Lens.Lens' StepFunctionsAction (Lude.Maybe Lude.Text)
sfaExecutionNamePrefix = Lens.lens (executionNamePrefix :: StepFunctionsAction -> Lude.Maybe Lude.Text) (\s a -> s {executionNamePrefix = a} :: StepFunctionsAction)
{-# DEPRECATED sfaExecutionNamePrefix "Use generic-lens or generic-optics with 'executionNamePrefix' instead." #-}

-- | The name of the Step Functions state machine whose execution will be started.
--
-- /Note:/ Consider using 'stateMachineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfaStateMachineName :: Lens.Lens' StepFunctionsAction Lude.Text
sfaStateMachineName = Lens.lens (stateMachineName :: StepFunctionsAction -> Lude.Text) (\s a -> s {stateMachineName = a} :: StepFunctionsAction)
{-# DEPRECATED sfaStateMachineName "Use generic-lens or generic-optics with 'stateMachineName' instead." #-}

-- | The ARN of the role that grants IoT permission to start execution of a state machine ("Action":"states:StartExecution").
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfaRoleARN :: Lens.Lens' StepFunctionsAction Lude.Text
sfaRoleARN = Lens.lens (roleARN :: StepFunctionsAction -> Lude.Text) (\s a -> s {roleARN = a} :: StepFunctionsAction)
{-# DEPRECATED sfaRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON StepFunctionsAction where
  parseJSON =
    Lude.withObject
      "StepFunctionsAction"
      ( \x ->
          StepFunctionsAction'
            Lude.<$> (x Lude..:? "executionNamePrefix")
            Lude.<*> (x Lude..: "stateMachineName")
            Lude.<*> (x Lude..: "roleArn")
      )

instance Lude.ToJSON StepFunctionsAction where
  toJSON StepFunctionsAction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("executionNamePrefix" Lude..=) Lude.<$> executionNamePrefix,
            Lude.Just ("stateMachineName" Lude..= stateMachineName),
            Lude.Just ("roleArn" Lude..= roleARN)
          ]
      )
