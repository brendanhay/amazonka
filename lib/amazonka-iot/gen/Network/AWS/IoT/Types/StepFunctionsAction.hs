{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.StepFunctionsAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.StepFunctionsAction
  ( StepFunctionsAction (..)
  -- * Smart constructor
  , mkStepFunctionsAction
  -- * Lenses
  , sfaStateMachineName
  , sfaRoleArn
  , sfaExecutionNamePrefix
  ) where

import qualified Network.AWS.IoT.Types.AwsArn as Types
import qualified Network.AWS.IoT.Types.ExecutionNamePrefix as Types
import qualified Network.AWS.IoT.Types.StateMachineName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Starts execution of a Step Functions state machine.
--
-- /See:/ 'mkStepFunctionsAction' smart constructor.
data StepFunctionsAction = StepFunctionsAction'
  { stateMachineName :: Types.StateMachineName
    -- ^ The name of the Step Functions state machine whose execution will be started.
  , roleArn :: Types.AwsArn
    -- ^ The ARN of the role that grants IoT permission to start execution of a state machine ("Action":"states:StartExecution").
  , executionNamePrefix :: Core.Maybe Types.ExecutionNamePrefix
    -- ^ (Optional) A name will be given to the state machine execution consisting of this prefix followed by a UUID. Step Functions automatically creates a unique name for each state machine execution if one is not provided.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StepFunctionsAction' value with any optional fields omitted.
mkStepFunctionsAction
    :: Types.StateMachineName -- ^ 'stateMachineName'
    -> Types.AwsArn -- ^ 'roleArn'
    -> StepFunctionsAction
mkStepFunctionsAction stateMachineName roleArn
  = StepFunctionsAction'{stateMachineName, roleArn,
                         executionNamePrefix = Core.Nothing}

-- | The name of the Step Functions state machine whose execution will be started.
--
-- /Note:/ Consider using 'stateMachineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfaStateMachineName :: Lens.Lens' StepFunctionsAction Types.StateMachineName
sfaStateMachineName = Lens.field @"stateMachineName"
{-# INLINEABLE sfaStateMachineName #-}
{-# DEPRECATED stateMachineName "Use generic-lens or generic-optics with 'stateMachineName' instead"  #-}

-- | The ARN of the role that grants IoT permission to start execution of a state machine ("Action":"states:StartExecution").
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfaRoleArn :: Lens.Lens' StepFunctionsAction Types.AwsArn
sfaRoleArn = Lens.field @"roleArn"
{-# INLINEABLE sfaRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | (Optional) A name will be given to the state machine execution consisting of this prefix followed by a UUID. Step Functions automatically creates a unique name for each state machine execution if one is not provided.
--
-- /Note:/ Consider using 'executionNamePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfaExecutionNamePrefix :: Lens.Lens' StepFunctionsAction (Core.Maybe Types.ExecutionNamePrefix)
sfaExecutionNamePrefix = Lens.field @"executionNamePrefix"
{-# INLINEABLE sfaExecutionNamePrefix #-}
{-# DEPRECATED executionNamePrefix "Use generic-lens or generic-optics with 'executionNamePrefix' instead"  #-}

instance Core.FromJSON StepFunctionsAction where
        toJSON StepFunctionsAction{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("stateMachineName" Core..= stateMachineName),
                  Core.Just ("roleArn" Core..= roleArn),
                  ("executionNamePrefix" Core..=) Core.<$> executionNamePrefix])

instance Core.FromJSON StepFunctionsAction where
        parseJSON
          = Core.withObject "StepFunctionsAction" Core.$
              \ x ->
                StepFunctionsAction' Core.<$>
                  (x Core..: "stateMachineName") Core.<*> x Core..: "roleArn"
                    Core.<*> x Core..:? "executionNamePrefix"
