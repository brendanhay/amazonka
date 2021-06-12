{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.StepFunctionsAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.StepFunctionsAction where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Starts execution of a Step Functions state machine.
--
-- /See:/ 'newStepFunctionsAction' smart constructor.
data StepFunctionsAction = StepFunctionsAction'
  { -- | (Optional) A name will be given to the state machine execution
    -- consisting of this prefix followed by a UUID. Step Functions
    -- automatically creates a unique name for each state machine execution if
    -- one is not provided.
    executionNamePrefix :: Core.Maybe Core.Text,
    -- | The name of the Step Functions state machine whose execution will be
    -- started.
    stateMachineName :: Core.Text,
    -- | The ARN of the role that grants IoT permission to start execution of a
    -- state machine (\"Action\":\"states:StartExecution\").
    roleArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StepFunctionsAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'executionNamePrefix', 'stepFunctionsAction_executionNamePrefix' - (Optional) A name will be given to the state machine execution
-- consisting of this prefix followed by a UUID. Step Functions
-- automatically creates a unique name for each state machine execution if
-- one is not provided.
--
-- 'stateMachineName', 'stepFunctionsAction_stateMachineName' - The name of the Step Functions state machine whose execution will be
-- started.
--
-- 'roleArn', 'stepFunctionsAction_roleArn' - The ARN of the role that grants IoT permission to start execution of a
-- state machine (\"Action\":\"states:StartExecution\").
newStepFunctionsAction ::
  -- | 'stateMachineName'
  Core.Text ->
  -- | 'roleArn'
  Core.Text ->
  StepFunctionsAction
newStepFunctionsAction pStateMachineName_ pRoleArn_ =
  StepFunctionsAction'
    { executionNamePrefix =
        Core.Nothing,
      stateMachineName = pStateMachineName_,
      roleArn = pRoleArn_
    }

-- | (Optional) A name will be given to the state machine execution
-- consisting of this prefix followed by a UUID. Step Functions
-- automatically creates a unique name for each state machine execution if
-- one is not provided.
stepFunctionsAction_executionNamePrefix :: Lens.Lens' StepFunctionsAction (Core.Maybe Core.Text)
stepFunctionsAction_executionNamePrefix = Lens.lens (\StepFunctionsAction' {executionNamePrefix} -> executionNamePrefix) (\s@StepFunctionsAction' {} a -> s {executionNamePrefix = a} :: StepFunctionsAction)

-- | The name of the Step Functions state machine whose execution will be
-- started.
stepFunctionsAction_stateMachineName :: Lens.Lens' StepFunctionsAction Core.Text
stepFunctionsAction_stateMachineName = Lens.lens (\StepFunctionsAction' {stateMachineName} -> stateMachineName) (\s@StepFunctionsAction' {} a -> s {stateMachineName = a} :: StepFunctionsAction)

-- | The ARN of the role that grants IoT permission to start execution of a
-- state machine (\"Action\":\"states:StartExecution\").
stepFunctionsAction_roleArn :: Lens.Lens' StepFunctionsAction Core.Text
stepFunctionsAction_roleArn = Lens.lens (\StepFunctionsAction' {roleArn} -> roleArn) (\s@StepFunctionsAction' {} a -> s {roleArn = a} :: StepFunctionsAction)

instance Core.FromJSON StepFunctionsAction where
  parseJSON =
    Core.withObject
      "StepFunctionsAction"
      ( \x ->
          StepFunctionsAction'
            Core.<$> (x Core..:? "executionNamePrefix")
            Core.<*> (x Core..: "stateMachineName")
            Core.<*> (x Core..: "roleArn")
      )

instance Core.Hashable StepFunctionsAction

instance Core.NFData StepFunctionsAction

instance Core.ToJSON StepFunctionsAction where
  toJSON StepFunctionsAction' {..} =
    Core.object
      ( Core.catMaybes
          [ ("executionNamePrefix" Core..=)
              Core.<$> executionNamePrefix,
            Core.Just
              ("stateMachineName" Core..= stateMachineName),
            Core.Just ("roleArn" Core..= roleArn)
          ]
      )
