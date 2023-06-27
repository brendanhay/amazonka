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
-- Module      : Amazonka.IoT.Types.StepFunctionsAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.StepFunctionsAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Starts execution of a Step Functions state machine.
--
-- /See:/ 'newStepFunctionsAction' smart constructor.
data StepFunctionsAction = StepFunctionsAction'
  { -- | (Optional) A name will be given to the state machine execution
    -- consisting of this prefix followed by a UUID. Step Functions
    -- automatically creates a unique name for each state machine execution if
    -- one is not provided.
    executionNamePrefix :: Prelude.Maybe Prelude.Text,
    -- | The name of the Step Functions state machine whose execution will be
    -- started.
    stateMachineName :: Prelude.Text,
    -- | The ARN of the role that grants IoT permission to start execution of a
    -- state machine (\"Action\":\"states:StartExecution\").
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  StepFunctionsAction
newStepFunctionsAction pStateMachineName_ pRoleArn_ =
  StepFunctionsAction'
    { executionNamePrefix =
        Prelude.Nothing,
      stateMachineName = pStateMachineName_,
      roleArn = pRoleArn_
    }

-- | (Optional) A name will be given to the state machine execution
-- consisting of this prefix followed by a UUID. Step Functions
-- automatically creates a unique name for each state machine execution if
-- one is not provided.
stepFunctionsAction_executionNamePrefix :: Lens.Lens' StepFunctionsAction (Prelude.Maybe Prelude.Text)
stepFunctionsAction_executionNamePrefix = Lens.lens (\StepFunctionsAction' {executionNamePrefix} -> executionNamePrefix) (\s@StepFunctionsAction' {} a -> s {executionNamePrefix = a} :: StepFunctionsAction)

-- | The name of the Step Functions state machine whose execution will be
-- started.
stepFunctionsAction_stateMachineName :: Lens.Lens' StepFunctionsAction Prelude.Text
stepFunctionsAction_stateMachineName = Lens.lens (\StepFunctionsAction' {stateMachineName} -> stateMachineName) (\s@StepFunctionsAction' {} a -> s {stateMachineName = a} :: StepFunctionsAction)

-- | The ARN of the role that grants IoT permission to start execution of a
-- state machine (\"Action\":\"states:StartExecution\").
stepFunctionsAction_roleArn :: Lens.Lens' StepFunctionsAction Prelude.Text
stepFunctionsAction_roleArn = Lens.lens (\StepFunctionsAction' {roleArn} -> roleArn) (\s@StepFunctionsAction' {} a -> s {roleArn = a} :: StepFunctionsAction)

instance Data.FromJSON StepFunctionsAction where
  parseJSON =
    Data.withObject
      "StepFunctionsAction"
      ( \x ->
          StepFunctionsAction'
            Prelude.<$> (x Data..:? "executionNamePrefix")
            Prelude.<*> (x Data..: "stateMachineName")
            Prelude.<*> (x Data..: "roleArn")
      )

instance Prelude.Hashable StepFunctionsAction where
  hashWithSalt _salt StepFunctionsAction' {..} =
    _salt
      `Prelude.hashWithSalt` executionNamePrefix
      `Prelude.hashWithSalt` stateMachineName
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData StepFunctionsAction where
  rnf StepFunctionsAction' {..} =
    Prelude.rnf executionNamePrefix
      `Prelude.seq` Prelude.rnf stateMachineName
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToJSON StepFunctionsAction where
  toJSON StepFunctionsAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("executionNamePrefix" Data..=)
              Prelude.<$> executionNamePrefix,
            Prelude.Just
              ("stateMachineName" Data..= stateMachineName),
            Prelude.Just ("roleArn" Data..= roleArn)
          ]
      )
