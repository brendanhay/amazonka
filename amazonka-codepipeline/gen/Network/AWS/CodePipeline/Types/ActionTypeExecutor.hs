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
-- Module      : Network.AWS.CodePipeline.Types.ActionTypeExecutor
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionTypeExecutor where

import Network.AWS.CodePipeline.Types.ExecutorConfiguration
import Network.AWS.CodePipeline.Types.ExecutorType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The action engine, or executor, for an action type created for a
-- provider, where the action is to be used by customers of the provider.
-- The action engine is associated with the model used to create and update
-- the action, such as the Lambda integration model.
--
-- /See:/ 'newActionTypeExecutor' smart constructor.
data ActionTypeExecutor = ActionTypeExecutor'
  { -- | The policy statement that specifies the permissions in the CodePipeline
    -- customer’s account that are needed to successfully run an action.
    --
    -- To grant permission to another account, specify the account ID as the
    -- Principal, a domain-style identifier defined by the service, for example
    -- @codepipeline.amazonaws.com@.
    --
    -- The size of the passed JSON policy document cannot exceed 2048
    -- characters.
    policyStatementsTemplate :: Core.Maybe Core.Text,
    -- | The timeout in seconds for the job. An action execution can have
    -- multiple jobs. This is the timeout for a single job, not the entire
    -- action execution.
    jobTimeout :: Core.Maybe Core.Natural,
    -- | The action configuration properties for the action type. These
    -- properties are specified in the action definition when the action type
    -- is created.
    configuration :: ExecutorConfiguration,
    -- | The integration model used to create and update the action type,
    -- @Lambda@ or @JobWorker@.
    type' :: ExecutorType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ActionTypeExecutor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyStatementsTemplate', 'actionTypeExecutor_policyStatementsTemplate' - The policy statement that specifies the permissions in the CodePipeline
-- customer’s account that are needed to successfully run an action.
--
-- To grant permission to another account, specify the account ID as the
-- Principal, a domain-style identifier defined by the service, for example
-- @codepipeline.amazonaws.com@.
--
-- The size of the passed JSON policy document cannot exceed 2048
-- characters.
--
-- 'jobTimeout', 'actionTypeExecutor_jobTimeout' - The timeout in seconds for the job. An action execution can have
-- multiple jobs. This is the timeout for a single job, not the entire
-- action execution.
--
-- 'configuration', 'actionTypeExecutor_configuration' - The action configuration properties for the action type. These
-- properties are specified in the action definition when the action type
-- is created.
--
-- 'type'', 'actionTypeExecutor_type' - The integration model used to create and update the action type,
-- @Lambda@ or @JobWorker@.
newActionTypeExecutor ::
  -- | 'configuration'
  ExecutorConfiguration ->
  -- | 'type''
  ExecutorType ->
  ActionTypeExecutor
newActionTypeExecutor pConfiguration_ pType_ =
  ActionTypeExecutor'
    { policyStatementsTemplate =
        Core.Nothing,
      jobTimeout = Core.Nothing,
      configuration = pConfiguration_,
      type' = pType_
    }

-- | The policy statement that specifies the permissions in the CodePipeline
-- customer’s account that are needed to successfully run an action.
--
-- To grant permission to another account, specify the account ID as the
-- Principal, a domain-style identifier defined by the service, for example
-- @codepipeline.amazonaws.com@.
--
-- The size of the passed JSON policy document cannot exceed 2048
-- characters.
actionTypeExecutor_policyStatementsTemplate :: Lens.Lens' ActionTypeExecutor (Core.Maybe Core.Text)
actionTypeExecutor_policyStatementsTemplate = Lens.lens (\ActionTypeExecutor' {policyStatementsTemplate} -> policyStatementsTemplate) (\s@ActionTypeExecutor' {} a -> s {policyStatementsTemplate = a} :: ActionTypeExecutor)

-- | The timeout in seconds for the job. An action execution can have
-- multiple jobs. This is the timeout for a single job, not the entire
-- action execution.
actionTypeExecutor_jobTimeout :: Lens.Lens' ActionTypeExecutor (Core.Maybe Core.Natural)
actionTypeExecutor_jobTimeout = Lens.lens (\ActionTypeExecutor' {jobTimeout} -> jobTimeout) (\s@ActionTypeExecutor' {} a -> s {jobTimeout = a} :: ActionTypeExecutor)

-- | The action configuration properties for the action type. These
-- properties are specified in the action definition when the action type
-- is created.
actionTypeExecutor_configuration :: Lens.Lens' ActionTypeExecutor ExecutorConfiguration
actionTypeExecutor_configuration = Lens.lens (\ActionTypeExecutor' {configuration} -> configuration) (\s@ActionTypeExecutor' {} a -> s {configuration = a} :: ActionTypeExecutor)

-- | The integration model used to create and update the action type,
-- @Lambda@ or @JobWorker@.
actionTypeExecutor_type :: Lens.Lens' ActionTypeExecutor ExecutorType
actionTypeExecutor_type = Lens.lens (\ActionTypeExecutor' {type'} -> type') (\s@ActionTypeExecutor' {} a -> s {type' = a} :: ActionTypeExecutor)

instance Core.FromJSON ActionTypeExecutor where
  parseJSON =
    Core.withObject
      "ActionTypeExecutor"
      ( \x ->
          ActionTypeExecutor'
            Core.<$> (x Core..:? "policyStatementsTemplate")
            Core.<*> (x Core..:? "jobTimeout")
            Core.<*> (x Core..: "configuration")
            Core.<*> (x Core..: "type")
      )

instance Core.Hashable ActionTypeExecutor

instance Core.NFData ActionTypeExecutor

instance Core.ToJSON ActionTypeExecutor where
  toJSON ActionTypeExecutor' {..} =
    Core.object
      ( Core.catMaybes
          [ ("policyStatementsTemplate" Core..=)
              Core.<$> policyStatementsTemplate,
            ("jobTimeout" Core..=) Core.<$> jobTimeout,
            Core.Just ("configuration" Core..= configuration),
            Core.Just ("type" Core..= type')
          ]
      )
