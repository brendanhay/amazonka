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
-- Module      : Amazonka.CodePipeline.Types.ActionTypeExecutor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.ActionTypeExecutor where

import Amazonka.CodePipeline.Types.ExecutorConfiguration
import Amazonka.CodePipeline.Types.ExecutorType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The action engine, or executor, for an action type created for a
-- provider, where the action is to be used by customers of the provider.
-- The action engine is associated with the model used to create and update
-- the action, such as the Lambda integration model.
--
-- /See:/ 'newActionTypeExecutor' smart constructor.
data ActionTypeExecutor = ActionTypeExecutor'
  { -- | The timeout in seconds for the job. An action execution can have
    -- multiple jobs. This is the timeout for a single job, not the entire
    -- action execution.
    jobTimeout :: Prelude.Maybe Prelude.Natural,
    -- | The policy statement that specifies the permissions in the CodePipeline
    -- customer’s account that are needed to successfully run an action.
    --
    -- To grant permission to another account, specify the account ID as the
    -- Principal, a domain-style identifier defined by the service, for example
    -- @codepipeline.amazonaws.com@.
    --
    -- The size of the passed JSON policy document cannot exceed 2048
    -- characters.
    policyStatementsTemplate :: Prelude.Maybe Prelude.Text,
    -- | The action configuration properties for the action type. These
    -- properties are specified in the action definition when the action type
    -- is created.
    configuration :: ExecutorConfiguration,
    -- | The integration model used to create and update the action type,
    -- @Lambda@ or @JobWorker@.
    type' :: ExecutorType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActionTypeExecutor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobTimeout', 'actionTypeExecutor_jobTimeout' - The timeout in seconds for the job. An action execution can have
-- multiple jobs. This is the timeout for a single job, not the entire
-- action execution.
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
    { jobTimeout = Prelude.Nothing,
      policyStatementsTemplate = Prelude.Nothing,
      configuration = pConfiguration_,
      type' = pType_
    }

-- | The timeout in seconds for the job. An action execution can have
-- multiple jobs. This is the timeout for a single job, not the entire
-- action execution.
actionTypeExecutor_jobTimeout :: Lens.Lens' ActionTypeExecutor (Prelude.Maybe Prelude.Natural)
actionTypeExecutor_jobTimeout = Lens.lens (\ActionTypeExecutor' {jobTimeout} -> jobTimeout) (\s@ActionTypeExecutor' {} a -> s {jobTimeout = a} :: ActionTypeExecutor)

-- | The policy statement that specifies the permissions in the CodePipeline
-- customer’s account that are needed to successfully run an action.
--
-- To grant permission to another account, specify the account ID as the
-- Principal, a domain-style identifier defined by the service, for example
-- @codepipeline.amazonaws.com@.
--
-- The size of the passed JSON policy document cannot exceed 2048
-- characters.
actionTypeExecutor_policyStatementsTemplate :: Lens.Lens' ActionTypeExecutor (Prelude.Maybe Prelude.Text)
actionTypeExecutor_policyStatementsTemplate = Lens.lens (\ActionTypeExecutor' {policyStatementsTemplate} -> policyStatementsTemplate) (\s@ActionTypeExecutor' {} a -> s {policyStatementsTemplate = a} :: ActionTypeExecutor)

-- | The action configuration properties for the action type. These
-- properties are specified in the action definition when the action type
-- is created.
actionTypeExecutor_configuration :: Lens.Lens' ActionTypeExecutor ExecutorConfiguration
actionTypeExecutor_configuration = Lens.lens (\ActionTypeExecutor' {configuration} -> configuration) (\s@ActionTypeExecutor' {} a -> s {configuration = a} :: ActionTypeExecutor)

-- | The integration model used to create and update the action type,
-- @Lambda@ or @JobWorker@.
actionTypeExecutor_type :: Lens.Lens' ActionTypeExecutor ExecutorType
actionTypeExecutor_type = Lens.lens (\ActionTypeExecutor' {type'} -> type') (\s@ActionTypeExecutor' {} a -> s {type' = a} :: ActionTypeExecutor)

instance Data.FromJSON ActionTypeExecutor where
  parseJSON =
    Data.withObject
      "ActionTypeExecutor"
      ( \x ->
          ActionTypeExecutor'
            Prelude.<$> (x Data..:? "jobTimeout")
            Prelude.<*> (x Data..:? "policyStatementsTemplate")
            Prelude.<*> (x Data..: "configuration")
            Prelude.<*> (x Data..: "type")
      )

instance Prelude.Hashable ActionTypeExecutor where
  hashWithSalt _salt ActionTypeExecutor' {..} =
    _salt
      `Prelude.hashWithSalt` jobTimeout
      `Prelude.hashWithSalt` policyStatementsTemplate
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` type'

instance Prelude.NFData ActionTypeExecutor where
  rnf ActionTypeExecutor' {..} =
    Prelude.rnf jobTimeout
      `Prelude.seq` Prelude.rnf policyStatementsTemplate
      `Prelude.seq` Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON ActionTypeExecutor where
  toJSON ActionTypeExecutor' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("jobTimeout" Data..=) Prelude.<$> jobTimeout,
            ("policyStatementsTemplate" Data..=)
              Prelude.<$> policyStatementsTemplate,
            Prelude.Just ("configuration" Data..= configuration),
            Prelude.Just ("type" Data..= type')
          ]
      )
