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
-- Module      : Amazonka.EMR.Types.StepConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.StepConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types.ActionOnFailure
import Amazonka.EMR.Types.HadoopJarStepConfig
import qualified Amazonka.Prelude as Prelude

-- | Specification for a cluster (job flow) step.
--
-- /See:/ 'newStepConfig' smart constructor.
data StepConfig = StepConfig'
  { -- | The action to take when the step fails. Use one of the following values:
    --
    -- -   @TERMINATE_CLUSTER@ - Shuts down the cluster.
    --
    -- -   @CANCEL_AND_WAIT@ - Cancels any pending steps and returns the
    --     cluster to the @WAITING@ state.
    --
    -- -   @CONTINUE@ - Continues to the next step in the queue.
    --
    -- -   @TERMINATE_JOB_FLOW@ - Shuts down the cluster. @TERMINATE_JOB_FLOW@
    --     is provided for backward compatibility. We recommend using
    --     @TERMINATE_CLUSTER@ instead.
    --
    -- If a cluster\'s @StepConcurrencyLevel@ is greater than @1@, do not use
    -- @AddJobFlowSteps@ to submit a step with this parameter set to
    -- @CANCEL_AND_WAIT@ or @TERMINATE_CLUSTER@. The step is not submitted and
    -- the action fails with a message that the @ActionOnFailure@ setting is
    -- not valid.
    --
    -- If you change a cluster\'s @StepConcurrencyLevel@ to be greater than 1
    -- while a step is running, the @ActionOnFailure@ parameter may not behave
    -- as you expect. In this case, for a step that fails with this parameter
    -- set to @CANCEL_AND_WAIT@, pending steps and the running step are not
    -- canceled; for a step that fails with this parameter set to
    -- @TERMINATE_CLUSTER@, the cluster does not terminate.
    actionOnFailure :: Prelude.Maybe ActionOnFailure,
    -- | The name of the step.
    name :: Prelude.Text,
    -- | The JAR file used for the step.
    hadoopJarStep :: HadoopJarStepConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StepConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionOnFailure', 'stepConfig_actionOnFailure' - The action to take when the step fails. Use one of the following values:
--
-- -   @TERMINATE_CLUSTER@ - Shuts down the cluster.
--
-- -   @CANCEL_AND_WAIT@ - Cancels any pending steps and returns the
--     cluster to the @WAITING@ state.
--
-- -   @CONTINUE@ - Continues to the next step in the queue.
--
-- -   @TERMINATE_JOB_FLOW@ - Shuts down the cluster. @TERMINATE_JOB_FLOW@
--     is provided for backward compatibility. We recommend using
--     @TERMINATE_CLUSTER@ instead.
--
-- If a cluster\'s @StepConcurrencyLevel@ is greater than @1@, do not use
-- @AddJobFlowSteps@ to submit a step with this parameter set to
-- @CANCEL_AND_WAIT@ or @TERMINATE_CLUSTER@. The step is not submitted and
-- the action fails with a message that the @ActionOnFailure@ setting is
-- not valid.
--
-- If you change a cluster\'s @StepConcurrencyLevel@ to be greater than 1
-- while a step is running, the @ActionOnFailure@ parameter may not behave
-- as you expect. In this case, for a step that fails with this parameter
-- set to @CANCEL_AND_WAIT@, pending steps and the running step are not
-- canceled; for a step that fails with this parameter set to
-- @TERMINATE_CLUSTER@, the cluster does not terminate.
--
-- 'name', 'stepConfig_name' - The name of the step.
--
-- 'hadoopJarStep', 'stepConfig_hadoopJarStep' - The JAR file used for the step.
newStepConfig ::
  -- | 'name'
  Prelude.Text ->
  -- | 'hadoopJarStep'
  HadoopJarStepConfig ->
  StepConfig
newStepConfig pName_ pHadoopJarStep_ =
  StepConfig'
    { actionOnFailure = Prelude.Nothing,
      name = pName_,
      hadoopJarStep = pHadoopJarStep_
    }

-- | The action to take when the step fails. Use one of the following values:
--
-- -   @TERMINATE_CLUSTER@ - Shuts down the cluster.
--
-- -   @CANCEL_AND_WAIT@ - Cancels any pending steps and returns the
--     cluster to the @WAITING@ state.
--
-- -   @CONTINUE@ - Continues to the next step in the queue.
--
-- -   @TERMINATE_JOB_FLOW@ - Shuts down the cluster. @TERMINATE_JOB_FLOW@
--     is provided for backward compatibility. We recommend using
--     @TERMINATE_CLUSTER@ instead.
--
-- If a cluster\'s @StepConcurrencyLevel@ is greater than @1@, do not use
-- @AddJobFlowSteps@ to submit a step with this parameter set to
-- @CANCEL_AND_WAIT@ or @TERMINATE_CLUSTER@. The step is not submitted and
-- the action fails with a message that the @ActionOnFailure@ setting is
-- not valid.
--
-- If you change a cluster\'s @StepConcurrencyLevel@ to be greater than 1
-- while a step is running, the @ActionOnFailure@ parameter may not behave
-- as you expect. In this case, for a step that fails with this parameter
-- set to @CANCEL_AND_WAIT@, pending steps and the running step are not
-- canceled; for a step that fails with this parameter set to
-- @TERMINATE_CLUSTER@, the cluster does not terminate.
stepConfig_actionOnFailure :: Lens.Lens' StepConfig (Prelude.Maybe ActionOnFailure)
stepConfig_actionOnFailure = Lens.lens (\StepConfig' {actionOnFailure} -> actionOnFailure) (\s@StepConfig' {} a -> s {actionOnFailure = a} :: StepConfig)

-- | The name of the step.
stepConfig_name :: Lens.Lens' StepConfig Prelude.Text
stepConfig_name = Lens.lens (\StepConfig' {name} -> name) (\s@StepConfig' {} a -> s {name = a} :: StepConfig)

-- | The JAR file used for the step.
stepConfig_hadoopJarStep :: Lens.Lens' StepConfig HadoopJarStepConfig
stepConfig_hadoopJarStep = Lens.lens (\StepConfig' {hadoopJarStep} -> hadoopJarStep) (\s@StepConfig' {} a -> s {hadoopJarStep = a} :: StepConfig)

instance Prelude.Hashable StepConfig where
  hashWithSalt _salt StepConfig' {..} =
    _salt
      `Prelude.hashWithSalt` actionOnFailure
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` hadoopJarStep

instance Prelude.NFData StepConfig where
  rnf StepConfig' {..} =
    Prelude.rnf actionOnFailure
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf hadoopJarStep

instance Data.ToJSON StepConfig where
  toJSON StepConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ActionOnFailure" Data..=)
              Prelude.<$> actionOnFailure,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ("HadoopJarStep" Data..= hadoopJarStep)
          ]
      )
