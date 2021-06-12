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
-- Module      : Network.AWS.EMR.Types.Step
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.Step where

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types.ActionOnFailure
import Network.AWS.EMR.Types.HadoopStepConfig
import Network.AWS.EMR.Types.StepStatus
import qualified Network.AWS.Lens as Lens

-- | This represents a step in a cluster.
--
-- /See:/ 'newStep' smart constructor.
data Step = Step'
  { -- | The current execution status details of the cluster step.
    status :: Core.Maybe StepStatus,
    -- | The identifier of the cluster step.
    id :: Core.Maybe Core.Text,
    -- | The Hadoop job configuration of the cluster step.
    config :: Core.Maybe HadoopStepConfig,
    -- | The action to take when the cluster step fails. Possible values are
    -- TERMINATE_CLUSTER, CANCEL_AND_WAIT, and CONTINUE. TERMINATE_JOB_FLOW is
    -- provided for backward compatibility. We recommend using
    -- TERMINATE_CLUSTER instead.
    actionOnFailure :: Core.Maybe ActionOnFailure,
    -- | The name of the cluster step.
    name :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Step' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'step_status' - The current execution status details of the cluster step.
--
-- 'id', 'step_id' - The identifier of the cluster step.
--
-- 'config', 'step_config' - The Hadoop job configuration of the cluster step.
--
-- 'actionOnFailure', 'step_actionOnFailure' - The action to take when the cluster step fails. Possible values are
-- TERMINATE_CLUSTER, CANCEL_AND_WAIT, and CONTINUE. TERMINATE_JOB_FLOW is
-- provided for backward compatibility. We recommend using
-- TERMINATE_CLUSTER instead.
--
-- 'name', 'step_name' - The name of the cluster step.
newStep ::
  Step
newStep =
  Step'
    { status = Core.Nothing,
      id = Core.Nothing,
      config = Core.Nothing,
      actionOnFailure = Core.Nothing,
      name = Core.Nothing
    }

-- | The current execution status details of the cluster step.
step_status :: Lens.Lens' Step (Core.Maybe StepStatus)
step_status = Lens.lens (\Step' {status} -> status) (\s@Step' {} a -> s {status = a} :: Step)

-- | The identifier of the cluster step.
step_id :: Lens.Lens' Step (Core.Maybe Core.Text)
step_id = Lens.lens (\Step' {id} -> id) (\s@Step' {} a -> s {id = a} :: Step)

-- | The Hadoop job configuration of the cluster step.
step_config :: Lens.Lens' Step (Core.Maybe HadoopStepConfig)
step_config = Lens.lens (\Step' {config} -> config) (\s@Step' {} a -> s {config = a} :: Step)

-- | The action to take when the cluster step fails. Possible values are
-- TERMINATE_CLUSTER, CANCEL_AND_WAIT, and CONTINUE. TERMINATE_JOB_FLOW is
-- provided for backward compatibility. We recommend using
-- TERMINATE_CLUSTER instead.
step_actionOnFailure :: Lens.Lens' Step (Core.Maybe ActionOnFailure)
step_actionOnFailure = Lens.lens (\Step' {actionOnFailure} -> actionOnFailure) (\s@Step' {} a -> s {actionOnFailure = a} :: Step)

-- | The name of the cluster step.
step_name :: Lens.Lens' Step (Core.Maybe Core.Text)
step_name = Lens.lens (\Step' {name} -> name) (\s@Step' {} a -> s {name = a} :: Step)

instance Core.FromJSON Step where
  parseJSON =
    Core.withObject
      "Step"
      ( \x ->
          Step'
            Core.<$> (x Core..:? "Status")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "Config")
            Core.<*> (x Core..:? "ActionOnFailure")
            Core.<*> (x Core..:? "Name")
      )

instance Core.Hashable Step

instance Core.NFData Step
