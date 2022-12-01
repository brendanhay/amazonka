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
-- Module      : Amazonka.EMR.Types.StepSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.StepSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EMR.Types.ActionOnFailure
import Amazonka.EMR.Types.HadoopStepConfig
import Amazonka.EMR.Types.StepStatus
import qualified Amazonka.Prelude as Prelude

-- | The summary of the cluster step.
--
-- /See:/ 'newStepSummary' smart constructor.
data StepSummary = StepSummary'
  { -- | The name of the cluster step.
    name :: Prelude.Maybe Prelude.Text,
    -- | The current execution status details of the cluster step.
    status :: Prelude.Maybe StepStatus,
    -- | The identifier of the cluster step.
    id :: Prelude.Maybe Prelude.Text,
    -- | The action to take when the cluster step fails. Possible values are
    -- TERMINATE_CLUSTER, CANCEL_AND_WAIT, and CONTINUE. TERMINATE_JOB_FLOW is
    -- available for backward compatibility.
    actionOnFailure :: Prelude.Maybe ActionOnFailure,
    -- | The Hadoop job configuration of the cluster step.
    config :: Prelude.Maybe HadoopStepConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StepSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'stepSummary_name' - The name of the cluster step.
--
-- 'status', 'stepSummary_status' - The current execution status details of the cluster step.
--
-- 'id', 'stepSummary_id' - The identifier of the cluster step.
--
-- 'actionOnFailure', 'stepSummary_actionOnFailure' - The action to take when the cluster step fails. Possible values are
-- TERMINATE_CLUSTER, CANCEL_AND_WAIT, and CONTINUE. TERMINATE_JOB_FLOW is
-- available for backward compatibility.
--
-- 'config', 'stepSummary_config' - The Hadoop job configuration of the cluster step.
newStepSummary ::
  StepSummary
newStepSummary =
  StepSummary'
    { name = Prelude.Nothing,
      status = Prelude.Nothing,
      id = Prelude.Nothing,
      actionOnFailure = Prelude.Nothing,
      config = Prelude.Nothing
    }

-- | The name of the cluster step.
stepSummary_name :: Lens.Lens' StepSummary (Prelude.Maybe Prelude.Text)
stepSummary_name = Lens.lens (\StepSummary' {name} -> name) (\s@StepSummary' {} a -> s {name = a} :: StepSummary)

-- | The current execution status details of the cluster step.
stepSummary_status :: Lens.Lens' StepSummary (Prelude.Maybe StepStatus)
stepSummary_status = Lens.lens (\StepSummary' {status} -> status) (\s@StepSummary' {} a -> s {status = a} :: StepSummary)

-- | The identifier of the cluster step.
stepSummary_id :: Lens.Lens' StepSummary (Prelude.Maybe Prelude.Text)
stepSummary_id = Lens.lens (\StepSummary' {id} -> id) (\s@StepSummary' {} a -> s {id = a} :: StepSummary)

-- | The action to take when the cluster step fails. Possible values are
-- TERMINATE_CLUSTER, CANCEL_AND_WAIT, and CONTINUE. TERMINATE_JOB_FLOW is
-- available for backward compatibility.
stepSummary_actionOnFailure :: Lens.Lens' StepSummary (Prelude.Maybe ActionOnFailure)
stepSummary_actionOnFailure = Lens.lens (\StepSummary' {actionOnFailure} -> actionOnFailure) (\s@StepSummary' {} a -> s {actionOnFailure = a} :: StepSummary)

-- | The Hadoop job configuration of the cluster step.
stepSummary_config :: Lens.Lens' StepSummary (Prelude.Maybe HadoopStepConfig)
stepSummary_config = Lens.lens (\StepSummary' {config} -> config) (\s@StepSummary' {} a -> s {config = a} :: StepSummary)

instance Core.FromJSON StepSummary where
  parseJSON =
    Core.withObject
      "StepSummary"
      ( \x ->
          StepSummary'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "ActionOnFailure")
            Prelude.<*> (x Core..:? "Config")
      )

instance Prelude.Hashable StepSummary where
  hashWithSalt _salt StepSummary' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` actionOnFailure
      `Prelude.hashWithSalt` config

instance Prelude.NFData StepSummary where
  rnf StepSummary' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf actionOnFailure
      `Prelude.seq` Prelude.rnf config
