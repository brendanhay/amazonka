{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EMR.Types.StepSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.StepSummary where

import Network.AWS.EMR.Types.ActionOnFailure
import Network.AWS.EMR.Types.HadoopStepConfig
import Network.AWS.EMR.Types.StepStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The summary of the cluster step.
--
-- /See:/ 'newStepSummary' smart constructor.
data StepSummary = StepSummary'
  { -- | The current execution status details of the cluster step.
    status :: Prelude.Maybe StepStatus,
    -- | The identifier of the cluster step.
    id :: Prelude.Maybe Prelude.Text,
    -- | The Hadoop job configuration of the cluster step.
    config :: Prelude.Maybe HadoopStepConfig,
    -- | The action to take when the cluster step fails. Possible values are
    -- TERMINATE_CLUSTER, CANCEL_AND_WAIT, and CONTINUE. TERMINATE_JOB_FLOW is
    -- available for backward compatibility. We recommend using
    -- TERMINATE_CLUSTER instead.
    actionOnFailure :: Prelude.Maybe ActionOnFailure,
    -- | The name of the cluster step.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StepSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'stepSummary_status' - The current execution status details of the cluster step.
--
-- 'id', 'stepSummary_id' - The identifier of the cluster step.
--
-- 'config', 'stepSummary_config' - The Hadoop job configuration of the cluster step.
--
-- 'actionOnFailure', 'stepSummary_actionOnFailure' - The action to take when the cluster step fails. Possible values are
-- TERMINATE_CLUSTER, CANCEL_AND_WAIT, and CONTINUE. TERMINATE_JOB_FLOW is
-- available for backward compatibility. We recommend using
-- TERMINATE_CLUSTER instead.
--
-- 'name', 'stepSummary_name' - The name of the cluster step.
newStepSummary ::
  StepSummary
newStepSummary =
  StepSummary'
    { status = Prelude.Nothing,
      id = Prelude.Nothing,
      config = Prelude.Nothing,
      actionOnFailure = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The current execution status details of the cluster step.
stepSummary_status :: Lens.Lens' StepSummary (Prelude.Maybe StepStatus)
stepSummary_status = Lens.lens (\StepSummary' {status} -> status) (\s@StepSummary' {} a -> s {status = a} :: StepSummary)

-- | The identifier of the cluster step.
stepSummary_id :: Lens.Lens' StepSummary (Prelude.Maybe Prelude.Text)
stepSummary_id = Lens.lens (\StepSummary' {id} -> id) (\s@StepSummary' {} a -> s {id = a} :: StepSummary)

-- | The Hadoop job configuration of the cluster step.
stepSummary_config :: Lens.Lens' StepSummary (Prelude.Maybe HadoopStepConfig)
stepSummary_config = Lens.lens (\StepSummary' {config} -> config) (\s@StepSummary' {} a -> s {config = a} :: StepSummary)

-- | The action to take when the cluster step fails. Possible values are
-- TERMINATE_CLUSTER, CANCEL_AND_WAIT, and CONTINUE. TERMINATE_JOB_FLOW is
-- available for backward compatibility. We recommend using
-- TERMINATE_CLUSTER instead.
stepSummary_actionOnFailure :: Lens.Lens' StepSummary (Prelude.Maybe ActionOnFailure)
stepSummary_actionOnFailure = Lens.lens (\StepSummary' {actionOnFailure} -> actionOnFailure) (\s@StepSummary' {} a -> s {actionOnFailure = a} :: StepSummary)

-- | The name of the cluster step.
stepSummary_name :: Lens.Lens' StepSummary (Prelude.Maybe Prelude.Text)
stepSummary_name = Lens.lens (\StepSummary' {name} -> name) (\s@StepSummary' {} a -> s {name = a} :: StepSummary)

instance Prelude.FromJSON StepSummary where
  parseJSON =
    Prelude.withObject
      "StepSummary"
      ( \x ->
          StepSummary'
            Prelude.<$> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "Id")
            Prelude.<*> (x Prelude..:? "Config")
            Prelude.<*> (x Prelude..:? "ActionOnFailure")
            Prelude.<*> (x Prelude..:? "Name")
      )

instance Prelude.Hashable StepSummary

instance Prelude.NFData StepSummary
