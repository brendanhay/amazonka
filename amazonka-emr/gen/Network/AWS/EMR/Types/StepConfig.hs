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
-- Module      : Network.AWS.EMR.Types.StepConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.StepConfig where

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types.ActionOnFailure
import Network.AWS.EMR.Types.HadoopJarStepConfig
import qualified Network.AWS.Lens as Lens

-- | Specification of a cluster (job flow) step.
--
-- /See:/ 'newStepConfig' smart constructor.
data StepConfig = StepConfig'
  { -- | The action to take when the cluster step fails. Possible values are
    -- TERMINATE_CLUSTER, CANCEL_AND_WAIT, and CONTINUE. TERMINATE_JOB_FLOW is
    -- provided for backward compatibility. We recommend using
    -- TERMINATE_CLUSTER instead.
    actionOnFailure :: Core.Maybe ActionOnFailure,
    -- | The name of the step.
    name :: Core.Text,
    -- | The JAR file used for the step.
    hadoopJarStep :: HadoopJarStepConfig
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StepConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionOnFailure', 'stepConfig_actionOnFailure' - The action to take when the cluster step fails. Possible values are
-- TERMINATE_CLUSTER, CANCEL_AND_WAIT, and CONTINUE. TERMINATE_JOB_FLOW is
-- provided for backward compatibility. We recommend using
-- TERMINATE_CLUSTER instead.
--
-- 'name', 'stepConfig_name' - The name of the step.
--
-- 'hadoopJarStep', 'stepConfig_hadoopJarStep' - The JAR file used for the step.
newStepConfig ::
  -- | 'name'
  Core.Text ->
  -- | 'hadoopJarStep'
  HadoopJarStepConfig ->
  StepConfig
newStepConfig pName_ pHadoopJarStep_ =
  StepConfig'
    { actionOnFailure = Core.Nothing,
      name = pName_,
      hadoopJarStep = pHadoopJarStep_
    }

-- | The action to take when the cluster step fails. Possible values are
-- TERMINATE_CLUSTER, CANCEL_AND_WAIT, and CONTINUE. TERMINATE_JOB_FLOW is
-- provided for backward compatibility. We recommend using
-- TERMINATE_CLUSTER instead.
stepConfig_actionOnFailure :: Lens.Lens' StepConfig (Core.Maybe ActionOnFailure)
stepConfig_actionOnFailure = Lens.lens (\StepConfig' {actionOnFailure} -> actionOnFailure) (\s@StepConfig' {} a -> s {actionOnFailure = a} :: StepConfig)

-- | The name of the step.
stepConfig_name :: Lens.Lens' StepConfig Core.Text
stepConfig_name = Lens.lens (\StepConfig' {name} -> name) (\s@StepConfig' {} a -> s {name = a} :: StepConfig)

-- | The JAR file used for the step.
stepConfig_hadoopJarStep :: Lens.Lens' StepConfig HadoopJarStepConfig
stepConfig_hadoopJarStep = Lens.lens (\StepConfig' {hadoopJarStep} -> hadoopJarStep) (\s@StepConfig' {} a -> s {hadoopJarStep = a} :: StepConfig)

instance Core.Hashable StepConfig

instance Core.NFData StepConfig

instance Core.ToJSON StepConfig where
  toJSON StepConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ActionOnFailure" Core..=)
              Core.<$> actionOnFailure,
            Core.Just ("Name" Core..= name),
            Core.Just ("HadoopJarStep" Core..= hadoopJarStep)
          ]
      )
