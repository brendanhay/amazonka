{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.StepSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.StepSummary
  ( StepSummary (..),

    -- * Smart constructor
    mkStepSummary,

    -- * Lenses
    sActionOnFailure,
    sConfig,
    sId,
    sName,
    sStatus,
  )
where

import qualified Network.AWS.EMR.Types.ActionOnFailure as Types
import qualified Network.AWS.EMR.Types.HadoopStepConfig as Types
import qualified Network.AWS.EMR.Types.StepId as Types
import qualified Network.AWS.EMR.Types.StepStatus as Types
import qualified Network.AWS.EMR.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The summary of the cluster step.
--
-- /See:/ 'mkStepSummary' smart constructor.
data StepSummary = StepSummary'
  { -- | The action to take when the cluster step fails. Possible values are TERMINATE_CLUSTER, CANCEL_AND_WAIT, and CONTINUE. TERMINATE_JOB_FLOW is available for backward compatibility. We recommend using TERMINATE_CLUSTER instead.
    actionOnFailure :: Core.Maybe Types.ActionOnFailure,
    -- | The Hadoop job configuration of the cluster step.
    config :: Core.Maybe Types.HadoopStepConfig,
    -- | The identifier of the cluster step.
    id :: Core.Maybe Types.StepId,
    -- | The name of the cluster step.
    name :: Core.Maybe Types.String,
    -- | The current execution status details of the cluster step.
    status :: Core.Maybe Types.StepStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'StepSummary' value with any optional fields omitted.
mkStepSummary ::
  StepSummary
mkStepSummary =
  StepSummary'
    { actionOnFailure = Core.Nothing,
      config = Core.Nothing,
      id = Core.Nothing,
      name = Core.Nothing,
      status = Core.Nothing
    }

-- | The action to take when the cluster step fails. Possible values are TERMINATE_CLUSTER, CANCEL_AND_WAIT, and CONTINUE. TERMINATE_JOB_FLOW is available for backward compatibility. We recommend using TERMINATE_CLUSTER instead.
--
-- /Note:/ Consider using 'actionOnFailure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sActionOnFailure :: Lens.Lens' StepSummary (Core.Maybe Types.ActionOnFailure)
sActionOnFailure = Lens.field @"actionOnFailure"
{-# DEPRECATED sActionOnFailure "Use generic-lens or generic-optics with 'actionOnFailure' instead." #-}

-- | The Hadoop job configuration of the cluster step.
--
-- /Note:/ Consider using 'config' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sConfig :: Lens.Lens' StepSummary (Core.Maybe Types.HadoopStepConfig)
sConfig = Lens.field @"config"
{-# DEPRECATED sConfig "Use generic-lens or generic-optics with 'config' instead." #-}

-- | The identifier of the cluster step.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sId :: Lens.Lens' StepSummary (Core.Maybe Types.StepId)
sId = Lens.field @"id"
{-# DEPRECATED sId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The name of the cluster step.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sName :: Lens.Lens' StepSummary (Core.Maybe Types.String)
sName = Lens.field @"name"
{-# DEPRECATED sName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The current execution status details of the cluster step.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStatus :: Lens.Lens' StepSummary (Core.Maybe Types.StepStatus)
sStatus = Lens.field @"status"
{-# DEPRECATED sStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON StepSummary where
  parseJSON =
    Core.withObject "StepSummary" Core.$
      \x ->
        StepSummary'
          Core.<$> (x Core..:? "ActionOnFailure")
          Core.<*> (x Core..:? "Config")
          Core.<*> (x Core..:? "Id")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "Status")
