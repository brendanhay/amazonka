{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.Step
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.Step
  ( Step (..)
  -- * Smart constructor
  , mkStep
  -- * Lenses
  , sfActionOnFailure
  , sfConfig
  , sfId
  , sfName
  , sfStatus
  ) where

import qualified Network.AWS.EMR.Types.ActionOnFailure as Types
import qualified Network.AWS.EMR.Types.HadoopStepConfig as Types
import qualified Network.AWS.EMR.Types.StepId as Types
import qualified Network.AWS.EMR.Types.StepStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | This represents a step in a cluster.
--
-- /See:/ 'mkStep' smart constructor.
data Step = Step'
  { actionOnFailure :: Core.Maybe Types.ActionOnFailure
    -- ^ The action to take when the cluster step fails. Possible values are TERMINATE_CLUSTER, CANCEL_AND_WAIT, and CONTINUE. TERMINATE_JOB_FLOW is provided for backward compatibility. We recommend using TERMINATE_CLUSTER instead.
  , config :: Core.Maybe Types.HadoopStepConfig
    -- ^ The Hadoop job configuration of the cluster step.
  , id :: Core.Maybe Types.StepId
    -- ^ The identifier of the cluster step.
  , name :: Core.Maybe Core.Text
    -- ^ The name of the cluster step.
  , status :: Core.Maybe Types.StepStatus
    -- ^ The current execution status details of the cluster step.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Step' value with any optional fields omitted.
mkStep
    :: Step
mkStep
  = Step'{actionOnFailure = Core.Nothing, config = Core.Nothing,
          id = Core.Nothing, name = Core.Nothing, status = Core.Nothing}

-- | The action to take when the cluster step fails. Possible values are TERMINATE_CLUSTER, CANCEL_AND_WAIT, and CONTINUE. TERMINATE_JOB_FLOW is provided for backward compatibility. We recommend using TERMINATE_CLUSTER instead.
--
-- /Note:/ Consider using 'actionOnFailure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfActionOnFailure :: Lens.Lens' Step (Core.Maybe Types.ActionOnFailure)
sfActionOnFailure = Lens.field @"actionOnFailure"
{-# INLINEABLE sfActionOnFailure #-}
{-# DEPRECATED actionOnFailure "Use generic-lens or generic-optics with 'actionOnFailure' instead"  #-}

-- | The Hadoop job configuration of the cluster step.
--
-- /Note:/ Consider using 'config' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfConfig :: Lens.Lens' Step (Core.Maybe Types.HadoopStepConfig)
sfConfig = Lens.field @"config"
{-# INLINEABLE sfConfig #-}
{-# DEPRECATED config "Use generic-lens or generic-optics with 'config' instead"  #-}

-- | The identifier of the cluster step.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfId :: Lens.Lens' Step (Core.Maybe Types.StepId)
sfId = Lens.field @"id"
{-# INLINEABLE sfId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The name of the cluster step.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfName :: Lens.Lens' Step (Core.Maybe Core.Text)
sfName = Lens.field @"name"
{-# INLINEABLE sfName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The current execution status details of the cluster step.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfStatus :: Lens.Lens' Step (Core.Maybe Types.StepStatus)
sfStatus = Lens.field @"status"
{-# INLINEABLE sfStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromJSON Step where
        parseJSON
          = Core.withObject "Step" Core.$
              \ x ->
                Step' Core.<$>
                  (x Core..:? "ActionOnFailure") Core.<*> x Core..:? "Config"
                    Core.<*> x Core..:? "Id"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "Status"
