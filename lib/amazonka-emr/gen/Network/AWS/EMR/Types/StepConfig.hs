{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.StepConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.StepConfig
  ( StepConfig (..)
  -- * Smart constructor
  , mkStepConfig
  -- * Lenses
  , scName
  , scHadoopJarStep
  , scActionOnFailure
  ) where

import qualified Network.AWS.EMR.Types.ActionOnFailure as Types
import qualified Network.AWS.EMR.Types.HadoopJarStepConfig as Types
import qualified Network.AWS.EMR.Types.XmlStringMaxLen256 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specification of a cluster (job flow) step.
--
-- /See:/ 'mkStepConfig' smart constructor.
data StepConfig = StepConfig'
  { name :: Types.XmlStringMaxLen256
    -- ^ The name of the step.
  , hadoopJarStep :: Types.HadoopJarStepConfig
    -- ^ The JAR file used for the step.
  , actionOnFailure :: Core.Maybe Types.ActionOnFailure
    -- ^ The action to take when the cluster step fails. Possible values are TERMINATE_CLUSTER, CANCEL_AND_WAIT, and CONTINUE. TERMINATE_JOB_FLOW is provided for backward compatibility. We recommend using TERMINATE_CLUSTER instead.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StepConfig' value with any optional fields omitted.
mkStepConfig
    :: Types.XmlStringMaxLen256 -- ^ 'name'
    -> Types.HadoopJarStepConfig -- ^ 'hadoopJarStep'
    -> StepConfig
mkStepConfig name hadoopJarStep
  = StepConfig'{name, hadoopJarStep, actionOnFailure = Core.Nothing}

-- | The name of the step.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scName :: Lens.Lens' StepConfig Types.XmlStringMaxLen256
scName = Lens.field @"name"
{-# INLINEABLE scName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The JAR file used for the step.
--
-- /Note:/ Consider using 'hadoopJarStep' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scHadoopJarStep :: Lens.Lens' StepConfig Types.HadoopJarStepConfig
scHadoopJarStep = Lens.field @"hadoopJarStep"
{-# INLINEABLE scHadoopJarStep #-}
{-# DEPRECATED hadoopJarStep "Use generic-lens or generic-optics with 'hadoopJarStep' instead"  #-}

-- | The action to take when the cluster step fails. Possible values are TERMINATE_CLUSTER, CANCEL_AND_WAIT, and CONTINUE. TERMINATE_JOB_FLOW is provided for backward compatibility. We recommend using TERMINATE_CLUSTER instead.
--
-- /Note:/ Consider using 'actionOnFailure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scActionOnFailure :: Lens.Lens' StepConfig (Core.Maybe Types.ActionOnFailure)
scActionOnFailure = Lens.field @"actionOnFailure"
{-# INLINEABLE scActionOnFailure #-}
{-# DEPRECATED actionOnFailure "Use generic-lens or generic-optics with 'actionOnFailure' instead"  #-}

instance Core.FromJSON StepConfig where
        toJSON StepConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("HadoopJarStep" Core..= hadoopJarStep),
                  ("ActionOnFailure" Core..=) Core.<$> actionOnFailure])
