{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.StepConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.StepConfig
  ( StepConfig (..),

    -- * Smart constructor
    mkStepConfig,

    -- * Lenses
    scActionOnFailure,
    scHadoopJARStep,
    scName,
  )
where

import Network.AWS.EMR.Types.ActionOnFailure
import Network.AWS.EMR.Types.HadoopJARStepConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specification of a cluster (job flow) step.
--
-- /See:/ 'mkStepConfig' smart constructor.
data StepConfig = StepConfig'
  { -- | The action to take when the cluster step fails. Possible values are TERMINATE_CLUSTER, CANCEL_AND_WAIT, and CONTINUE. TERMINATE_JOB_FLOW is provided for backward compatibility. We recommend using TERMINATE_CLUSTER instead.
    actionOnFailure :: Lude.Maybe ActionOnFailure,
    -- | The JAR file used for the step.
    hadoopJARStep :: HadoopJARStepConfig,
    -- | The name of the step.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StepConfig' with the minimum fields required to make a request.
--
-- * 'actionOnFailure' - The action to take when the cluster step fails. Possible values are TERMINATE_CLUSTER, CANCEL_AND_WAIT, and CONTINUE. TERMINATE_JOB_FLOW is provided for backward compatibility. We recommend using TERMINATE_CLUSTER instead.
-- * 'hadoopJARStep' - The JAR file used for the step.
-- * 'name' - The name of the step.
mkStepConfig ::
  -- | 'hadoopJARStep'
  HadoopJARStepConfig ->
  -- | 'name'
  Lude.Text ->
  StepConfig
mkStepConfig pHadoopJARStep_ pName_ =
  StepConfig'
    { actionOnFailure = Lude.Nothing,
      hadoopJARStep = pHadoopJARStep_,
      name = pName_
    }

-- | The action to take when the cluster step fails. Possible values are TERMINATE_CLUSTER, CANCEL_AND_WAIT, and CONTINUE. TERMINATE_JOB_FLOW is provided for backward compatibility. We recommend using TERMINATE_CLUSTER instead.
--
-- /Note:/ Consider using 'actionOnFailure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scActionOnFailure :: Lens.Lens' StepConfig (Lude.Maybe ActionOnFailure)
scActionOnFailure = Lens.lens (actionOnFailure :: StepConfig -> Lude.Maybe ActionOnFailure) (\s a -> s {actionOnFailure = a} :: StepConfig)
{-# DEPRECATED scActionOnFailure "Use generic-lens or generic-optics with 'actionOnFailure' instead." #-}

-- | The JAR file used for the step.
--
-- /Note:/ Consider using 'hadoopJARStep' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scHadoopJARStep :: Lens.Lens' StepConfig HadoopJARStepConfig
scHadoopJARStep = Lens.lens (hadoopJARStep :: StepConfig -> HadoopJARStepConfig) (\s a -> s {hadoopJARStep = a} :: StepConfig)
{-# DEPRECATED scHadoopJARStep "Use generic-lens or generic-optics with 'hadoopJARStep' instead." #-}

-- | The name of the step.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scName :: Lens.Lens' StepConfig Lude.Text
scName = Lens.lens (name :: StepConfig -> Lude.Text) (\s a -> s {name = a} :: StepConfig)
{-# DEPRECATED scName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.ToJSON StepConfig where
  toJSON StepConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ActionOnFailure" Lude..=) Lude.<$> actionOnFailure,
            Lude.Just ("HadoopJarStep" Lude..= hadoopJARStep),
            Lude.Just ("Name" Lude..= name)
          ]
      )
