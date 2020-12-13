{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.Step
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.Step
  ( Step (..),

    -- * Smart constructor
    mkStep,

    -- * Lenses
    sgStatus,
    sgActionOnFailure,
    sgConfig,
    sgName,
    sgId,
  )
where

import Network.AWS.EMR.Types.ActionOnFailure
import Network.AWS.EMR.Types.HadoopStepConfig
import Network.AWS.EMR.Types.StepStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | This represents a step in a cluster.
--
-- /See:/ 'mkStep' smart constructor.
data Step = Step'
  { -- | The current execution status details of the cluster step.
    status :: Lude.Maybe StepStatus,
    -- | The action to take when the cluster step fails. Possible values are TERMINATE_CLUSTER, CANCEL_AND_WAIT, and CONTINUE. TERMINATE_JOB_FLOW is provided for backward compatibility. We recommend using TERMINATE_CLUSTER instead.
    actionOnFailure :: Lude.Maybe ActionOnFailure,
    -- | The Hadoop job configuration of the cluster step.
    config :: Lude.Maybe HadoopStepConfig,
    -- | The name of the cluster step.
    name :: Lude.Maybe Lude.Text,
    -- | The identifier of the cluster step.
    id :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Step' with the minimum fields required to make a request.
--
-- * 'status' - The current execution status details of the cluster step.
-- * 'actionOnFailure' - The action to take when the cluster step fails. Possible values are TERMINATE_CLUSTER, CANCEL_AND_WAIT, and CONTINUE. TERMINATE_JOB_FLOW is provided for backward compatibility. We recommend using TERMINATE_CLUSTER instead.
-- * 'config' - The Hadoop job configuration of the cluster step.
-- * 'name' - The name of the cluster step.
-- * 'id' - The identifier of the cluster step.
mkStep ::
  Step
mkStep =
  Step'
    { status = Lude.Nothing,
      actionOnFailure = Lude.Nothing,
      config = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing
    }

-- | The current execution status details of the cluster step.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgStatus :: Lens.Lens' Step (Lude.Maybe StepStatus)
sgStatus = Lens.lens (status :: Step -> Lude.Maybe StepStatus) (\s a -> s {status = a} :: Step)
{-# DEPRECATED sgStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The action to take when the cluster step fails. Possible values are TERMINATE_CLUSTER, CANCEL_AND_WAIT, and CONTINUE. TERMINATE_JOB_FLOW is provided for backward compatibility. We recommend using TERMINATE_CLUSTER instead.
--
-- /Note:/ Consider using 'actionOnFailure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgActionOnFailure :: Lens.Lens' Step (Lude.Maybe ActionOnFailure)
sgActionOnFailure = Lens.lens (actionOnFailure :: Step -> Lude.Maybe ActionOnFailure) (\s a -> s {actionOnFailure = a} :: Step)
{-# DEPRECATED sgActionOnFailure "Use generic-lens or generic-optics with 'actionOnFailure' instead." #-}

-- | The Hadoop job configuration of the cluster step.
--
-- /Note:/ Consider using 'config' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgConfig :: Lens.Lens' Step (Lude.Maybe HadoopStepConfig)
sgConfig = Lens.lens (config :: Step -> Lude.Maybe HadoopStepConfig) (\s a -> s {config = a} :: Step)
{-# DEPRECATED sgConfig "Use generic-lens or generic-optics with 'config' instead." #-}

-- | The name of the cluster step.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgName :: Lens.Lens' Step (Lude.Maybe Lude.Text)
sgName = Lens.lens (name :: Step -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Step)
{-# DEPRECATED sgName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The identifier of the cluster step.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgId :: Lens.Lens' Step (Lude.Maybe Lude.Text)
sgId = Lens.lens (id :: Step -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Step)
{-# DEPRECATED sgId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON Step where
  parseJSON =
    Lude.withObject
      "Step"
      ( \x ->
          Step'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "ActionOnFailure")
            Lude.<*> (x Lude..:? "Config")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Id")
      )
