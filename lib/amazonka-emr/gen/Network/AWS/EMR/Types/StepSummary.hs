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
    sStatus,
    sActionOnFailure,
    sConfig,
    sName,
    sId,
  )
where

import Network.AWS.EMR.Types.ActionOnFailure
import Network.AWS.EMR.Types.HadoopStepConfig
import Network.AWS.EMR.Types.StepStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The summary of the cluster step.
--
-- /See:/ 'mkStepSummary' smart constructor.
data StepSummary = StepSummary'
  { -- | The current execution status details of the cluster step.
    status :: Lude.Maybe StepStatus,
    -- | The action to take when the cluster step fails. Possible values are TERMINATE_CLUSTER, CANCEL_AND_WAIT, and CONTINUE. TERMINATE_JOB_FLOW is available for backward compatibility. We recommend using TERMINATE_CLUSTER instead.
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

-- | Creates a value of 'StepSummary' with the minimum fields required to make a request.
--
-- * 'status' - The current execution status details of the cluster step.
-- * 'actionOnFailure' - The action to take when the cluster step fails. Possible values are TERMINATE_CLUSTER, CANCEL_AND_WAIT, and CONTINUE. TERMINATE_JOB_FLOW is available for backward compatibility. We recommend using TERMINATE_CLUSTER instead.
-- * 'config' - The Hadoop job configuration of the cluster step.
-- * 'name' - The name of the cluster step.
-- * 'id' - The identifier of the cluster step.
mkStepSummary ::
  StepSummary
mkStepSummary =
  StepSummary'
    { status = Lude.Nothing,
      actionOnFailure = Lude.Nothing,
      config = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing
    }

-- | The current execution status details of the cluster step.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStatus :: Lens.Lens' StepSummary (Lude.Maybe StepStatus)
sStatus = Lens.lens (status :: StepSummary -> Lude.Maybe StepStatus) (\s a -> s {status = a} :: StepSummary)
{-# DEPRECATED sStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The action to take when the cluster step fails. Possible values are TERMINATE_CLUSTER, CANCEL_AND_WAIT, and CONTINUE. TERMINATE_JOB_FLOW is available for backward compatibility. We recommend using TERMINATE_CLUSTER instead.
--
-- /Note:/ Consider using 'actionOnFailure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sActionOnFailure :: Lens.Lens' StepSummary (Lude.Maybe ActionOnFailure)
sActionOnFailure = Lens.lens (actionOnFailure :: StepSummary -> Lude.Maybe ActionOnFailure) (\s a -> s {actionOnFailure = a} :: StepSummary)
{-# DEPRECATED sActionOnFailure "Use generic-lens or generic-optics with 'actionOnFailure' instead." #-}

-- | The Hadoop job configuration of the cluster step.
--
-- /Note:/ Consider using 'config' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sConfig :: Lens.Lens' StepSummary (Lude.Maybe HadoopStepConfig)
sConfig = Lens.lens (config :: StepSummary -> Lude.Maybe HadoopStepConfig) (\s a -> s {config = a} :: StepSummary)
{-# DEPRECATED sConfig "Use generic-lens or generic-optics with 'config' instead." #-}

-- | The name of the cluster step.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sName :: Lens.Lens' StepSummary (Lude.Maybe Lude.Text)
sName = Lens.lens (name :: StepSummary -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: StepSummary)
{-# DEPRECATED sName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The identifier of the cluster step.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sId :: Lens.Lens' StepSummary (Lude.Maybe Lude.Text)
sId = Lens.lens (id :: StepSummary -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: StepSummary)
{-# DEPRECATED sId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON StepSummary where
  parseJSON =
    Lude.withObject
      "StepSummary"
      ( \x ->
          StepSummary'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "ActionOnFailure")
            Lude.<*> (x Lude..:? "Config")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Id")
      )
