{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.InstanceTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.InstanceTarget
  ( InstanceTarget (..),

    -- * Smart constructor
    mkInstanceTarget,

    -- * Lenses
    itTargetARN,
    itTargetId,
    itStatus,
    itDeploymentId,
    itInstanceLabel,
    itLastUpdatedAt,
    itLifecycleEvents,
  )
where

import Network.AWS.CodeDeploy.Types.LifecycleEvent
import Network.AWS.CodeDeploy.Types.TargetLabel
import Network.AWS.CodeDeploy.Types.TargetStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A target Amazon EC2 or on-premises instance during a deployment that uses the EC2/On-premises compute platform.
--
-- /See:/ 'mkInstanceTarget' smart constructor.
data InstanceTarget = InstanceTarget'
  { -- | The Amazon Resource Name (ARN) of the target.
    targetARN :: Lude.Maybe Lude.Text,
    -- | The unique ID of a deployment target that has a type of @instanceTarget@ .
    targetId :: Lude.Maybe Lude.Text,
    -- | The status an EC2/On-premises deployment's target instance.
    status :: Lude.Maybe TargetStatus,
    -- | The unique ID of a deployment.
    deploymentId :: Lude.Maybe Lude.Text,
    -- | A label that identifies whether the instance is an original target (@BLUE@ ) or a replacement target (@GREEN@ ).
    instanceLabel :: Lude.Maybe TargetLabel,
    -- | The date and time when the target instance was updated by a deployment.
    lastUpdatedAt :: Lude.Maybe Lude.Timestamp,
    -- | The lifecycle events of the deployment to this target instance.
    lifecycleEvents :: Lude.Maybe [LifecycleEvent]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceTarget' with the minimum fields required to make a request.
--
-- * 'targetARN' - The Amazon Resource Name (ARN) of the target.
-- * 'targetId' - The unique ID of a deployment target that has a type of @instanceTarget@ .
-- * 'status' - The status an EC2/On-premises deployment's target instance.
-- * 'deploymentId' - The unique ID of a deployment.
-- * 'instanceLabel' - A label that identifies whether the instance is an original target (@BLUE@ ) or a replacement target (@GREEN@ ).
-- * 'lastUpdatedAt' - The date and time when the target instance was updated by a deployment.
-- * 'lifecycleEvents' - The lifecycle events of the deployment to this target instance.
mkInstanceTarget ::
  InstanceTarget
mkInstanceTarget =
  InstanceTarget'
    { targetARN = Lude.Nothing,
      targetId = Lude.Nothing,
      status = Lude.Nothing,
      deploymentId = Lude.Nothing,
      instanceLabel = Lude.Nothing,
      lastUpdatedAt = Lude.Nothing,
      lifecycleEvents = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the target.
--
-- /Note:/ Consider using 'targetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itTargetARN :: Lens.Lens' InstanceTarget (Lude.Maybe Lude.Text)
itTargetARN = Lens.lens (targetARN :: InstanceTarget -> Lude.Maybe Lude.Text) (\s a -> s {targetARN = a} :: InstanceTarget)
{-# DEPRECATED itTargetARN "Use generic-lens or generic-optics with 'targetARN' instead." #-}

-- | The unique ID of a deployment target that has a type of @instanceTarget@ .
--
-- /Note:/ Consider using 'targetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itTargetId :: Lens.Lens' InstanceTarget (Lude.Maybe Lude.Text)
itTargetId = Lens.lens (targetId :: InstanceTarget -> Lude.Maybe Lude.Text) (\s a -> s {targetId = a} :: InstanceTarget)
{-# DEPRECATED itTargetId "Use generic-lens or generic-optics with 'targetId' instead." #-}

-- | The status an EC2/On-premises deployment's target instance.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itStatus :: Lens.Lens' InstanceTarget (Lude.Maybe TargetStatus)
itStatus = Lens.lens (status :: InstanceTarget -> Lude.Maybe TargetStatus) (\s a -> s {status = a} :: InstanceTarget)
{-# DEPRECATED itStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The unique ID of a deployment.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itDeploymentId :: Lens.Lens' InstanceTarget (Lude.Maybe Lude.Text)
itDeploymentId = Lens.lens (deploymentId :: InstanceTarget -> Lude.Maybe Lude.Text) (\s a -> s {deploymentId = a} :: InstanceTarget)
{-# DEPRECATED itDeploymentId "Use generic-lens or generic-optics with 'deploymentId' instead." #-}

-- | A label that identifies whether the instance is an original target (@BLUE@ ) or a replacement target (@GREEN@ ).
--
-- /Note:/ Consider using 'instanceLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itInstanceLabel :: Lens.Lens' InstanceTarget (Lude.Maybe TargetLabel)
itInstanceLabel = Lens.lens (instanceLabel :: InstanceTarget -> Lude.Maybe TargetLabel) (\s a -> s {instanceLabel = a} :: InstanceTarget)
{-# DEPRECATED itInstanceLabel "Use generic-lens or generic-optics with 'instanceLabel' instead." #-}

-- | The date and time when the target instance was updated by a deployment.
--
-- /Note:/ Consider using 'lastUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itLastUpdatedAt :: Lens.Lens' InstanceTarget (Lude.Maybe Lude.Timestamp)
itLastUpdatedAt = Lens.lens (lastUpdatedAt :: InstanceTarget -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedAt = a} :: InstanceTarget)
{-# DEPRECATED itLastUpdatedAt "Use generic-lens or generic-optics with 'lastUpdatedAt' instead." #-}

-- | The lifecycle events of the deployment to this target instance.
--
-- /Note:/ Consider using 'lifecycleEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itLifecycleEvents :: Lens.Lens' InstanceTarget (Lude.Maybe [LifecycleEvent])
itLifecycleEvents = Lens.lens (lifecycleEvents :: InstanceTarget -> Lude.Maybe [LifecycleEvent]) (\s a -> s {lifecycleEvents = a} :: InstanceTarget)
{-# DEPRECATED itLifecycleEvents "Use generic-lens or generic-optics with 'lifecycleEvents' instead." #-}

instance Lude.FromJSON InstanceTarget where
  parseJSON =
    Lude.withObject
      "InstanceTarget"
      ( \x ->
          InstanceTarget'
            Lude.<$> (x Lude..:? "targetArn")
            Lude.<*> (x Lude..:? "targetId")
            Lude.<*> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "deploymentId")
            Lude.<*> (x Lude..:? "instanceLabel")
            Lude.<*> (x Lude..:? "lastUpdatedAt")
            Lude.<*> (x Lude..:? "lifecycleEvents" Lude..!= Lude.mempty)
      )
