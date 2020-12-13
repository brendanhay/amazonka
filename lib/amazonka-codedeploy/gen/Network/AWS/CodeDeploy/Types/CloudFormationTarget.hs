{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.CloudFormationTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.CloudFormationTarget
  ( CloudFormationTarget (..),

    -- * Smart constructor
    mkCloudFormationTarget,

    -- * Lenses
    cftTargetId,
    cftStatus,
    cftDeploymentId,
    cftResourceType,
    cftLastUpdatedAt,
    cftLifecycleEvents,
    cftTargetVersionWeight,
  )
where

import Network.AWS.CodeDeploy.Types.LifecycleEvent
import Network.AWS.CodeDeploy.Types.TargetStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the target to be updated by an AWS CloudFormation blue/green deployment. This target type is used for all deployments initiated by a CloudFormation stack update.
--
-- /See:/ 'mkCloudFormationTarget' smart constructor.
data CloudFormationTarget = CloudFormationTarget'
  { -- | The unique ID of a deployment target that has a type of @CloudFormationTarget@ .
    targetId :: Lude.Maybe Lude.Text,
    -- | The status of an AWS CloudFormation blue/green deployment's target application.
    status :: Lude.Maybe TargetStatus,
    -- | The unique ID of an AWS CloudFormation blue/green deployment.
    deploymentId :: Lude.Maybe Lude.Text,
    -- | The resource type for the AWS CloudFormation blue/green deployment.
    resourceType :: Lude.Maybe Lude.Text,
    -- | The date and time when the target application was updated by an AWS CloudFormation blue/green deployment.
    lastUpdatedAt :: Lude.Maybe Lude.Timestamp,
    -- | The lifecycle events of the AWS CloudFormation blue/green deployment to this target application.
    lifecycleEvents :: Lude.Maybe [LifecycleEvent],
    -- | The percentage of production traffic that the target version of an AWS CloudFormation blue/green deployment receives.
    targetVersionWeight :: Lude.Maybe Lude.Double
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CloudFormationTarget' with the minimum fields required to make a request.
--
-- * 'targetId' - The unique ID of a deployment target that has a type of @CloudFormationTarget@ .
-- * 'status' - The status of an AWS CloudFormation blue/green deployment's target application.
-- * 'deploymentId' - The unique ID of an AWS CloudFormation blue/green deployment.
-- * 'resourceType' - The resource type for the AWS CloudFormation blue/green deployment.
-- * 'lastUpdatedAt' - The date and time when the target application was updated by an AWS CloudFormation blue/green deployment.
-- * 'lifecycleEvents' - The lifecycle events of the AWS CloudFormation blue/green deployment to this target application.
-- * 'targetVersionWeight' - The percentage of production traffic that the target version of an AWS CloudFormation blue/green deployment receives.
mkCloudFormationTarget ::
  CloudFormationTarget
mkCloudFormationTarget =
  CloudFormationTarget'
    { targetId = Lude.Nothing,
      status = Lude.Nothing,
      deploymentId = Lude.Nothing,
      resourceType = Lude.Nothing,
      lastUpdatedAt = Lude.Nothing,
      lifecycleEvents = Lude.Nothing,
      targetVersionWeight = Lude.Nothing
    }

-- | The unique ID of a deployment target that has a type of @CloudFormationTarget@ .
--
-- /Note:/ Consider using 'targetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cftTargetId :: Lens.Lens' CloudFormationTarget (Lude.Maybe Lude.Text)
cftTargetId = Lens.lens (targetId :: CloudFormationTarget -> Lude.Maybe Lude.Text) (\s a -> s {targetId = a} :: CloudFormationTarget)
{-# DEPRECATED cftTargetId "Use generic-lens or generic-optics with 'targetId' instead." #-}

-- | The status of an AWS CloudFormation blue/green deployment's target application.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cftStatus :: Lens.Lens' CloudFormationTarget (Lude.Maybe TargetStatus)
cftStatus = Lens.lens (status :: CloudFormationTarget -> Lude.Maybe TargetStatus) (\s a -> s {status = a} :: CloudFormationTarget)
{-# DEPRECATED cftStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The unique ID of an AWS CloudFormation blue/green deployment.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cftDeploymentId :: Lens.Lens' CloudFormationTarget (Lude.Maybe Lude.Text)
cftDeploymentId = Lens.lens (deploymentId :: CloudFormationTarget -> Lude.Maybe Lude.Text) (\s a -> s {deploymentId = a} :: CloudFormationTarget)
{-# DEPRECATED cftDeploymentId "Use generic-lens or generic-optics with 'deploymentId' instead." #-}

-- | The resource type for the AWS CloudFormation blue/green deployment.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cftResourceType :: Lens.Lens' CloudFormationTarget (Lude.Maybe Lude.Text)
cftResourceType = Lens.lens (resourceType :: CloudFormationTarget -> Lude.Maybe Lude.Text) (\s a -> s {resourceType = a} :: CloudFormationTarget)
{-# DEPRECATED cftResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The date and time when the target application was updated by an AWS CloudFormation blue/green deployment.
--
-- /Note:/ Consider using 'lastUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cftLastUpdatedAt :: Lens.Lens' CloudFormationTarget (Lude.Maybe Lude.Timestamp)
cftLastUpdatedAt = Lens.lens (lastUpdatedAt :: CloudFormationTarget -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedAt = a} :: CloudFormationTarget)
{-# DEPRECATED cftLastUpdatedAt "Use generic-lens or generic-optics with 'lastUpdatedAt' instead." #-}

-- | The lifecycle events of the AWS CloudFormation blue/green deployment to this target application.
--
-- /Note:/ Consider using 'lifecycleEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cftLifecycleEvents :: Lens.Lens' CloudFormationTarget (Lude.Maybe [LifecycleEvent])
cftLifecycleEvents = Lens.lens (lifecycleEvents :: CloudFormationTarget -> Lude.Maybe [LifecycleEvent]) (\s a -> s {lifecycleEvents = a} :: CloudFormationTarget)
{-# DEPRECATED cftLifecycleEvents "Use generic-lens or generic-optics with 'lifecycleEvents' instead." #-}

-- | The percentage of production traffic that the target version of an AWS CloudFormation blue/green deployment receives.
--
-- /Note:/ Consider using 'targetVersionWeight' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cftTargetVersionWeight :: Lens.Lens' CloudFormationTarget (Lude.Maybe Lude.Double)
cftTargetVersionWeight = Lens.lens (targetVersionWeight :: CloudFormationTarget -> Lude.Maybe Lude.Double) (\s a -> s {targetVersionWeight = a} :: CloudFormationTarget)
{-# DEPRECATED cftTargetVersionWeight "Use generic-lens or generic-optics with 'targetVersionWeight' instead." #-}

instance Lude.FromJSON CloudFormationTarget where
  parseJSON =
    Lude.withObject
      "CloudFormationTarget"
      ( \x ->
          CloudFormationTarget'
            Lude.<$> (x Lude..:? "targetId")
            Lude.<*> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "deploymentId")
            Lude.<*> (x Lude..:? "resourceType")
            Lude.<*> (x Lude..:? "lastUpdatedAt")
            Lude.<*> (x Lude..:? "lifecycleEvents" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "targetVersionWeight")
      )
