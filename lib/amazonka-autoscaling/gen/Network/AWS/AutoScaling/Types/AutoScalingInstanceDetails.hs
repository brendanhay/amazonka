-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.AutoScalingInstanceDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.AutoScalingInstanceDetails
  ( AutoScalingInstanceDetails (..),

    -- * Smart constructor
    mkAutoScalingInstanceDetails,

    -- * Lenses
    asidWeightedCapacity,
    asidInstanceType,
    asidLaunchConfigurationName,
    asidLaunchTemplate,
    asidInstanceId,
    asidAutoScalingGroupName,
    asidAvailabilityZone,
    asidLifecycleState,
    asidHealthStatus,
    asidProtectedFromScaleIn,
  )
where

import Network.AWS.AutoScaling.Types.LaunchTemplateSpecification
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an EC2 instance associated with an Auto Scaling group.
--
-- /See:/ 'mkAutoScalingInstanceDetails' smart constructor.
data AutoScalingInstanceDetails = AutoScalingInstanceDetails'
  { weightedCapacity ::
      Lude.Maybe Lude.Text,
    instanceType :: Lude.Maybe Lude.Text,
    launchConfigurationName ::
      Lude.Maybe Lude.Text,
    launchTemplate ::
      Lude.Maybe
        LaunchTemplateSpecification,
    instanceId :: Lude.Text,
    autoScalingGroupName :: Lude.Text,
    availabilityZone :: Lude.Text,
    lifecycleState :: Lude.Text,
    healthStatus :: Lude.Text,
    protectedFromScaleIn :: Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AutoScalingInstanceDetails' with the minimum fields required to make a request.
--
-- * 'autoScalingGroupName' - The name of the Auto Scaling group for the instance.
-- * 'availabilityZone' - The Availability Zone for the instance.
-- * 'healthStatus' - The last reported health status of this instance. "Healthy" means that the instance is healthy and should remain in service. "Unhealthy" means that the instance is unhealthy and Amazon EC2 Auto Scaling should terminate and replace it.
-- * 'instanceId' - The ID of the instance.
-- * 'instanceType' - The instance type of the EC2 instance.
-- * 'launchConfigurationName' - The launch configuration used to launch the instance. This value is not available if you attached the instance to the Auto Scaling group.
-- * 'launchTemplate' - The launch template for the instance.
-- * 'lifecycleState' - The lifecycle state for the instance.
-- * 'protectedFromScaleIn' - Indicates whether the instance is protected from termination by Amazon EC2 Auto Scaling when scaling in.
-- * 'weightedCapacity' - The number of capacity units contributed by the instance based on its instance type.
--
-- Valid Range: Minimum value of 1. Maximum value of 999.
mkAutoScalingInstanceDetails ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'autoScalingGroupName'
  Lude.Text ->
  -- | 'availabilityZone'
  Lude.Text ->
  -- | 'lifecycleState'
  Lude.Text ->
  -- | 'healthStatus'
  Lude.Text ->
  -- | 'protectedFromScaleIn'
  Lude.Bool ->
  AutoScalingInstanceDetails
mkAutoScalingInstanceDetails
  pInstanceId_
  pAutoScalingGroupName_
  pAvailabilityZone_
  pLifecycleState_
  pHealthStatus_
  pProtectedFromScaleIn_ =
    AutoScalingInstanceDetails'
      { weightedCapacity = Lude.Nothing,
        instanceType = Lude.Nothing,
        launchConfigurationName = Lude.Nothing,
        launchTemplate = Lude.Nothing,
        instanceId = pInstanceId_,
        autoScalingGroupName = pAutoScalingGroupName_,
        availabilityZone = pAvailabilityZone_,
        lifecycleState = pLifecycleState_,
        healthStatus = pHealthStatus_,
        protectedFromScaleIn = pProtectedFromScaleIn_
      }

-- | The number of capacity units contributed by the instance based on its instance type.
--
-- Valid Range: Minimum value of 1. Maximum value of 999.
--
-- /Note:/ Consider using 'weightedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asidWeightedCapacity :: Lens.Lens' AutoScalingInstanceDetails (Lude.Maybe Lude.Text)
asidWeightedCapacity = Lens.lens (weightedCapacity :: AutoScalingInstanceDetails -> Lude.Maybe Lude.Text) (\s a -> s {weightedCapacity = a} :: AutoScalingInstanceDetails)
{-# DEPRECATED asidWeightedCapacity "Use generic-lens or generic-optics with 'weightedCapacity' instead." #-}

-- | The instance type of the EC2 instance.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asidInstanceType :: Lens.Lens' AutoScalingInstanceDetails (Lude.Maybe Lude.Text)
asidInstanceType = Lens.lens (instanceType :: AutoScalingInstanceDetails -> Lude.Maybe Lude.Text) (\s a -> s {instanceType = a} :: AutoScalingInstanceDetails)
{-# DEPRECATED asidInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The launch configuration used to launch the instance. This value is not available if you attached the instance to the Auto Scaling group.
--
-- /Note:/ Consider using 'launchConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asidLaunchConfigurationName :: Lens.Lens' AutoScalingInstanceDetails (Lude.Maybe Lude.Text)
asidLaunchConfigurationName = Lens.lens (launchConfigurationName :: AutoScalingInstanceDetails -> Lude.Maybe Lude.Text) (\s a -> s {launchConfigurationName = a} :: AutoScalingInstanceDetails)
{-# DEPRECATED asidLaunchConfigurationName "Use generic-lens or generic-optics with 'launchConfigurationName' instead." #-}

-- | The launch template for the instance.
--
-- /Note:/ Consider using 'launchTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asidLaunchTemplate :: Lens.Lens' AutoScalingInstanceDetails (Lude.Maybe LaunchTemplateSpecification)
asidLaunchTemplate = Lens.lens (launchTemplate :: AutoScalingInstanceDetails -> Lude.Maybe LaunchTemplateSpecification) (\s a -> s {launchTemplate = a} :: AutoScalingInstanceDetails)
{-# DEPRECATED asidLaunchTemplate "Use generic-lens or generic-optics with 'launchTemplate' instead." #-}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asidInstanceId :: Lens.Lens' AutoScalingInstanceDetails Lude.Text
asidInstanceId = Lens.lens (instanceId :: AutoScalingInstanceDetails -> Lude.Text) (\s a -> s {instanceId = a} :: AutoScalingInstanceDetails)
{-# DEPRECATED asidInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The name of the Auto Scaling group for the instance.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asidAutoScalingGroupName :: Lens.Lens' AutoScalingInstanceDetails Lude.Text
asidAutoScalingGroupName = Lens.lens (autoScalingGroupName :: AutoScalingInstanceDetails -> Lude.Text) (\s a -> s {autoScalingGroupName = a} :: AutoScalingInstanceDetails)
{-# DEPRECATED asidAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | The Availability Zone for the instance.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asidAvailabilityZone :: Lens.Lens' AutoScalingInstanceDetails Lude.Text
asidAvailabilityZone = Lens.lens (availabilityZone :: AutoScalingInstanceDetails -> Lude.Text) (\s a -> s {availabilityZone = a} :: AutoScalingInstanceDetails)
{-# DEPRECATED asidAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The lifecycle state for the instance.
--
-- /Note:/ Consider using 'lifecycleState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asidLifecycleState :: Lens.Lens' AutoScalingInstanceDetails Lude.Text
asidLifecycleState = Lens.lens (lifecycleState :: AutoScalingInstanceDetails -> Lude.Text) (\s a -> s {lifecycleState = a} :: AutoScalingInstanceDetails)
{-# DEPRECATED asidLifecycleState "Use generic-lens or generic-optics with 'lifecycleState' instead." #-}

-- | The last reported health status of this instance. "Healthy" means that the instance is healthy and should remain in service. "Unhealthy" means that the instance is unhealthy and Amazon EC2 Auto Scaling should terminate and replace it.
--
-- /Note:/ Consider using 'healthStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asidHealthStatus :: Lens.Lens' AutoScalingInstanceDetails Lude.Text
asidHealthStatus = Lens.lens (healthStatus :: AutoScalingInstanceDetails -> Lude.Text) (\s a -> s {healthStatus = a} :: AutoScalingInstanceDetails)
{-# DEPRECATED asidHealthStatus "Use generic-lens or generic-optics with 'healthStatus' instead." #-}

-- | Indicates whether the instance is protected from termination by Amazon EC2 Auto Scaling when scaling in.
--
-- /Note:/ Consider using 'protectedFromScaleIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asidProtectedFromScaleIn :: Lens.Lens' AutoScalingInstanceDetails Lude.Bool
asidProtectedFromScaleIn = Lens.lens (protectedFromScaleIn :: AutoScalingInstanceDetails -> Lude.Bool) (\s a -> s {protectedFromScaleIn = a} :: AutoScalingInstanceDetails)
{-# DEPRECATED asidProtectedFromScaleIn "Use generic-lens or generic-optics with 'protectedFromScaleIn' instead." #-}

instance Lude.FromXML AutoScalingInstanceDetails where
  parseXML x =
    AutoScalingInstanceDetails'
      Lude.<$> (x Lude..@? "WeightedCapacity")
      Lude.<*> (x Lude..@? "InstanceType")
      Lude.<*> (x Lude..@? "LaunchConfigurationName")
      Lude.<*> (x Lude..@? "LaunchTemplate")
      Lude.<*> (x Lude..@ "InstanceId")
      Lude.<*> (x Lude..@ "AutoScalingGroupName")
      Lude.<*> (x Lude..@ "AvailabilityZone")
      Lude.<*> (x Lude..@ "LifecycleState")
      Lude.<*> (x Lude..@ "HealthStatus")
      Lude.<*> (x Lude..@ "ProtectedFromScaleIn")
