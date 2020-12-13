{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    asidInstanceId,
    asidWeightedCapacity,
    asidProtectedFromScaleIn,
    asidInstanceType,
    asidAvailabilityZone,
    asidAutoScalingGroupName,
    asidLaunchConfigurationName,
    asidLaunchTemplate,
    asidHealthStatus,
    asidLifecycleState,
  )
where

import Network.AWS.AutoScaling.Types.LaunchTemplateSpecification
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an EC2 instance associated with an Auto Scaling group.
--
-- /See:/ 'mkAutoScalingInstanceDetails' smart constructor.
data AutoScalingInstanceDetails = AutoScalingInstanceDetails'
  { -- | The ID of the instance.
    instanceId :: Lude.Text,
    -- | The number of capacity units contributed by the instance based on its instance type.
    --
    -- Valid Range: Minimum value of 1. Maximum value of 999.
    weightedCapacity :: Lude.Maybe Lude.Text,
    -- | Indicates whether the instance is protected from termination by Amazon EC2 Auto Scaling when scaling in.
    protectedFromScaleIn :: Lude.Bool,
    -- | The instance type of the EC2 instance.
    instanceType :: Lude.Maybe Lude.Text,
    -- | The Availability Zone for the instance.
    availabilityZone :: Lude.Text,
    -- | The name of the Auto Scaling group for the instance.
    autoScalingGroupName :: Lude.Text,
    -- | The launch configuration used to launch the instance. This value is not available if you attached the instance to the Auto Scaling group.
    launchConfigurationName :: Lude.Maybe Lude.Text,
    -- | The launch template for the instance.
    launchTemplate :: Lude.Maybe LaunchTemplateSpecification,
    -- | The last reported health status of this instance. "Healthy" means that the instance is healthy and should remain in service. "Unhealthy" means that the instance is unhealthy and Amazon EC2 Auto Scaling should terminate and replace it.
    healthStatus :: Lude.Text,
    -- | The lifecycle state for the instance.
    lifecycleState :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AutoScalingInstanceDetails' with the minimum fields required to make a request.
--
-- * 'instanceId' - The ID of the instance.
-- * 'weightedCapacity' - The number of capacity units contributed by the instance based on its instance type.
--
-- Valid Range: Minimum value of 1. Maximum value of 999.
-- * 'protectedFromScaleIn' - Indicates whether the instance is protected from termination by Amazon EC2 Auto Scaling when scaling in.
-- * 'instanceType' - The instance type of the EC2 instance.
-- * 'availabilityZone' - The Availability Zone for the instance.
-- * 'autoScalingGroupName' - The name of the Auto Scaling group for the instance.
-- * 'launchConfigurationName' - The launch configuration used to launch the instance. This value is not available if you attached the instance to the Auto Scaling group.
-- * 'launchTemplate' - The launch template for the instance.
-- * 'healthStatus' - The last reported health status of this instance. "Healthy" means that the instance is healthy and should remain in service. "Unhealthy" means that the instance is unhealthy and Amazon EC2 Auto Scaling should terminate and replace it.
-- * 'lifecycleState' - The lifecycle state for the instance.
mkAutoScalingInstanceDetails ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'protectedFromScaleIn'
  Lude.Bool ->
  -- | 'availabilityZone'
  Lude.Text ->
  -- | 'autoScalingGroupName'
  Lude.Text ->
  -- | 'healthStatus'
  Lude.Text ->
  -- | 'lifecycleState'
  Lude.Text ->
  AutoScalingInstanceDetails
mkAutoScalingInstanceDetails
  pInstanceId_
  pProtectedFromScaleIn_
  pAvailabilityZone_
  pAutoScalingGroupName_
  pHealthStatus_
  pLifecycleState_ =
    AutoScalingInstanceDetails'
      { instanceId = pInstanceId_,
        weightedCapacity = Lude.Nothing,
        protectedFromScaleIn = pProtectedFromScaleIn_,
        instanceType = Lude.Nothing,
        availabilityZone = pAvailabilityZone_,
        autoScalingGroupName = pAutoScalingGroupName_,
        launchConfigurationName = Lude.Nothing,
        launchTemplate = Lude.Nothing,
        healthStatus = pHealthStatus_,
        lifecycleState = pLifecycleState_
      }

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asidInstanceId :: Lens.Lens' AutoScalingInstanceDetails Lude.Text
asidInstanceId = Lens.lens (instanceId :: AutoScalingInstanceDetails -> Lude.Text) (\s a -> s {instanceId = a} :: AutoScalingInstanceDetails)
{-# DEPRECATED asidInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The number of capacity units contributed by the instance based on its instance type.
--
-- Valid Range: Minimum value of 1. Maximum value of 999.
--
-- /Note:/ Consider using 'weightedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asidWeightedCapacity :: Lens.Lens' AutoScalingInstanceDetails (Lude.Maybe Lude.Text)
asidWeightedCapacity = Lens.lens (weightedCapacity :: AutoScalingInstanceDetails -> Lude.Maybe Lude.Text) (\s a -> s {weightedCapacity = a} :: AutoScalingInstanceDetails)
{-# DEPRECATED asidWeightedCapacity "Use generic-lens or generic-optics with 'weightedCapacity' instead." #-}

-- | Indicates whether the instance is protected from termination by Amazon EC2 Auto Scaling when scaling in.
--
-- /Note:/ Consider using 'protectedFromScaleIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asidProtectedFromScaleIn :: Lens.Lens' AutoScalingInstanceDetails Lude.Bool
asidProtectedFromScaleIn = Lens.lens (protectedFromScaleIn :: AutoScalingInstanceDetails -> Lude.Bool) (\s a -> s {protectedFromScaleIn = a} :: AutoScalingInstanceDetails)
{-# DEPRECATED asidProtectedFromScaleIn "Use generic-lens or generic-optics with 'protectedFromScaleIn' instead." #-}

-- | The instance type of the EC2 instance.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asidInstanceType :: Lens.Lens' AutoScalingInstanceDetails (Lude.Maybe Lude.Text)
asidInstanceType = Lens.lens (instanceType :: AutoScalingInstanceDetails -> Lude.Maybe Lude.Text) (\s a -> s {instanceType = a} :: AutoScalingInstanceDetails)
{-# DEPRECATED asidInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The Availability Zone for the instance.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asidAvailabilityZone :: Lens.Lens' AutoScalingInstanceDetails Lude.Text
asidAvailabilityZone = Lens.lens (availabilityZone :: AutoScalingInstanceDetails -> Lude.Text) (\s a -> s {availabilityZone = a} :: AutoScalingInstanceDetails)
{-# DEPRECATED asidAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The name of the Auto Scaling group for the instance.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asidAutoScalingGroupName :: Lens.Lens' AutoScalingInstanceDetails Lude.Text
asidAutoScalingGroupName = Lens.lens (autoScalingGroupName :: AutoScalingInstanceDetails -> Lude.Text) (\s a -> s {autoScalingGroupName = a} :: AutoScalingInstanceDetails)
{-# DEPRECATED asidAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

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

-- | The last reported health status of this instance. "Healthy" means that the instance is healthy and should remain in service. "Unhealthy" means that the instance is unhealthy and Amazon EC2 Auto Scaling should terminate and replace it.
--
-- /Note:/ Consider using 'healthStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asidHealthStatus :: Lens.Lens' AutoScalingInstanceDetails Lude.Text
asidHealthStatus = Lens.lens (healthStatus :: AutoScalingInstanceDetails -> Lude.Text) (\s a -> s {healthStatus = a} :: AutoScalingInstanceDetails)
{-# DEPRECATED asidHealthStatus "Use generic-lens or generic-optics with 'healthStatus' instead." #-}

-- | The lifecycle state for the instance.
--
-- /Note:/ Consider using 'lifecycleState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asidLifecycleState :: Lens.Lens' AutoScalingInstanceDetails Lude.Text
asidLifecycleState = Lens.lens (lifecycleState :: AutoScalingInstanceDetails -> Lude.Text) (\s a -> s {lifecycleState = a} :: AutoScalingInstanceDetails)
{-# DEPRECATED asidLifecycleState "Use generic-lens or generic-optics with 'lifecycleState' instead." #-}

instance Lude.FromXML AutoScalingInstanceDetails where
  parseXML x =
    AutoScalingInstanceDetails'
      Lude.<$> (x Lude..@ "InstanceId")
      Lude.<*> (x Lude..@? "WeightedCapacity")
      Lude.<*> (x Lude..@ "ProtectedFromScaleIn")
      Lude.<*> (x Lude..@? "InstanceType")
      Lude.<*> (x Lude..@ "AvailabilityZone")
      Lude.<*> (x Lude..@ "AutoScalingGroupName")
      Lude.<*> (x Lude..@? "LaunchConfigurationName")
      Lude.<*> (x Lude..@? "LaunchTemplate")
      Lude.<*> (x Lude..@ "HealthStatus")
      Lude.<*> (x Lude..@ "LifecycleState")
