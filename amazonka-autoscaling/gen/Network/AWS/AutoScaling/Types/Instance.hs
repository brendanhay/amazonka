{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.Instance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.Instance where

import Network.AWS.AutoScaling.Types.LaunchTemplateSpecification
import Network.AWS.AutoScaling.Types.LifecycleState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an EC2 instance.
--
-- /See:/ 'newInstance' smart constructor.
data Instance = Instance'
  { -- | The instance type of the EC2 instance.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | The launch template for the instance.
    launchTemplate :: Prelude.Maybe LaunchTemplateSpecification,
    -- | The launch configuration associated with the instance.
    launchConfigurationName :: Prelude.Maybe Prelude.Text,
    -- | The number of capacity units contributed by the instance based on its
    -- instance type.
    --
    -- Valid Range: Minimum value of 1. Maximum value of 999.
    weightedCapacity :: Prelude.Maybe Prelude.Text,
    -- | The ID of the instance.
    instanceId :: Prelude.Text,
    -- | The Availability Zone in which the instance is running.
    availabilityZone :: Prelude.Text,
    -- | A description of the current lifecycle state. The @Quarantined@ state is
    -- not used. For information about lifecycle states, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/AutoScalingGroupLifecycle.html Instance lifecycle>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    lifecycleState :: LifecycleState,
    -- | The last reported health status of the instance. \"Healthy\" means that
    -- the instance is healthy and should remain in service. \"Unhealthy\"
    -- means that the instance is unhealthy and that Amazon EC2 Auto Scaling
    -- should terminate and replace it.
    healthStatus :: Prelude.Text,
    -- | Indicates whether the instance is protected from termination by Amazon
    -- EC2 Auto Scaling when scaling in.
    protectedFromScaleIn :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Instance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceType', 'instance_instanceType' - The instance type of the EC2 instance.
--
-- 'launchTemplate', 'instance_launchTemplate' - The launch template for the instance.
--
-- 'launchConfigurationName', 'instance_launchConfigurationName' - The launch configuration associated with the instance.
--
-- 'weightedCapacity', 'instance_weightedCapacity' - The number of capacity units contributed by the instance based on its
-- instance type.
--
-- Valid Range: Minimum value of 1. Maximum value of 999.
--
-- 'instanceId', 'instance_instanceId' - The ID of the instance.
--
-- 'availabilityZone', 'instance_availabilityZone' - The Availability Zone in which the instance is running.
--
-- 'lifecycleState', 'instance_lifecycleState' - A description of the current lifecycle state. The @Quarantined@ state is
-- not used. For information about lifecycle states, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/AutoScalingGroupLifecycle.html Instance lifecycle>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'healthStatus', 'instance_healthStatus' - The last reported health status of the instance. \"Healthy\" means that
-- the instance is healthy and should remain in service. \"Unhealthy\"
-- means that the instance is unhealthy and that Amazon EC2 Auto Scaling
-- should terminate and replace it.
--
-- 'protectedFromScaleIn', 'instance_protectedFromScaleIn' - Indicates whether the instance is protected from termination by Amazon
-- EC2 Auto Scaling when scaling in.
newInstance ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'availabilityZone'
  Prelude.Text ->
  -- | 'lifecycleState'
  LifecycleState ->
  -- | 'healthStatus'
  Prelude.Text ->
  -- | 'protectedFromScaleIn'
  Prelude.Bool ->
  Instance
newInstance
  pInstanceId_
  pAvailabilityZone_
  pLifecycleState_
  pHealthStatus_
  pProtectedFromScaleIn_ =
    Instance'
      { instanceType = Prelude.Nothing,
        launchTemplate = Prelude.Nothing,
        launchConfigurationName = Prelude.Nothing,
        weightedCapacity = Prelude.Nothing,
        instanceId = pInstanceId_,
        availabilityZone = pAvailabilityZone_,
        lifecycleState = pLifecycleState_,
        healthStatus = pHealthStatus_,
        protectedFromScaleIn = pProtectedFromScaleIn_
      }

-- | The instance type of the EC2 instance.
instance_instanceType :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_instanceType = Lens.lens (\Instance' {instanceType} -> instanceType) (\s@Instance' {} a -> s {instanceType = a} :: Instance)

-- | The launch template for the instance.
instance_launchTemplate :: Lens.Lens' Instance (Prelude.Maybe LaunchTemplateSpecification)
instance_launchTemplate = Lens.lens (\Instance' {launchTemplate} -> launchTemplate) (\s@Instance' {} a -> s {launchTemplate = a} :: Instance)

-- | The launch configuration associated with the instance.
instance_launchConfigurationName :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_launchConfigurationName = Lens.lens (\Instance' {launchConfigurationName} -> launchConfigurationName) (\s@Instance' {} a -> s {launchConfigurationName = a} :: Instance)

-- | The number of capacity units contributed by the instance based on its
-- instance type.
--
-- Valid Range: Minimum value of 1. Maximum value of 999.
instance_weightedCapacity :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_weightedCapacity = Lens.lens (\Instance' {weightedCapacity} -> weightedCapacity) (\s@Instance' {} a -> s {weightedCapacity = a} :: Instance)

-- | The ID of the instance.
instance_instanceId :: Lens.Lens' Instance Prelude.Text
instance_instanceId = Lens.lens (\Instance' {instanceId} -> instanceId) (\s@Instance' {} a -> s {instanceId = a} :: Instance)

-- | The Availability Zone in which the instance is running.
instance_availabilityZone :: Lens.Lens' Instance Prelude.Text
instance_availabilityZone = Lens.lens (\Instance' {availabilityZone} -> availabilityZone) (\s@Instance' {} a -> s {availabilityZone = a} :: Instance)

-- | A description of the current lifecycle state. The @Quarantined@ state is
-- not used. For information about lifecycle states, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/AutoScalingGroupLifecycle.html Instance lifecycle>
-- in the /Amazon EC2 Auto Scaling User Guide/.
instance_lifecycleState :: Lens.Lens' Instance LifecycleState
instance_lifecycleState = Lens.lens (\Instance' {lifecycleState} -> lifecycleState) (\s@Instance' {} a -> s {lifecycleState = a} :: Instance)

-- | The last reported health status of the instance. \"Healthy\" means that
-- the instance is healthy and should remain in service. \"Unhealthy\"
-- means that the instance is unhealthy and that Amazon EC2 Auto Scaling
-- should terminate and replace it.
instance_healthStatus :: Lens.Lens' Instance Prelude.Text
instance_healthStatus = Lens.lens (\Instance' {healthStatus} -> healthStatus) (\s@Instance' {} a -> s {healthStatus = a} :: Instance)

-- | Indicates whether the instance is protected from termination by Amazon
-- EC2 Auto Scaling when scaling in.
instance_protectedFromScaleIn :: Lens.Lens' Instance Prelude.Bool
instance_protectedFromScaleIn = Lens.lens (\Instance' {protectedFromScaleIn} -> protectedFromScaleIn) (\s@Instance' {} a -> s {protectedFromScaleIn = a} :: Instance)

instance Prelude.FromXML Instance where
  parseXML x =
    Instance'
      Prelude.<$> (x Prelude..@? "InstanceType")
      Prelude.<*> (x Prelude..@? "LaunchTemplate")
      Prelude.<*> (x Prelude..@? "LaunchConfigurationName")
      Prelude.<*> (x Prelude..@? "WeightedCapacity")
      Prelude.<*> (x Prelude..@ "InstanceId")
      Prelude.<*> (x Prelude..@ "AvailabilityZone")
      Prelude.<*> (x Prelude..@ "LifecycleState")
      Prelude.<*> (x Prelude..@ "HealthStatus")
      Prelude.<*> (x Prelude..@ "ProtectedFromScaleIn")

instance Prelude.Hashable Instance

instance Prelude.NFData Instance
