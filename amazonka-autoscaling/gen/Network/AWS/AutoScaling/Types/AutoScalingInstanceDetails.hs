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
-- Module      : Network.AWS.AutoScaling.Types.AutoScalingInstanceDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.AutoScalingInstanceDetails where

import Network.AWS.AutoScaling.Types.LaunchTemplateSpecification
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an EC2 instance associated with an Auto Scaling group.
--
-- /See:/ 'newAutoScalingInstanceDetails' smart constructor.
data AutoScalingInstanceDetails = AutoScalingInstanceDetails'
  { -- | The instance type of the EC2 instance.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | The launch template for the instance.
    launchTemplate :: Prelude.Maybe LaunchTemplateSpecification,
    -- | The launch configuration used to launch the instance. This value is not
    -- available if you attached the instance to the Auto Scaling group.
    launchConfigurationName :: Prelude.Maybe Prelude.Text,
    -- | The number of capacity units contributed by the instance based on its
    -- instance type.
    --
    -- Valid Range: Minimum value of 1. Maximum value of 999.
    weightedCapacity :: Prelude.Maybe Prelude.Text,
    -- | The ID of the instance.
    instanceId :: Prelude.Text,
    -- | The name of the Auto Scaling group for the instance.
    autoScalingGroupName :: Prelude.Text,
    -- | The Availability Zone for the instance.
    availabilityZone :: Prelude.Text,
    -- | The lifecycle state for the instance. The @Quarantined@ state is not
    -- used. For information about lifecycle states, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/AutoScalingGroupLifecycle.html Instance lifecycle>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    --
    -- Valid Values: @Pending@ | @Pending:Wait@ | @Pending:Proceed@ |
    -- @Quarantined@ | @InService@ | @Terminating@ | @Terminating:Wait@ |
    -- @Terminating:Proceed@ | @Terminated@ | @Detaching@ | @Detached@ |
    -- @EnteringStandby@ | @Standby@
    lifecycleState :: Prelude.Text,
    -- | The last reported health status of this instance. \"Healthy\" means that
    -- the instance is healthy and should remain in service. \"Unhealthy\"
    -- means that the instance is unhealthy and Amazon EC2 Auto Scaling should
    -- terminate and replace it.
    healthStatus :: Prelude.Text,
    -- | Indicates whether the instance is protected from termination by Amazon
    -- EC2 Auto Scaling when scaling in.
    protectedFromScaleIn :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AutoScalingInstanceDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceType', 'autoScalingInstanceDetails_instanceType' - The instance type of the EC2 instance.
--
-- 'launchTemplate', 'autoScalingInstanceDetails_launchTemplate' - The launch template for the instance.
--
-- 'launchConfigurationName', 'autoScalingInstanceDetails_launchConfigurationName' - The launch configuration used to launch the instance. This value is not
-- available if you attached the instance to the Auto Scaling group.
--
-- 'weightedCapacity', 'autoScalingInstanceDetails_weightedCapacity' - The number of capacity units contributed by the instance based on its
-- instance type.
--
-- Valid Range: Minimum value of 1. Maximum value of 999.
--
-- 'instanceId', 'autoScalingInstanceDetails_instanceId' - The ID of the instance.
--
-- 'autoScalingGroupName', 'autoScalingInstanceDetails_autoScalingGroupName' - The name of the Auto Scaling group for the instance.
--
-- 'availabilityZone', 'autoScalingInstanceDetails_availabilityZone' - The Availability Zone for the instance.
--
-- 'lifecycleState', 'autoScalingInstanceDetails_lifecycleState' - The lifecycle state for the instance. The @Quarantined@ state is not
-- used. For information about lifecycle states, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/AutoScalingGroupLifecycle.html Instance lifecycle>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- Valid Values: @Pending@ | @Pending:Wait@ | @Pending:Proceed@ |
-- @Quarantined@ | @InService@ | @Terminating@ | @Terminating:Wait@ |
-- @Terminating:Proceed@ | @Terminated@ | @Detaching@ | @Detached@ |
-- @EnteringStandby@ | @Standby@
--
-- 'healthStatus', 'autoScalingInstanceDetails_healthStatus' - The last reported health status of this instance. \"Healthy\" means that
-- the instance is healthy and should remain in service. \"Unhealthy\"
-- means that the instance is unhealthy and Amazon EC2 Auto Scaling should
-- terminate and replace it.
--
-- 'protectedFromScaleIn', 'autoScalingInstanceDetails_protectedFromScaleIn' - Indicates whether the instance is protected from termination by Amazon
-- EC2 Auto Scaling when scaling in.
newAutoScalingInstanceDetails ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'autoScalingGroupName'
  Prelude.Text ->
  -- | 'availabilityZone'
  Prelude.Text ->
  -- | 'lifecycleState'
  Prelude.Text ->
  -- | 'healthStatus'
  Prelude.Text ->
  -- | 'protectedFromScaleIn'
  Prelude.Bool ->
  AutoScalingInstanceDetails
newAutoScalingInstanceDetails
  pInstanceId_
  pAutoScalingGroupName_
  pAvailabilityZone_
  pLifecycleState_
  pHealthStatus_
  pProtectedFromScaleIn_ =
    AutoScalingInstanceDetails'
      { instanceType =
          Prelude.Nothing,
        launchTemplate = Prelude.Nothing,
        launchConfigurationName = Prelude.Nothing,
        weightedCapacity = Prelude.Nothing,
        instanceId = pInstanceId_,
        autoScalingGroupName = pAutoScalingGroupName_,
        availabilityZone = pAvailabilityZone_,
        lifecycleState = pLifecycleState_,
        healthStatus = pHealthStatus_,
        protectedFromScaleIn = pProtectedFromScaleIn_
      }

-- | The instance type of the EC2 instance.
autoScalingInstanceDetails_instanceType :: Lens.Lens' AutoScalingInstanceDetails (Prelude.Maybe Prelude.Text)
autoScalingInstanceDetails_instanceType = Lens.lens (\AutoScalingInstanceDetails' {instanceType} -> instanceType) (\s@AutoScalingInstanceDetails' {} a -> s {instanceType = a} :: AutoScalingInstanceDetails)

-- | The launch template for the instance.
autoScalingInstanceDetails_launchTemplate :: Lens.Lens' AutoScalingInstanceDetails (Prelude.Maybe LaunchTemplateSpecification)
autoScalingInstanceDetails_launchTemplate = Lens.lens (\AutoScalingInstanceDetails' {launchTemplate} -> launchTemplate) (\s@AutoScalingInstanceDetails' {} a -> s {launchTemplate = a} :: AutoScalingInstanceDetails)

-- | The launch configuration used to launch the instance. This value is not
-- available if you attached the instance to the Auto Scaling group.
autoScalingInstanceDetails_launchConfigurationName :: Lens.Lens' AutoScalingInstanceDetails (Prelude.Maybe Prelude.Text)
autoScalingInstanceDetails_launchConfigurationName = Lens.lens (\AutoScalingInstanceDetails' {launchConfigurationName} -> launchConfigurationName) (\s@AutoScalingInstanceDetails' {} a -> s {launchConfigurationName = a} :: AutoScalingInstanceDetails)

-- | The number of capacity units contributed by the instance based on its
-- instance type.
--
-- Valid Range: Minimum value of 1. Maximum value of 999.
autoScalingInstanceDetails_weightedCapacity :: Lens.Lens' AutoScalingInstanceDetails (Prelude.Maybe Prelude.Text)
autoScalingInstanceDetails_weightedCapacity = Lens.lens (\AutoScalingInstanceDetails' {weightedCapacity} -> weightedCapacity) (\s@AutoScalingInstanceDetails' {} a -> s {weightedCapacity = a} :: AutoScalingInstanceDetails)

-- | The ID of the instance.
autoScalingInstanceDetails_instanceId :: Lens.Lens' AutoScalingInstanceDetails Prelude.Text
autoScalingInstanceDetails_instanceId = Lens.lens (\AutoScalingInstanceDetails' {instanceId} -> instanceId) (\s@AutoScalingInstanceDetails' {} a -> s {instanceId = a} :: AutoScalingInstanceDetails)

-- | The name of the Auto Scaling group for the instance.
autoScalingInstanceDetails_autoScalingGroupName :: Lens.Lens' AutoScalingInstanceDetails Prelude.Text
autoScalingInstanceDetails_autoScalingGroupName = Lens.lens (\AutoScalingInstanceDetails' {autoScalingGroupName} -> autoScalingGroupName) (\s@AutoScalingInstanceDetails' {} a -> s {autoScalingGroupName = a} :: AutoScalingInstanceDetails)

-- | The Availability Zone for the instance.
autoScalingInstanceDetails_availabilityZone :: Lens.Lens' AutoScalingInstanceDetails Prelude.Text
autoScalingInstanceDetails_availabilityZone = Lens.lens (\AutoScalingInstanceDetails' {availabilityZone} -> availabilityZone) (\s@AutoScalingInstanceDetails' {} a -> s {availabilityZone = a} :: AutoScalingInstanceDetails)

-- | The lifecycle state for the instance. The @Quarantined@ state is not
-- used. For information about lifecycle states, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/AutoScalingGroupLifecycle.html Instance lifecycle>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- Valid Values: @Pending@ | @Pending:Wait@ | @Pending:Proceed@ |
-- @Quarantined@ | @InService@ | @Terminating@ | @Terminating:Wait@ |
-- @Terminating:Proceed@ | @Terminated@ | @Detaching@ | @Detached@ |
-- @EnteringStandby@ | @Standby@
autoScalingInstanceDetails_lifecycleState :: Lens.Lens' AutoScalingInstanceDetails Prelude.Text
autoScalingInstanceDetails_lifecycleState = Lens.lens (\AutoScalingInstanceDetails' {lifecycleState} -> lifecycleState) (\s@AutoScalingInstanceDetails' {} a -> s {lifecycleState = a} :: AutoScalingInstanceDetails)

-- | The last reported health status of this instance. \"Healthy\" means that
-- the instance is healthy and should remain in service. \"Unhealthy\"
-- means that the instance is unhealthy and Amazon EC2 Auto Scaling should
-- terminate and replace it.
autoScalingInstanceDetails_healthStatus :: Lens.Lens' AutoScalingInstanceDetails Prelude.Text
autoScalingInstanceDetails_healthStatus = Lens.lens (\AutoScalingInstanceDetails' {healthStatus} -> healthStatus) (\s@AutoScalingInstanceDetails' {} a -> s {healthStatus = a} :: AutoScalingInstanceDetails)

-- | Indicates whether the instance is protected from termination by Amazon
-- EC2 Auto Scaling when scaling in.
autoScalingInstanceDetails_protectedFromScaleIn :: Lens.Lens' AutoScalingInstanceDetails Prelude.Bool
autoScalingInstanceDetails_protectedFromScaleIn = Lens.lens (\AutoScalingInstanceDetails' {protectedFromScaleIn} -> protectedFromScaleIn) (\s@AutoScalingInstanceDetails' {} a -> s {protectedFromScaleIn = a} :: AutoScalingInstanceDetails)

instance Prelude.FromXML AutoScalingInstanceDetails where
  parseXML x =
    AutoScalingInstanceDetails'
      Prelude.<$> (x Prelude..@? "InstanceType")
      Prelude.<*> (x Prelude..@? "LaunchTemplate")
      Prelude.<*> (x Prelude..@? "LaunchConfigurationName")
      Prelude.<*> (x Prelude..@? "WeightedCapacity")
      Prelude.<*> (x Prelude..@ "InstanceId")
      Prelude.<*> (x Prelude..@ "AutoScalingGroupName")
      Prelude.<*> (x Prelude..@ "AvailabilityZone")
      Prelude.<*> (x Prelude..@ "LifecycleState")
      Prelude.<*> (x Prelude..@ "HealthStatus")
      Prelude.<*> (x Prelude..@ "ProtectedFromScaleIn")

instance Prelude.Hashable AutoScalingInstanceDetails

instance Prelude.NFData AutoScalingInstanceDetails
