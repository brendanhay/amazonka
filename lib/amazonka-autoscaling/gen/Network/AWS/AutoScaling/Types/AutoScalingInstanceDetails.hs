{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.AutoScalingInstanceDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.AutoScalingInstanceDetails where

import Network.AWS.AutoScaling.Types.LaunchTemplateSpecification
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an EC2 instance associated with an Auto Scaling group.
--
--
--
-- /See:/ 'autoScalingInstanceDetails' smart constructor.
data AutoScalingInstanceDetails = AutoScalingInstanceDetails'
  { _asidWeightedCapacity ::
      !(Maybe Text),
    _asidInstanceType :: !(Maybe Text),
    _asidLaunchConfigurationName ::
      !(Maybe Text),
    _asidLaunchTemplate ::
      !(Maybe LaunchTemplateSpecification),
    _asidInstanceId :: !Text,
    _asidAutoScalingGroupName :: !Text,
    _asidAvailabilityZone :: !Text,
    _asidLifecycleState :: !Text,
    _asidHealthStatus :: !Text,
    _asidProtectedFromScaleIn :: !Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AutoScalingInstanceDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asidWeightedCapacity' - The number of capacity units contributed by the instance based on its instance type. Valid Range: Minimum value of 1. Maximum value of 999.
--
-- * 'asidInstanceType' - The instance type of the EC2 instance.
--
-- * 'asidLaunchConfigurationName' - The launch configuration used to launch the instance. This value is not available if you attached the instance to the Auto Scaling group.
--
-- * 'asidLaunchTemplate' - The launch template for the instance.
--
-- * 'asidInstanceId' - The ID of the instance.
--
-- * 'asidAutoScalingGroupName' - The name of the Auto Scaling group for the instance.
--
-- * 'asidAvailabilityZone' - The Availability Zone for the instance.
--
-- * 'asidLifecycleState' - The lifecycle state for the instance.
--
-- * 'asidHealthStatus' - The last reported health status of this instance. "Healthy" means that the instance is healthy and should remain in service. "Unhealthy" means that the instance is unhealthy and Amazon EC2 Auto Scaling should terminate and replace it.
--
-- * 'asidProtectedFromScaleIn' - Indicates whether the instance is protected from termination by Amazon EC2 Auto Scaling when scaling in.
autoScalingInstanceDetails ::
  -- | 'asidInstanceId'
  Text ->
  -- | 'asidAutoScalingGroupName'
  Text ->
  -- | 'asidAvailabilityZone'
  Text ->
  -- | 'asidLifecycleState'
  Text ->
  -- | 'asidHealthStatus'
  Text ->
  -- | 'asidProtectedFromScaleIn'
  Bool ->
  AutoScalingInstanceDetails
autoScalingInstanceDetails
  pInstanceId_
  pAutoScalingGroupName_
  pAvailabilityZone_
  pLifecycleState_
  pHealthStatus_
  pProtectedFromScaleIn_ =
    AutoScalingInstanceDetails'
      { _asidWeightedCapacity = Nothing,
        _asidInstanceType = Nothing,
        _asidLaunchConfigurationName = Nothing,
        _asidLaunchTemplate = Nothing,
        _asidInstanceId = pInstanceId_,
        _asidAutoScalingGroupName = pAutoScalingGroupName_,
        _asidAvailabilityZone = pAvailabilityZone_,
        _asidLifecycleState = pLifecycleState_,
        _asidHealthStatus = pHealthStatus_,
        _asidProtectedFromScaleIn = pProtectedFromScaleIn_
      }

-- | The number of capacity units contributed by the instance based on its instance type. Valid Range: Minimum value of 1. Maximum value of 999.
asidWeightedCapacity :: Lens' AutoScalingInstanceDetails (Maybe Text)
asidWeightedCapacity = lens _asidWeightedCapacity (\s a -> s {_asidWeightedCapacity = a})

-- | The instance type of the EC2 instance.
asidInstanceType :: Lens' AutoScalingInstanceDetails (Maybe Text)
asidInstanceType = lens _asidInstanceType (\s a -> s {_asidInstanceType = a})

-- | The launch configuration used to launch the instance. This value is not available if you attached the instance to the Auto Scaling group.
asidLaunchConfigurationName :: Lens' AutoScalingInstanceDetails (Maybe Text)
asidLaunchConfigurationName = lens _asidLaunchConfigurationName (\s a -> s {_asidLaunchConfigurationName = a})

-- | The launch template for the instance.
asidLaunchTemplate :: Lens' AutoScalingInstanceDetails (Maybe LaunchTemplateSpecification)
asidLaunchTemplate = lens _asidLaunchTemplate (\s a -> s {_asidLaunchTemplate = a})

-- | The ID of the instance.
asidInstanceId :: Lens' AutoScalingInstanceDetails Text
asidInstanceId = lens _asidInstanceId (\s a -> s {_asidInstanceId = a})

-- | The name of the Auto Scaling group for the instance.
asidAutoScalingGroupName :: Lens' AutoScalingInstanceDetails Text
asidAutoScalingGroupName = lens _asidAutoScalingGroupName (\s a -> s {_asidAutoScalingGroupName = a})

-- | The Availability Zone for the instance.
asidAvailabilityZone :: Lens' AutoScalingInstanceDetails Text
asidAvailabilityZone = lens _asidAvailabilityZone (\s a -> s {_asidAvailabilityZone = a})

-- | The lifecycle state for the instance.
asidLifecycleState :: Lens' AutoScalingInstanceDetails Text
asidLifecycleState = lens _asidLifecycleState (\s a -> s {_asidLifecycleState = a})

-- | The last reported health status of this instance. "Healthy" means that the instance is healthy and should remain in service. "Unhealthy" means that the instance is unhealthy and Amazon EC2 Auto Scaling should terminate and replace it.
asidHealthStatus :: Lens' AutoScalingInstanceDetails Text
asidHealthStatus = lens _asidHealthStatus (\s a -> s {_asidHealthStatus = a})

-- | Indicates whether the instance is protected from termination by Amazon EC2 Auto Scaling when scaling in.
asidProtectedFromScaleIn :: Lens' AutoScalingInstanceDetails Bool
asidProtectedFromScaleIn = lens _asidProtectedFromScaleIn (\s a -> s {_asidProtectedFromScaleIn = a})

instance FromXML AutoScalingInstanceDetails where
  parseXML x =
    AutoScalingInstanceDetails'
      <$> (x .@? "WeightedCapacity")
      <*> (x .@? "InstanceType")
      <*> (x .@? "LaunchConfigurationName")
      <*> (x .@? "LaunchTemplate")
      <*> (x .@ "InstanceId")
      <*> (x .@ "AutoScalingGroupName")
      <*> (x .@ "AvailabilityZone")
      <*> (x .@ "LifecycleState")
      <*> (x .@ "HealthStatus")
      <*> (x .@ "ProtectedFromScaleIn")

instance Hashable AutoScalingInstanceDetails

instance NFData AutoScalingInstanceDetails
