{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.Instance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.Instance where

import Network.AWS.AutoScaling.Types.LaunchTemplateSpecification
import Network.AWS.AutoScaling.Types.LifecycleState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an EC2 instance.
--
--
--
-- /See:/ 'instance'' smart constructor.
data Instance = Instance'
  { _iWeightedCapacity :: !(Maybe Text),
    _iInstanceType :: !(Maybe Text),
    _iLaunchConfigurationName :: !(Maybe Text),
    _iLaunchTemplate :: !(Maybe LaunchTemplateSpecification),
    _iInstanceId :: !Text,
    _iAvailabilityZone :: !Text,
    _iLifecycleState :: !LifecycleState,
    _iHealthStatus :: !Text,
    _iProtectedFromScaleIn :: !Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Instance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iWeightedCapacity' - The number of capacity units contributed by the instance based on its instance type. Valid Range: Minimum value of 1. Maximum value of 999.
--
-- * 'iInstanceType' - The instance type of the EC2 instance.
--
-- * 'iLaunchConfigurationName' - The launch configuration associated with the instance.
--
-- * 'iLaunchTemplate' - The launch template for the instance.
--
-- * 'iInstanceId' - The ID of the instance.
--
-- * 'iAvailabilityZone' - The Availability Zone in which the instance is running.
--
-- * 'iLifecycleState' - A description of the current lifecycle state. The @Quarantined@ state is not used.
--
-- * 'iHealthStatus' - The last reported health status of the instance. "Healthy" means that the instance is healthy and should remain in service. "Unhealthy" means that the instance is unhealthy and that Amazon EC2 Auto Scaling should terminate and replace it.
--
-- * 'iProtectedFromScaleIn' - Indicates whether the instance is protected from termination by Amazon EC2 Auto Scaling when scaling in.
instance' ::
  -- | 'iInstanceId'
  Text ->
  -- | 'iAvailabilityZone'
  Text ->
  -- | 'iLifecycleState'
  LifecycleState ->
  -- | 'iHealthStatus'
  Text ->
  -- | 'iProtectedFromScaleIn'
  Bool ->
  Instance
instance'
  pInstanceId_
  pAvailabilityZone_
  pLifecycleState_
  pHealthStatus_
  pProtectedFromScaleIn_ =
    Instance'
      { _iWeightedCapacity = Nothing,
        _iInstanceType = Nothing,
        _iLaunchConfigurationName = Nothing,
        _iLaunchTemplate = Nothing,
        _iInstanceId = pInstanceId_,
        _iAvailabilityZone = pAvailabilityZone_,
        _iLifecycleState = pLifecycleState_,
        _iHealthStatus = pHealthStatus_,
        _iProtectedFromScaleIn = pProtectedFromScaleIn_
      }

-- | The number of capacity units contributed by the instance based on its instance type. Valid Range: Minimum value of 1. Maximum value of 999.
iWeightedCapacity :: Lens' Instance (Maybe Text)
iWeightedCapacity = lens _iWeightedCapacity (\s a -> s {_iWeightedCapacity = a})

-- | The instance type of the EC2 instance.
iInstanceType :: Lens' Instance (Maybe Text)
iInstanceType = lens _iInstanceType (\s a -> s {_iInstanceType = a})

-- | The launch configuration associated with the instance.
iLaunchConfigurationName :: Lens' Instance (Maybe Text)
iLaunchConfigurationName = lens _iLaunchConfigurationName (\s a -> s {_iLaunchConfigurationName = a})

-- | The launch template for the instance.
iLaunchTemplate :: Lens' Instance (Maybe LaunchTemplateSpecification)
iLaunchTemplate = lens _iLaunchTemplate (\s a -> s {_iLaunchTemplate = a})

-- | The ID of the instance.
iInstanceId :: Lens' Instance Text
iInstanceId = lens _iInstanceId (\s a -> s {_iInstanceId = a})

-- | The Availability Zone in which the instance is running.
iAvailabilityZone :: Lens' Instance Text
iAvailabilityZone = lens _iAvailabilityZone (\s a -> s {_iAvailabilityZone = a})

-- | A description of the current lifecycle state. The @Quarantined@ state is not used.
iLifecycleState :: Lens' Instance LifecycleState
iLifecycleState = lens _iLifecycleState (\s a -> s {_iLifecycleState = a})

-- | The last reported health status of the instance. "Healthy" means that the instance is healthy and should remain in service. "Unhealthy" means that the instance is unhealthy and that Amazon EC2 Auto Scaling should terminate and replace it.
iHealthStatus :: Lens' Instance Text
iHealthStatus = lens _iHealthStatus (\s a -> s {_iHealthStatus = a})

-- | Indicates whether the instance is protected from termination by Amazon EC2 Auto Scaling when scaling in.
iProtectedFromScaleIn :: Lens' Instance Bool
iProtectedFromScaleIn = lens _iProtectedFromScaleIn (\s a -> s {_iProtectedFromScaleIn = a})

instance FromXML Instance where
  parseXML x =
    Instance'
      <$> (x .@? "WeightedCapacity")
      <*> (x .@? "InstanceType")
      <*> (x .@? "LaunchConfigurationName")
      <*> (x .@? "LaunchTemplate")
      <*> (x .@ "InstanceId")
      <*> (x .@ "AvailabilityZone")
      <*> (x .@ "LifecycleState")
      <*> (x .@ "HealthStatus")
      <*> (x .@ "ProtectedFromScaleIn")

instance Hashable Instance

instance NFData Instance
