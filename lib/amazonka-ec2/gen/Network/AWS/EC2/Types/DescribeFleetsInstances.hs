{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DescribeFleetsInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DescribeFleetsInstances where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InstanceLifecycle
import Network.AWS.EC2.Types.InstanceType
import Network.AWS.EC2.Types.LaunchTemplateAndOverridesResponse
import Network.AWS.EC2.Types.PlatformValues
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the instances that were launched by the fleet.
--
--
--
-- /See:/ 'describeFleetsInstances' smart constructor.
data DescribeFleetsInstances = DescribeFleetsInstances'
  { _dfiPlatform ::
      !(Maybe PlatformValues),
    _dfiLifecycle :: !(Maybe InstanceLifecycle),
    _dfiLaunchTemplateAndOverrides ::
      !(Maybe LaunchTemplateAndOverridesResponse),
    _dfiInstanceType :: !(Maybe InstanceType),
    _dfiInstanceIds :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeFleetsInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfiPlatform' - The value is @Windows@ for Windows instances. Otherwise, the value is blank.
--
-- * 'dfiLifecycle' - Indicates if the instance that was launched is a Spot Instance or On-Demand Instance.
--
-- * 'dfiLaunchTemplateAndOverrides' - The launch templates and overrides that were used for launching the instances. The values that you specify in the Overrides replace the values in the launch template.
--
-- * 'dfiInstanceType' - The instance type.
--
-- * 'dfiInstanceIds' - The IDs of the instances.
describeFleetsInstances ::
  DescribeFleetsInstances
describeFleetsInstances =
  DescribeFleetsInstances'
    { _dfiPlatform = Nothing,
      _dfiLifecycle = Nothing,
      _dfiLaunchTemplateAndOverrides = Nothing,
      _dfiInstanceType = Nothing,
      _dfiInstanceIds = Nothing
    }

-- | The value is @Windows@ for Windows instances. Otherwise, the value is blank.
dfiPlatform :: Lens' DescribeFleetsInstances (Maybe PlatformValues)
dfiPlatform = lens _dfiPlatform (\s a -> s {_dfiPlatform = a})

-- | Indicates if the instance that was launched is a Spot Instance or On-Demand Instance.
dfiLifecycle :: Lens' DescribeFleetsInstances (Maybe InstanceLifecycle)
dfiLifecycle = lens _dfiLifecycle (\s a -> s {_dfiLifecycle = a})

-- | The launch templates and overrides that were used for launching the instances. The values that you specify in the Overrides replace the values in the launch template.
dfiLaunchTemplateAndOverrides :: Lens' DescribeFleetsInstances (Maybe LaunchTemplateAndOverridesResponse)
dfiLaunchTemplateAndOverrides = lens _dfiLaunchTemplateAndOverrides (\s a -> s {_dfiLaunchTemplateAndOverrides = a})

-- | The instance type.
dfiInstanceType :: Lens' DescribeFleetsInstances (Maybe InstanceType)
dfiInstanceType = lens _dfiInstanceType (\s a -> s {_dfiInstanceType = a})

-- | The IDs of the instances.
dfiInstanceIds :: Lens' DescribeFleetsInstances [Text]
dfiInstanceIds = lens _dfiInstanceIds (\s a -> s {_dfiInstanceIds = a}) . _Default . _Coerce

instance FromXML DescribeFleetsInstances where
  parseXML x =
    DescribeFleetsInstances'
      <$> (x .@? "platform")
      <*> (x .@? "lifecycle")
      <*> (x .@? "launchTemplateAndOverrides")
      <*> (x .@? "instanceType")
      <*> (x .@? "instanceIds" .!@ mempty >>= may (parseXMLList "item"))

instance Hashable DescribeFleetsInstances

instance NFData DescribeFleetsInstances
