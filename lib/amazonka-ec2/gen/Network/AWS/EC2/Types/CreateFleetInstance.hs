{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CreateFleetInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CreateFleetInstance where

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
-- /See:/ 'createFleetInstance' smart constructor.
data CreateFleetInstance = CreateFleetInstance'
  { _cfiPlatform ::
      !(Maybe PlatformValues),
    _cfiLifecycle :: !(Maybe InstanceLifecycle),
    _cfiLaunchTemplateAndOverrides ::
      !(Maybe LaunchTemplateAndOverridesResponse),
    _cfiInstanceType :: !(Maybe InstanceType),
    _cfiInstanceIds :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateFleetInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfiPlatform' - The value is @Windows@ for Windows instances. Otherwise, the value is blank.
--
-- * 'cfiLifecycle' - Indicates if the instance that was launched is a Spot Instance or On-Demand Instance.
--
-- * 'cfiLaunchTemplateAndOverrides' - The launch templates and overrides that were used for launching the instances. The values that you specify in the Overrides replace the values in the launch template.
--
-- * 'cfiInstanceType' - The instance type.
--
-- * 'cfiInstanceIds' - The IDs of the instances.
createFleetInstance ::
  CreateFleetInstance
createFleetInstance =
  CreateFleetInstance'
    { _cfiPlatform = Nothing,
      _cfiLifecycle = Nothing,
      _cfiLaunchTemplateAndOverrides = Nothing,
      _cfiInstanceType = Nothing,
      _cfiInstanceIds = Nothing
    }

-- | The value is @Windows@ for Windows instances. Otherwise, the value is blank.
cfiPlatform :: Lens' CreateFleetInstance (Maybe PlatformValues)
cfiPlatform = lens _cfiPlatform (\s a -> s {_cfiPlatform = a})

-- | Indicates if the instance that was launched is a Spot Instance or On-Demand Instance.
cfiLifecycle :: Lens' CreateFleetInstance (Maybe InstanceLifecycle)
cfiLifecycle = lens _cfiLifecycle (\s a -> s {_cfiLifecycle = a})

-- | The launch templates and overrides that were used for launching the instances. The values that you specify in the Overrides replace the values in the launch template.
cfiLaunchTemplateAndOverrides :: Lens' CreateFleetInstance (Maybe LaunchTemplateAndOverridesResponse)
cfiLaunchTemplateAndOverrides = lens _cfiLaunchTemplateAndOverrides (\s a -> s {_cfiLaunchTemplateAndOverrides = a})

-- | The instance type.
cfiInstanceType :: Lens' CreateFleetInstance (Maybe InstanceType)
cfiInstanceType = lens _cfiInstanceType (\s a -> s {_cfiInstanceType = a})

-- | The IDs of the instances.
cfiInstanceIds :: Lens' CreateFleetInstance [Text]
cfiInstanceIds = lens _cfiInstanceIds (\s a -> s {_cfiInstanceIds = a}) . _Default . _Coerce

instance FromXML CreateFleetInstance where
  parseXML x =
    CreateFleetInstance'
      <$> (x .@? "platform")
      <*> (x .@? "lifecycle")
      <*> (x .@? "launchTemplateAndOverrides")
      <*> (x .@? "instanceType")
      <*> (x .@? "instanceIds" .!@ mempty >>= may (parseXMLList "item"))

instance Hashable CreateFleetInstance

instance NFData CreateFleetInstance
