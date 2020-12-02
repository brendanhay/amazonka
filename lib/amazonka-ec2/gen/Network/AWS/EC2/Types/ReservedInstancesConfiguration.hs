{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ReservedInstancesConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ReservedInstancesConfiguration where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InstanceType
import Network.AWS.EC2.Types.Scope
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the configuration settings for the modified Reserved Instances.
--
--
--
-- /See:/ 'reservedInstancesConfiguration' smart constructor.
data ReservedInstancesConfiguration = ReservedInstancesConfiguration'
  { _ricPlatform ::
      !(Maybe Text),
    _ricInstanceCount ::
      !(Maybe Int),
    _ricInstanceType ::
      !(Maybe InstanceType),
    _ricAvailabilityZone ::
      !(Maybe Text),
    _ricScope :: !(Maybe Scope)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReservedInstancesConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ricPlatform' - The network platform of the modified Reserved Instances, which is either EC2-Classic or EC2-VPC.
--
-- * 'ricInstanceCount' - The number of modified Reserved Instances.
--
-- * 'ricInstanceType' - The instance type for the modified Reserved Instances.
--
-- * 'ricAvailabilityZone' - The Availability Zone for the modified Reserved Instances.
--
-- * 'ricScope' - Whether the Reserved Instance is applied to instances in a Region or instances in a specific Availability Zone.
reservedInstancesConfiguration ::
  ReservedInstancesConfiguration
reservedInstancesConfiguration =
  ReservedInstancesConfiguration'
    { _ricPlatform = Nothing,
      _ricInstanceCount = Nothing,
      _ricInstanceType = Nothing,
      _ricAvailabilityZone = Nothing,
      _ricScope = Nothing
    }

-- | The network platform of the modified Reserved Instances, which is either EC2-Classic or EC2-VPC.
ricPlatform :: Lens' ReservedInstancesConfiguration (Maybe Text)
ricPlatform = lens _ricPlatform (\s a -> s {_ricPlatform = a})

-- | The number of modified Reserved Instances.
ricInstanceCount :: Lens' ReservedInstancesConfiguration (Maybe Int)
ricInstanceCount = lens _ricInstanceCount (\s a -> s {_ricInstanceCount = a})

-- | The instance type for the modified Reserved Instances.
ricInstanceType :: Lens' ReservedInstancesConfiguration (Maybe InstanceType)
ricInstanceType = lens _ricInstanceType (\s a -> s {_ricInstanceType = a})

-- | The Availability Zone for the modified Reserved Instances.
ricAvailabilityZone :: Lens' ReservedInstancesConfiguration (Maybe Text)
ricAvailabilityZone = lens _ricAvailabilityZone (\s a -> s {_ricAvailabilityZone = a})

-- | Whether the Reserved Instance is applied to instances in a Region or instances in a specific Availability Zone.
ricScope :: Lens' ReservedInstancesConfiguration (Maybe Scope)
ricScope = lens _ricScope (\s a -> s {_ricScope = a})

instance FromXML ReservedInstancesConfiguration where
  parseXML x =
    ReservedInstancesConfiguration'
      <$> (x .@? "platform")
      <*> (x .@? "instanceCount")
      <*> (x .@? "instanceType")
      <*> (x .@? "availabilityZone")
      <*> (x .@? "scope")

instance Hashable ReservedInstancesConfiguration

instance NFData ReservedInstancesConfiguration

instance ToQuery ReservedInstancesConfiguration where
  toQuery ReservedInstancesConfiguration' {..} =
    mconcat
      [ "Platform" =: _ricPlatform,
        "InstanceCount" =: _ricInstanceCount,
        "InstanceType" =: _ricInstanceType,
        "AvailabilityZone" =: _ricAvailabilityZone,
        "Scope" =: _ricScope
      ]
