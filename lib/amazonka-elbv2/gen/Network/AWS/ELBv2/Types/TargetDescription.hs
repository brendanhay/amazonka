{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.TargetDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.TargetDescription where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a target.
--
--
--
-- /See:/ 'targetDescription' smart constructor.
data TargetDescription = TargetDescription'
  { _tdAvailabilityZone ::
      !(Maybe Text),
    _tdPort :: !(Maybe Nat),
    _tdId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TargetDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdAvailabilityZone' - An Availability Zone or @all@ . This determines whether the target receives traffic from the load balancer nodes in the specified Availability Zone or from all enabled Availability Zones for the load balancer. This parameter is not supported if the target type of the target group is @instance@ . If the target type is @ip@ and the IP address is in a subnet of the VPC for the target group, the Availability Zone is automatically detected and this parameter is optional. If the IP address is outside the VPC, this parameter is required. With an Application Load Balancer, if the target type is @ip@ and the IP address is outside the VPC for the target group, the only supported value is @all@ . If the target type is @lambda@ , this parameter is optional and the only supported value is @all@ .
--
-- * 'tdPort' - The port on which the target is listening. If the target group protocol is GENEVE, the supported port is 6081. Not used if the target is a Lambda function.
--
-- * 'tdId' - The ID of the target. If the target type of the target group is @instance@ , specify an instance ID. If the target type is @ip@ , specify an IP address. If the target type is @lambda@ , specify the ARN of the Lambda function.
targetDescription ::
  -- | 'tdId'
  Text ->
  TargetDescription
targetDescription pId_ =
  TargetDescription'
    { _tdAvailabilityZone = Nothing,
      _tdPort = Nothing,
      _tdId = pId_
    }

-- | An Availability Zone or @all@ . This determines whether the target receives traffic from the load balancer nodes in the specified Availability Zone or from all enabled Availability Zones for the load balancer. This parameter is not supported if the target type of the target group is @instance@ . If the target type is @ip@ and the IP address is in a subnet of the VPC for the target group, the Availability Zone is automatically detected and this parameter is optional. If the IP address is outside the VPC, this parameter is required. With an Application Load Balancer, if the target type is @ip@ and the IP address is outside the VPC for the target group, the only supported value is @all@ . If the target type is @lambda@ , this parameter is optional and the only supported value is @all@ .
tdAvailabilityZone :: Lens' TargetDescription (Maybe Text)
tdAvailabilityZone = lens _tdAvailabilityZone (\s a -> s {_tdAvailabilityZone = a})

-- | The port on which the target is listening. If the target group protocol is GENEVE, the supported port is 6081. Not used if the target is a Lambda function.
tdPort :: Lens' TargetDescription (Maybe Natural)
tdPort = lens _tdPort (\s a -> s {_tdPort = a}) . mapping _Nat

-- | The ID of the target. If the target type of the target group is @instance@ , specify an instance ID. If the target type is @ip@ , specify an IP address. If the target type is @lambda@ , specify the ARN of the Lambda function.
tdId :: Lens' TargetDescription Text
tdId = lens _tdId (\s a -> s {_tdId = a})

instance FromXML TargetDescription where
  parseXML x =
    TargetDescription'
      <$> (x .@? "AvailabilityZone") <*> (x .@? "Port") <*> (x .@ "Id")

instance Hashable TargetDescription

instance NFData TargetDescription

instance ToQuery TargetDescription where
  toQuery TargetDescription' {..} =
    mconcat
      [ "AvailabilityZone" =: _tdAvailabilityZone,
        "Port" =: _tdPort,
        "Id" =: _tdId
      ]
