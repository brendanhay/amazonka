{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.AWSVPCConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.AWSVPCConfiguration where

import Network.AWS.ECS.Types.AssignPublicIP
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object representing the networking details for a task or service.
--
--
--
-- /See:/ 'awsVPCConfiguration' smart constructor.
data AWSVPCConfiguration = AWSVPCConfiguration'
  { _avcSecurityGroups ::
      !(Maybe [Text]),
    _avcAssignPublicIP :: !(Maybe AssignPublicIP),
    _avcSubnets :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AWSVPCConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avcSecurityGroups' - The IDs of the security groups associated with the task or service. If you do not specify a security group, the default security group for the VPC is used. There is a limit of 5 security groups that can be specified per @AwsVpcConfiguration@ .
--
-- * 'avcAssignPublicIP' - Whether the task's elastic network interface receives a public IP address. The default value is @DISABLED@ .
--
-- * 'avcSubnets' - The IDs of the subnets associated with the task or service. There is a limit of 16 subnets that can be specified per @AwsVpcConfiguration@ .
awsVPCConfiguration ::
  AWSVPCConfiguration
awsVPCConfiguration =
  AWSVPCConfiguration'
    { _avcSecurityGroups = Nothing,
      _avcAssignPublicIP = Nothing,
      _avcSubnets = mempty
    }

-- | The IDs of the security groups associated with the task or service. If you do not specify a security group, the default security group for the VPC is used. There is a limit of 5 security groups that can be specified per @AwsVpcConfiguration@ .
avcSecurityGroups :: Lens' AWSVPCConfiguration [Text]
avcSecurityGroups = lens _avcSecurityGroups (\s a -> s {_avcSecurityGroups = a}) . _Default . _Coerce

-- | Whether the task's elastic network interface receives a public IP address. The default value is @DISABLED@ .
avcAssignPublicIP :: Lens' AWSVPCConfiguration (Maybe AssignPublicIP)
avcAssignPublicIP = lens _avcAssignPublicIP (\s a -> s {_avcAssignPublicIP = a})

-- | The IDs of the subnets associated with the task or service. There is a limit of 16 subnets that can be specified per @AwsVpcConfiguration@ .
avcSubnets :: Lens' AWSVPCConfiguration [Text]
avcSubnets = lens _avcSubnets (\s a -> s {_avcSubnets = a}) . _Coerce

instance FromJSON AWSVPCConfiguration where
  parseJSON =
    withObject
      "AWSVPCConfiguration"
      ( \x ->
          AWSVPCConfiguration'
            <$> (x .:? "securityGroups" .!= mempty)
            <*> (x .:? "assignPublicIp")
            <*> (x .:? "subnets" .!= mempty)
      )

instance Hashable AWSVPCConfiguration

instance NFData AWSVPCConfiguration

instance ToJSON AWSVPCConfiguration where
  toJSON AWSVPCConfiguration' {..} =
    object
      ( catMaybes
          [ ("securityGroups" .=) <$> _avcSecurityGroups,
            ("assignPublicIp" .=) <$> _avcAssignPublicIP,
            Just ("subnets" .= _avcSubnets)
          ]
      )
