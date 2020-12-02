{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.AWSVPCConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.AWSVPCConfiguration where

import Network.AWS.CloudWatchEvents.Types.AssignPublicIP
import Network.AWS.Lens
import Network.AWS.Prelude

-- | This structure specifies the VPC subnets and security groups for the task, and whether a public IP address is to be used. This structure is relevant only for ECS tasks that use the @awsvpc@ network mode.
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
-- * 'avcSecurityGroups' - Specifies the security groups associated with the task. These security groups must all be in the same VPC. You can specify as many as five security groups. If you do not specify a security group, the default security group for the VPC is used.
--
-- * 'avcAssignPublicIP' - Specifies whether the task's elastic network interface receives a public IP address. You can specify @ENABLED@ only when @LaunchType@ in @EcsParameters@ is set to @FARGATE@ .
--
-- * 'avcSubnets' - Specifies the subnets associated with the task. These subnets must all be in the same VPC. You can specify as many as 16 subnets.
awsVPCConfiguration ::
  AWSVPCConfiguration
awsVPCConfiguration =
  AWSVPCConfiguration'
    { _avcSecurityGroups = Nothing,
      _avcAssignPublicIP = Nothing,
      _avcSubnets = mempty
    }

-- | Specifies the security groups associated with the task. These security groups must all be in the same VPC. You can specify as many as five security groups. If you do not specify a security group, the default security group for the VPC is used.
avcSecurityGroups :: Lens' AWSVPCConfiguration [Text]
avcSecurityGroups = lens _avcSecurityGroups (\s a -> s {_avcSecurityGroups = a}) . _Default . _Coerce

-- | Specifies whether the task's elastic network interface receives a public IP address. You can specify @ENABLED@ only when @LaunchType@ in @EcsParameters@ is set to @FARGATE@ .
avcAssignPublicIP :: Lens' AWSVPCConfiguration (Maybe AssignPublicIP)
avcAssignPublicIP = lens _avcAssignPublicIP (\s a -> s {_avcAssignPublicIP = a})

-- | Specifies the subnets associated with the task. These subnets must all be in the same VPC. You can specify as many as 16 subnets.
avcSubnets :: Lens' AWSVPCConfiguration [Text]
avcSubnets = lens _avcSubnets (\s a -> s {_avcSubnets = a}) . _Coerce

instance FromJSON AWSVPCConfiguration where
  parseJSON =
    withObject
      "AWSVPCConfiguration"
      ( \x ->
          AWSVPCConfiguration'
            <$> (x .:? "SecurityGroups" .!= mempty)
            <*> (x .:? "AssignPublicIp")
            <*> (x .:? "Subnets" .!= mempty)
      )

instance Hashable AWSVPCConfiguration

instance NFData AWSVPCConfiguration

instance ToJSON AWSVPCConfiguration where
  toJSON AWSVPCConfiguration' {..} =
    object
      ( catMaybes
          [ ("SecurityGroups" .=) <$> _avcSecurityGroups,
            ("AssignPublicIp" .=) <$> _avcAssignPublicIP,
            Just ("Subnets" .= _avcSubnets)
          ]
      )
