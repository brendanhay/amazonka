{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputVPCRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputVPCRequest where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Settings for a private VPC Input.
--
-- When this property is specified, the input destination addresses will be created in a VPC rather than with public Internet addresses.
-- This property requires setting the roleArn property on Input creation.
-- Not compatible with the inputSecurityGroups property.
--
-- /See:/ 'inputVPCRequest' smart constructor.
data InputVPCRequest = InputVPCRequest'
  { _ivrSecurityGroupIds ::
      !(Maybe [Text]),
    _ivrSubnetIds :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputVPCRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ivrSecurityGroupIds' - A list of up to 5 EC2 VPC security group IDs to attach to the Input VPC network interfaces. Requires subnetIds. If none are specified then the VPC default security group will be used.
--
-- * 'ivrSubnetIds' - A list of 2 VPC subnet IDs from the same VPC. Subnet IDs must be mapped to two unique availability zones (AZ).
inputVPCRequest ::
  InputVPCRequest
inputVPCRequest =
  InputVPCRequest'
    { _ivrSecurityGroupIds = Nothing,
      _ivrSubnetIds = mempty
    }

-- | A list of up to 5 EC2 VPC security group IDs to attach to the Input VPC network interfaces. Requires subnetIds. If none are specified then the VPC default security group will be used.
ivrSecurityGroupIds :: Lens' InputVPCRequest [Text]
ivrSecurityGroupIds = lens _ivrSecurityGroupIds (\s a -> s {_ivrSecurityGroupIds = a}) . _Default . _Coerce

-- | A list of 2 VPC subnet IDs from the same VPC. Subnet IDs must be mapped to two unique availability zones (AZ).
ivrSubnetIds :: Lens' InputVPCRequest [Text]
ivrSubnetIds = lens _ivrSubnetIds (\s a -> s {_ivrSubnetIds = a}) . _Coerce

instance Hashable InputVPCRequest

instance NFData InputVPCRequest

instance ToJSON InputVPCRequest where
  toJSON InputVPCRequest' {..} =
    object
      ( catMaybes
          [ ("securityGroupIds" .=) <$> _ivrSecurityGroupIds,
            Just ("subnetIds" .= _ivrSubnetIds)
          ]
      )
