{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.VPCConfigResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.VPCConfigResponse where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The VPC security groups and subnets that are attached to a Lambda function.
--
--
--
-- /See:/ 'vpcConfigResponse' smart constructor.
data VPCConfigResponse = VPCConfigResponse'
  { _vcSecurityGroupIds ::
      !(Maybe [Text]),
    _vcSubnetIds :: !(Maybe [Text]),
    _vcVPCId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VPCConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vcSecurityGroupIds' - A list of VPC security groups IDs.
--
-- * 'vcSubnetIds' - A list of VPC subnet IDs.
--
-- * 'vcVPCId' - The ID of the VPC.
vpcConfigResponse ::
  VPCConfigResponse
vpcConfigResponse =
  VPCConfigResponse'
    { _vcSecurityGroupIds = Nothing,
      _vcSubnetIds = Nothing,
      _vcVPCId = Nothing
    }

-- | A list of VPC security groups IDs.
vcSecurityGroupIds :: Lens' VPCConfigResponse [Text]
vcSecurityGroupIds = lens _vcSecurityGroupIds (\s a -> s {_vcSecurityGroupIds = a}) . _Default . _Coerce

-- | A list of VPC subnet IDs.
vcSubnetIds :: Lens' VPCConfigResponse [Text]
vcSubnetIds = lens _vcSubnetIds (\s a -> s {_vcSubnetIds = a}) . _Default . _Coerce

-- | The ID of the VPC.
vcVPCId :: Lens' VPCConfigResponse (Maybe Text)
vcVPCId = lens _vcVPCId (\s a -> s {_vcVPCId = a})

instance FromJSON VPCConfigResponse where
  parseJSON =
    withObject
      "VPCConfigResponse"
      ( \x ->
          VPCConfigResponse'
            <$> (x .:? "SecurityGroupIds" .!= mempty)
            <*> (x .:? "SubnetIds" .!= mempty)
            <*> (x .:? "VpcId")
      )

instance Hashable VPCConfigResponse

instance NFData VPCConfigResponse
