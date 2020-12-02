{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.VPCConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.VPCConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The VPC security groups and subnets that are attached to a Lambda function. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/configuration-vpc.html VPC Settings> .
--
--
--
-- /See:/ 'vpcConfig' smart constructor.
data VPCConfig = VPCConfig'
  { _vpccSecurityGroupIds ::
      !(Maybe [Text]),
    _vpccSubnetIds :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VPCConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vpccSecurityGroupIds' - A list of VPC security groups IDs.
--
-- * 'vpccSubnetIds' - A list of VPC subnet IDs.
vpcConfig ::
  VPCConfig
vpcConfig =
  VPCConfig'
    { _vpccSecurityGroupIds = Nothing,
      _vpccSubnetIds = Nothing
    }

-- | A list of VPC security groups IDs.
vpccSecurityGroupIds :: Lens' VPCConfig [Text]
vpccSecurityGroupIds = lens _vpccSecurityGroupIds (\s a -> s {_vpccSecurityGroupIds = a}) . _Default . _Coerce

-- | A list of VPC subnet IDs.
vpccSubnetIds :: Lens' VPCConfig [Text]
vpccSubnetIds = lens _vpccSubnetIds (\s a -> s {_vpccSubnetIds = a}) . _Default . _Coerce

instance Hashable VPCConfig

instance NFData VPCConfig

instance ToJSON VPCConfig where
  toJSON VPCConfig' {..} =
    object
      ( catMaybes
          [ ("SecurityGroupIds" .=) <$> _vpccSecurityGroupIds,
            ("SubnetIds" .=) <$> _vpccSubnetIds
          ]
      )
