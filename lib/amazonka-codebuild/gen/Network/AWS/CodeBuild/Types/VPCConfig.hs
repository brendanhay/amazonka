{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.VPCConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.VPCConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the VPC configuration that AWS CodeBuild accesses.
--
--
--
-- /See:/ 'vpcConfig' smart constructor.
data VPCConfig = VPCConfig'
  { _vcSecurityGroupIds :: !(Maybe [Text]),
    _vcVpcId :: !(Maybe Text),
    _vcSubnets :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VPCConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vcSecurityGroupIds' - A list of one or more security groups IDs in your Amazon VPC.
--
-- * 'vcVpcId' - The ID of the Amazon VPC.
--
-- * 'vcSubnets' - A list of one or more subnet IDs in your Amazon VPC.
vpcConfig ::
  VPCConfig
vpcConfig =
  VPCConfig'
    { _vcSecurityGroupIds = Nothing,
      _vcVpcId = Nothing,
      _vcSubnets = Nothing
    }

-- | A list of one or more security groups IDs in your Amazon VPC.
vcSecurityGroupIds :: Lens' VPCConfig [Text]
vcSecurityGroupIds = lens _vcSecurityGroupIds (\s a -> s {_vcSecurityGroupIds = a}) . _Default . _Coerce

-- | The ID of the Amazon VPC.
vcVpcId :: Lens' VPCConfig (Maybe Text)
vcVpcId = lens _vcVpcId (\s a -> s {_vcVpcId = a})

-- | A list of one or more subnet IDs in your Amazon VPC.
vcSubnets :: Lens' VPCConfig [Text]
vcSubnets = lens _vcSubnets (\s a -> s {_vcSubnets = a}) . _Default . _Coerce

instance FromJSON VPCConfig where
  parseJSON =
    withObject
      "VPCConfig"
      ( \x ->
          VPCConfig'
            <$> (x .:? "securityGroupIds" .!= mempty)
            <*> (x .:? "vpcId")
            <*> (x .:? "subnets" .!= mempty)
      )

instance Hashable VPCConfig

instance NFData VPCConfig

instance ToJSON VPCConfig where
  toJSON VPCConfig' {..} =
    object
      ( catMaybes
          [ ("securityGroupIds" .=) <$> _vcSecurityGroupIds,
            ("vpcId" .=) <$> _vcVpcId,
            ("subnets" .=) <$> _vcSubnets
          ]
      )
