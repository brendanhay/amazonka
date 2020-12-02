{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.VPCConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.VPCConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes VPC configuration information for fleets and image builders.
--
--
--
-- /See:/ 'vpcConfig' smart constructor.
data VPCConfig = VPCConfig'
  { _vcSecurityGroupIds :: !(Maybe [Text]),
    _vcSubnetIds :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VPCConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vcSecurityGroupIds' - The identifiers of the security groups for the fleet or image builder.
--
-- * 'vcSubnetIds' - The identifiers of the subnets to which a network interface is attached from the fleet instance or image builder instance. Fleet instances use one or more subnets. Image builder instances use one subnet.
vpcConfig ::
  VPCConfig
vpcConfig =
  VPCConfig' {_vcSecurityGroupIds = Nothing, _vcSubnetIds = Nothing}

-- | The identifiers of the security groups for the fleet or image builder.
vcSecurityGroupIds :: Lens' VPCConfig [Text]
vcSecurityGroupIds = lens _vcSecurityGroupIds (\s a -> s {_vcSecurityGroupIds = a}) . _Default . _Coerce

-- | The identifiers of the subnets to which a network interface is attached from the fleet instance or image builder instance. Fleet instances use one or more subnets. Image builder instances use one subnet.
vcSubnetIds :: Lens' VPCConfig [Text]
vcSubnetIds = lens _vcSubnetIds (\s a -> s {_vcSubnetIds = a}) . _Default . _Coerce

instance FromJSON VPCConfig where
  parseJSON =
    withObject
      "VPCConfig"
      ( \x ->
          VPCConfig'
            <$> (x .:? "SecurityGroupIds" .!= mempty)
            <*> (x .:? "SubnetIds" .!= mempty)
      )

instance Hashable VPCConfig

instance NFData VPCConfig

instance ToJSON VPCConfig where
  toJSON VPCConfig' {..} =
    object
      ( catMaybes
          [ ("SecurityGroupIds" .=) <$> _vcSecurityGroupIds,
            ("SubnetIds" .=) <$> _vcSubnetIds
          ]
      )
