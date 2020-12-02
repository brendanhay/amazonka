{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.VPCOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.VPCOptions where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Options to specify the subnets and security groups for VPC endpoint. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html VPC Endpoints for Amazon Elasticsearch Service Domains> .
--
--
--
-- /See:/ 'vpcOptions' smart constructor.
data VPCOptions = VPCOptions'
  { _voSecurityGroupIds ::
      !(Maybe [Text]),
    _voSubnetIds :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VPCOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'voSecurityGroupIds' - Specifies the security groups for VPC endpoint.
--
-- * 'voSubnetIds' - Specifies the subnets for VPC endpoint.
vpcOptions ::
  VPCOptions
vpcOptions =
  VPCOptions'
    { _voSecurityGroupIds = Nothing,
      _voSubnetIds = Nothing
    }

-- | Specifies the security groups for VPC endpoint.
voSecurityGroupIds :: Lens' VPCOptions [Text]
voSecurityGroupIds = lens _voSecurityGroupIds (\s a -> s {_voSecurityGroupIds = a}) . _Default . _Coerce

-- | Specifies the subnets for VPC endpoint.
voSubnetIds :: Lens' VPCOptions [Text]
voSubnetIds = lens _voSubnetIds (\s a -> s {_voSubnetIds = a}) . _Default . _Coerce

instance Hashable VPCOptions

instance NFData VPCOptions

instance ToJSON VPCOptions where
  toJSON VPCOptions' {..} =
    object
      ( catMaybes
          [ ("SecurityGroupIds" .=) <$> _voSecurityGroupIds,
            ("SubnetIds" .=) <$> _voSubnetIds
          ]
      )
