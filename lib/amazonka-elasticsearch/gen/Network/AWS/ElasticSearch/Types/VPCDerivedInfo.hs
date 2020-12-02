{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.VPCDerivedInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.VPCDerivedInfo where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Options to specify the subnets and security groups for VPC endpoint. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html VPC Endpoints for Amazon Elasticsearch Service Domains> .
--
--
--
-- /See:/ 'vpcDerivedInfo' smart constructor.
data VPCDerivedInfo = VPCDerivedInfo'
  { _vdiSecurityGroupIds ::
      !(Maybe [Text]),
    _vdiSubnetIds :: !(Maybe [Text]),
    _vdiVPCId :: !(Maybe Text),
    _vdiAvailabilityZones :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VPCDerivedInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vdiSecurityGroupIds' - Specifies the security groups for VPC endpoint.
--
-- * 'vdiSubnetIds' - Specifies the subnets for VPC endpoint.
--
-- * 'vdiVPCId' - The VPC Id for the Elasticsearch domain. Exists only if the domain was created with VPCOptions.
--
-- * 'vdiAvailabilityZones' - The availability zones for the Elasticsearch domain. Exists only if the domain was created with VPCOptions.
vpcDerivedInfo ::
  VPCDerivedInfo
vpcDerivedInfo =
  VPCDerivedInfo'
    { _vdiSecurityGroupIds = Nothing,
      _vdiSubnetIds = Nothing,
      _vdiVPCId = Nothing,
      _vdiAvailabilityZones = Nothing
    }

-- | Specifies the security groups for VPC endpoint.
vdiSecurityGroupIds :: Lens' VPCDerivedInfo [Text]
vdiSecurityGroupIds = lens _vdiSecurityGroupIds (\s a -> s {_vdiSecurityGroupIds = a}) . _Default . _Coerce

-- | Specifies the subnets for VPC endpoint.
vdiSubnetIds :: Lens' VPCDerivedInfo [Text]
vdiSubnetIds = lens _vdiSubnetIds (\s a -> s {_vdiSubnetIds = a}) . _Default . _Coerce

-- | The VPC Id for the Elasticsearch domain. Exists only if the domain was created with VPCOptions.
vdiVPCId :: Lens' VPCDerivedInfo (Maybe Text)
vdiVPCId = lens _vdiVPCId (\s a -> s {_vdiVPCId = a})

-- | The availability zones for the Elasticsearch domain. Exists only if the domain was created with VPCOptions.
vdiAvailabilityZones :: Lens' VPCDerivedInfo [Text]
vdiAvailabilityZones = lens _vdiAvailabilityZones (\s a -> s {_vdiAvailabilityZones = a}) . _Default . _Coerce

instance FromJSON VPCDerivedInfo where
  parseJSON =
    withObject
      "VPCDerivedInfo"
      ( \x ->
          VPCDerivedInfo'
            <$> (x .:? "SecurityGroupIds" .!= mempty)
            <*> (x .:? "SubnetIds" .!= mempty)
            <*> (x .:? "VPCId")
            <*> (x .:? "AvailabilityZones" .!= mempty)
      )

instance Hashable VPCDerivedInfo

instance NFData VPCDerivedInfo
