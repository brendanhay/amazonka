{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.VPCConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.VPCConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies a VPC that your training jobs and hosted models have access to. Control access to and from your training and model containers by configuring the VPC. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/host-vpc.html Protect Endpoints by Using an Amazon Virtual Private Cloud> and <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud> .
--
--
--
-- /See:/ 'vpcConfig' smart constructor.
data VPCConfig = VPCConfig'
  { _vcSecurityGroupIds :: !(List1 Text),
    _vcSubnets :: !(List1 Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VPCConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vcSecurityGroupIds' - The VPC security group IDs, in the form sg-xxxxxxxx. Specify the security groups for the VPC that is specified in the @Subnets@ field.
--
-- * 'vcSubnets' - The ID of the subnets in the VPC to which you want to connect your training job or model. For information about the availability of specific instance types, see <https://docs.aws.amazon.com/sagemaker/latest/dg/instance-types-az.html Supported Instance Types and Availability Zones> .
vpcConfig ::
  -- | 'vcSecurityGroupIds'
  NonEmpty Text ->
  -- | 'vcSubnets'
  NonEmpty Text ->
  VPCConfig
vpcConfig pSecurityGroupIds_ pSubnets_ =
  VPCConfig'
    { _vcSecurityGroupIds = _List1 # pSecurityGroupIds_,
      _vcSubnets = _List1 # pSubnets_
    }

-- | The VPC security group IDs, in the form sg-xxxxxxxx. Specify the security groups for the VPC that is specified in the @Subnets@ field.
vcSecurityGroupIds :: Lens' VPCConfig (NonEmpty Text)
vcSecurityGroupIds = lens _vcSecurityGroupIds (\s a -> s {_vcSecurityGroupIds = a}) . _List1

-- | The ID of the subnets in the VPC to which you want to connect your training job or model. For information about the availability of specific instance types, see <https://docs.aws.amazon.com/sagemaker/latest/dg/instance-types-az.html Supported Instance Types and Availability Zones> .
vcSubnets :: Lens' VPCConfig (NonEmpty Text)
vcSubnets = lens _vcSubnets (\s a -> s {_vcSubnets = a}) . _List1

instance FromJSON VPCConfig where
  parseJSON =
    withObject
      "VPCConfig"
      ( \x ->
          VPCConfig' <$> (x .: "SecurityGroupIds") <*> (x .: "Subnets")
      )

instance Hashable VPCConfig

instance NFData VPCConfig

instance ToJSON VPCConfig where
  toJSON VPCConfig' {..} =
    object
      ( catMaybes
          [ Just ("SecurityGroupIds" .= _vcSecurityGroupIds),
            Just ("Subnets" .= _vcSubnets)
          ]
      )
