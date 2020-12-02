{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.VPCConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.VPCConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Configuration parameters for an optional private Virtual Private Cloud (VPC) containing the resources you are using for the job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
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
-- * 'vcSecurityGroupIds' - The ID number for a security group on an instance of your private VPC. Security groups on your VPC function serve as a virtual firewall to control inbound and outbound traffic and provides security for the resources that you’ll be accessing on the VPC. This ID number is preceded by "sg-", for instance: "sg-03b388029b0a285ea". For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_SecurityGroups.html Security Groups for your VPC> .
--
-- * 'vcSubnets' - The ID for each subnet being used in your private VPC. This subnet is a subset of the a range of IPv4 addresses used by the VPC and is specific to a given availability zone in the VPC’s region. This ID number is preceded by "subnet-", for instance: "subnet-04ccf456919e69055". For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html VPCs and Subnets> .
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

-- | The ID number for a security group on an instance of your private VPC. Security groups on your VPC function serve as a virtual firewall to control inbound and outbound traffic and provides security for the resources that you’ll be accessing on the VPC. This ID number is preceded by "sg-", for instance: "sg-03b388029b0a285ea". For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_SecurityGroups.html Security Groups for your VPC> .
vcSecurityGroupIds :: Lens' VPCConfig (NonEmpty Text)
vcSecurityGroupIds = lens _vcSecurityGroupIds (\s a -> s {_vcSecurityGroupIds = a}) . _List1

-- | The ID for each subnet being used in your private VPC. This subnet is a subset of the a range of IPv4 addresses used by the VPC and is specific to a given availability zone in the VPC’s region. This ID number is preceded by "subnet-", for instance: "subnet-04ccf456919e69055". For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html VPCs and Subnets> .
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
