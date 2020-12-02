{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.VPCPeeringAuthorization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.VPCPeeringAuthorization where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents an authorization for a VPC peering connection between the VPC for an Amazon GameLift fleet and another VPC on an account you have access to. This authorization must exist and be valid for the peering connection to be established. Authorizations are valid for 24 hours after they are issued.
--
--
--     * 'CreateVpcPeeringAuthorization'
--
--     * 'DescribeVpcPeeringAuthorizations'
--
--     * 'DeleteVpcPeeringAuthorization'
--
--     * 'CreateVpcPeeringConnection'
--
--     * 'DescribeVpcPeeringConnections'
--
--     * 'DeleteVpcPeeringConnection'
--
--
--
--
-- /See:/ 'vpcPeeringAuthorization' smart constructor.
data VPCPeeringAuthorization = VPCPeeringAuthorization'
  { _vpaCreationTime ::
      !(Maybe POSIX),
    _vpaPeerVPCId :: !(Maybe Text),
    _vpaPeerVPCAWSAccountId :: !(Maybe Text),
    _vpaGameLiftAWSAccountId :: !(Maybe Text),
    _vpaExpirationTime :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VPCPeeringAuthorization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vpaCreationTime' - Time stamp indicating when this authorization was issued. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- * 'vpaPeerVPCId' - A unique identifier for a VPC with resources to be accessed by your Amazon GameLift fleet. The VPC must be in the same Region where your fleet is deployed. Look up a VPC ID using the <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS Management Console. Learn more about VPC peering in <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets> .
--
-- * 'vpaPeerVPCAWSAccountId' -
--
-- * 'vpaGameLiftAWSAccountId' - A unique identifier for the AWS account that you use to manage your Amazon GameLift fleet. You can find your Account ID in the AWS Management Console under account settings.
--
-- * 'vpaExpirationTime' - Time stamp indicating when this authorization expires (24 hours after issuance). Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
vpcPeeringAuthorization ::
  VPCPeeringAuthorization
vpcPeeringAuthorization =
  VPCPeeringAuthorization'
    { _vpaCreationTime = Nothing,
      _vpaPeerVPCId = Nothing,
      _vpaPeerVPCAWSAccountId = Nothing,
      _vpaGameLiftAWSAccountId = Nothing,
      _vpaExpirationTime = Nothing
    }

-- | Time stamp indicating when this authorization was issued. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
vpaCreationTime :: Lens' VPCPeeringAuthorization (Maybe UTCTime)
vpaCreationTime = lens _vpaCreationTime (\s a -> s {_vpaCreationTime = a}) . mapping _Time

-- | A unique identifier for a VPC with resources to be accessed by your Amazon GameLift fleet. The VPC must be in the same Region where your fleet is deployed. Look up a VPC ID using the <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS Management Console. Learn more about VPC peering in <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets> .
vpaPeerVPCId :: Lens' VPCPeeringAuthorization (Maybe Text)
vpaPeerVPCId = lens _vpaPeerVPCId (\s a -> s {_vpaPeerVPCId = a})

-- |
vpaPeerVPCAWSAccountId :: Lens' VPCPeeringAuthorization (Maybe Text)
vpaPeerVPCAWSAccountId = lens _vpaPeerVPCAWSAccountId (\s a -> s {_vpaPeerVPCAWSAccountId = a})

-- | A unique identifier for the AWS account that you use to manage your Amazon GameLift fleet. You can find your Account ID in the AWS Management Console under account settings.
vpaGameLiftAWSAccountId :: Lens' VPCPeeringAuthorization (Maybe Text)
vpaGameLiftAWSAccountId = lens _vpaGameLiftAWSAccountId (\s a -> s {_vpaGameLiftAWSAccountId = a})

-- | Time stamp indicating when this authorization expires (24 hours after issuance). Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
vpaExpirationTime :: Lens' VPCPeeringAuthorization (Maybe UTCTime)
vpaExpirationTime = lens _vpaExpirationTime (\s a -> s {_vpaExpirationTime = a}) . mapping _Time

instance FromJSON VPCPeeringAuthorization where
  parseJSON =
    withObject
      "VPCPeeringAuthorization"
      ( \x ->
          VPCPeeringAuthorization'
            <$> (x .:? "CreationTime")
            <*> (x .:? "PeerVpcId")
            <*> (x .:? "PeerVpcAwsAccountId")
            <*> (x .:? "GameLiftAwsAccountId")
            <*> (x .:? "ExpirationTime")
      )

instance Hashable VPCPeeringAuthorization

instance NFData VPCPeeringAuthorization
