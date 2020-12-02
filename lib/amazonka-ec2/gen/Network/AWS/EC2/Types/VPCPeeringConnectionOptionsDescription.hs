{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPCPeeringConnectionOptionsDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VPCPeeringConnectionOptionsDescription where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the VPC peering connection options.
--
--
--
-- /See:/ 'vpcPeeringConnectionOptionsDescription' smart constructor.
data VPCPeeringConnectionOptionsDescription = VPCPeeringConnectionOptionsDescription'
  { _vpcodAllowEgressFromLocalVPCToRemoteClassicLink ::
      !(Maybe Bool),
    _vpcodAllowEgressFromLocalClassicLinkToRemoteVPC ::
      !(Maybe Bool),
    _vpcodAllowDNSResolutionFromRemoteVPC ::
      !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VPCPeeringConnectionOptionsDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vpcodAllowEgressFromLocalVPCToRemoteClassicLink' - Indicates whether a local VPC can communicate with a ClassicLink connection in the peer VPC over the VPC peering connection.
--
-- * 'vpcodAllowEgressFromLocalClassicLinkToRemoteVPC' - Indicates whether a local ClassicLink connection can communicate with the peer VPC over the VPC peering connection.
--
-- * 'vpcodAllowDNSResolutionFromRemoteVPC' - Indicates whether a local VPC can resolve public DNS hostnames to private IP addresses when queried from instances in a peer VPC.
vpcPeeringConnectionOptionsDescription ::
  VPCPeeringConnectionOptionsDescription
vpcPeeringConnectionOptionsDescription =
  VPCPeeringConnectionOptionsDescription'
    { _vpcodAllowEgressFromLocalVPCToRemoteClassicLink =
        Nothing,
      _vpcodAllowEgressFromLocalClassicLinkToRemoteVPC =
        Nothing,
      _vpcodAllowDNSResolutionFromRemoteVPC = Nothing
    }

-- | Indicates whether a local VPC can communicate with a ClassicLink connection in the peer VPC over the VPC peering connection.
vpcodAllowEgressFromLocalVPCToRemoteClassicLink :: Lens' VPCPeeringConnectionOptionsDescription (Maybe Bool)
vpcodAllowEgressFromLocalVPCToRemoteClassicLink = lens _vpcodAllowEgressFromLocalVPCToRemoteClassicLink (\s a -> s {_vpcodAllowEgressFromLocalVPCToRemoteClassicLink = a})

-- | Indicates whether a local ClassicLink connection can communicate with the peer VPC over the VPC peering connection.
vpcodAllowEgressFromLocalClassicLinkToRemoteVPC :: Lens' VPCPeeringConnectionOptionsDescription (Maybe Bool)
vpcodAllowEgressFromLocalClassicLinkToRemoteVPC = lens _vpcodAllowEgressFromLocalClassicLinkToRemoteVPC (\s a -> s {_vpcodAllowEgressFromLocalClassicLinkToRemoteVPC = a})

-- | Indicates whether a local VPC can resolve public DNS hostnames to private IP addresses when queried from instances in a peer VPC.
vpcodAllowDNSResolutionFromRemoteVPC :: Lens' VPCPeeringConnectionOptionsDescription (Maybe Bool)
vpcodAllowDNSResolutionFromRemoteVPC = lens _vpcodAllowDNSResolutionFromRemoteVPC (\s a -> s {_vpcodAllowDNSResolutionFromRemoteVPC = a})

instance FromXML VPCPeeringConnectionOptionsDescription where
  parseXML x =
    VPCPeeringConnectionOptionsDescription'
      <$> (x .@? "allowEgressFromLocalVpcToRemoteClassicLink")
      <*> (x .@? "allowEgressFromLocalClassicLinkToRemoteVpc")
      <*> (x .@? "allowDnsResolutionFromRemoteVpc")

instance Hashable VPCPeeringConnectionOptionsDescription

instance NFData VPCPeeringConnectionOptionsDescription
