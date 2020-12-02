{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PeeringConnectionOptionsRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PeeringConnectionOptionsRequest where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The VPC peering connection options.
--
--
--
-- /See:/ 'peeringConnectionOptionsRequest' smart constructor.
data PeeringConnectionOptionsRequest = PeeringConnectionOptionsRequest'
  { _pcorAllowEgressFromLocalVPCToRemoteClassicLink ::
      !(Maybe Bool),
    _pcorAllowEgressFromLocalClassicLinkToRemoteVPC ::
      !(Maybe Bool),
    _pcorAllowDNSResolutionFromRemoteVPC ::
      !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PeeringConnectionOptionsRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcorAllowEgressFromLocalVPCToRemoteClassicLink' - If true, enables outbound communication from instances in a local VPC to an EC2-Classic instance that's linked to a peer VPC using ClassicLink.
--
-- * 'pcorAllowEgressFromLocalClassicLinkToRemoteVPC' - If true, enables outbound communication from an EC2-Classic instance that's linked to a local VPC using ClassicLink to instances in a peer VPC.
--
-- * 'pcorAllowDNSResolutionFromRemoteVPC' - If true, enables a local VPC to resolve public DNS hostnames to private IP addresses when queried from instances in the peer VPC.
peeringConnectionOptionsRequest ::
  PeeringConnectionOptionsRequest
peeringConnectionOptionsRequest =
  PeeringConnectionOptionsRequest'
    { _pcorAllowEgressFromLocalVPCToRemoteClassicLink =
        Nothing,
      _pcorAllowEgressFromLocalClassicLinkToRemoteVPC = Nothing,
      _pcorAllowDNSResolutionFromRemoteVPC = Nothing
    }

-- | If true, enables outbound communication from instances in a local VPC to an EC2-Classic instance that's linked to a peer VPC using ClassicLink.
pcorAllowEgressFromLocalVPCToRemoteClassicLink :: Lens' PeeringConnectionOptionsRequest (Maybe Bool)
pcorAllowEgressFromLocalVPCToRemoteClassicLink = lens _pcorAllowEgressFromLocalVPCToRemoteClassicLink (\s a -> s {_pcorAllowEgressFromLocalVPCToRemoteClassicLink = a})

-- | If true, enables outbound communication from an EC2-Classic instance that's linked to a local VPC using ClassicLink to instances in a peer VPC.
pcorAllowEgressFromLocalClassicLinkToRemoteVPC :: Lens' PeeringConnectionOptionsRequest (Maybe Bool)
pcorAllowEgressFromLocalClassicLinkToRemoteVPC = lens _pcorAllowEgressFromLocalClassicLinkToRemoteVPC (\s a -> s {_pcorAllowEgressFromLocalClassicLinkToRemoteVPC = a})

-- | If true, enables a local VPC to resolve public DNS hostnames to private IP addresses when queried from instances in the peer VPC.
pcorAllowDNSResolutionFromRemoteVPC :: Lens' PeeringConnectionOptionsRequest (Maybe Bool)
pcorAllowDNSResolutionFromRemoteVPC = lens _pcorAllowDNSResolutionFromRemoteVPC (\s a -> s {_pcorAllowDNSResolutionFromRemoteVPC = a})

instance Hashable PeeringConnectionOptionsRequest

instance NFData PeeringConnectionOptionsRequest

instance ToQuery PeeringConnectionOptionsRequest where
  toQuery PeeringConnectionOptionsRequest' {..} =
    mconcat
      [ "AllowEgressFromLocalVpcToRemoteClassicLink"
          =: _pcorAllowEgressFromLocalVPCToRemoteClassicLink,
        "AllowEgressFromLocalClassicLinkToRemoteVpc"
          =: _pcorAllowEgressFromLocalClassicLinkToRemoteVPC,
        "AllowDnsResolutionFromRemoteVpc"
          =: _pcorAllowDNSResolutionFromRemoteVPC
      ]
