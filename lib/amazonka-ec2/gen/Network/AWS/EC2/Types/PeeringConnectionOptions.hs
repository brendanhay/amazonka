{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PeeringConnectionOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PeeringConnectionOptions where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the VPC peering connection options.
--
--
--
-- /See:/ 'peeringConnectionOptions' smart constructor.
data PeeringConnectionOptions = PeeringConnectionOptions'
  { _pcoAllowEgressFromLocalVPCToRemoteClassicLink ::
      !(Maybe Bool),
    _pcoAllowEgressFromLocalClassicLinkToRemoteVPC ::
      !(Maybe Bool),
    _pcoAllowDNSResolutionFromRemoteVPC ::
      !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PeeringConnectionOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcoAllowEgressFromLocalVPCToRemoteClassicLink' - If true, enables outbound communication from instances in a local VPC to an EC2-Classic instance that's linked to a peer VPC using ClassicLink.
--
-- * 'pcoAllowEgressFromLocalClassicLinkToRemoteVPC' - If true, enables outbound communication from an EC2-Classic instance that's linked to a local VPC using ClassicLink to instances in a peer VPC.
--
-- * 'pcoAllowDNSResolutionFromRemoteVPC' - If true, the public DNS hostnames of instances in the specified VPC resolve to private IP addresses when queried from instances in the peer VPC.
peeringConnectionOptions ::
  PeeringConnectionOptions
peeringConnectionOptions =
  PeeringConnectionOptions'
    { _pcoAllowEgressFromLocalVPCToRemoteClassicLink =
        Nothing,
      _pcoAllowEgressFromLocalClassicLinkToRemoteVPC = Nothing,
      _pcoAllowDNSResolutionFromRemoteVPC = Nothing
    }

-- | If true, enables outbound communication from instances in a local VPC to an EC2-Classic instance that's linked to a peer VPC using ClassicLink.
pcoAllowEgressFromLocalVPCToRemoteClassicLink :: Lens' PeeringConnectionOptions (Maybe Bool)
pcoAllowEgressFromLocalVPCToRemoteClassicLink = lens _pcoAllowEgressFromLocalVPCToRemoteClassicLink (\s a -> s {_pcoAllowEgressFromLocalVPCToRemoteClassicLink = a})

-- | If true, enables outbound communication from an EC2-Classic instance that's linked to a local VPC using ClassicLink to instances in a peer VPC.
pcoAllowEgressFromLocalClassicLinkToRemoteVPC :: Lens' PeeringConnectionOptions (Maybe Bool)
pcoAllowEgressFromLocalClassicLinkToRemoteVPC = lens _pcoAllowEgressFromLocalClassicLinkToRemoteVPC (\s a -> s {_pcoAllowEgressFromLocalClassicLinkToRemoteVPC = a})

-- | If true, the public DNS hostnames of instances in the specified VPC resolve to private IP addresses when queried from instances in the peer VPC.
pcoAllowDNSResolutionFromRemoteVPC :: Lens' PeeringConnectionOptions (Maybe Bool)
pcoAllowDNSResolutionFromRemoteVPC = lens _pcoAllowDNSResolutionFromRemoteVPC (\s a -> s {_pcoAllowDNSResolutionFromRemoteVPC = a})

instance FromXML PeeringConnectionOptions where
  parseXML x =
    PeeringConnectionOptions'
      <$> (x .@? "allowEgressFromLocalVpcToRemoteClassicLink")
      <*> (x .@? "allowEgressFromLocalClassicLinkToRemoteVpc")
      <*> (x .@? "allowDnsResolutionFromRemoteVpc")

instance Hashable PeeringConnectionOptions

instance NFData PeeringConnectionOptions
