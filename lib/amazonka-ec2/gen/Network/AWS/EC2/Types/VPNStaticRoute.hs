{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPNStaticRoute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VPNStaticRoute where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.VPNState
import Network.AWS.EC2.Types.VPNStaticRouteSource
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a static route for a VPN connection.
--
--
--
-- /See:/ 'vpnStaticRoute' smart constructor.
data VPNStaticRoute = VPNStaticRoute'
  { _vsrState ::
      !(Maybe VPNState),
    _vsrSource :: !(Maybe VPNStaticRouteSource),
    _vsrDestinationCidrBlock :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VPNStaticRoute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vsrState' - The current state of the static route.
--
-- * 'vsrSource' - Indicates how the routes were provided.
--
-- * 'vsrDestinationCidrBlock' - The CIDR block associated with the local subnet of the customer data center.
vpnStaticRoute ::
  VPNStaticRoute
vpnStaticRoute =
  VPNStaticRoute'
    { _vsrState = Nothing,
      _vsrSource = Nothing,
      _vsrDestinationCidrBlock = Nothing
    }

-- | The current state of the static route.
vsrState :: Lens' VPNStaticRoute (Maybe VPNState)
vsrState = lens _vsrState (\s a -> s {_vsrState = a})

-- | Indicates how the routes were provided.
vsrSource :: Lens' VPNStaticRoute (Maybe VPNStaticRouteSource)
vsrSource = lens _vsrSource (\s a -> s {_vsrSource = a})

-- | The CIDR block associated with the local subnet of the customer data center.
vsrDestinationCidrBlock :: Lens' VPNStaticRoute (Maybe Text)
vsrDestinationCidrBlock = lens _vsrDestinationCidrBlock (\s a -> s {_vsrDestinationCidrBlock = a})

instance FromXML VPNStaticRoute where
  parseXML x =
    VPNStaticRoute'
      <$> (x .@? "state")
      <*> (x .@? "source")
      <*> (x .@? "destinationCidrBlock")

instance Hashable VPNStaticRoute

instance NFData VPNStaticRoute
