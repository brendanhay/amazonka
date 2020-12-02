{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AuthorizationRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AuthorizationRule where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ClientVPNAuthorizationRuleStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about an authorization rule.
--
--
--
-- /See:/ 'authorizationRule' smart constructor.
data AuthorizationRule = AuthorizationRule'
  { _arStatus ::
      !(Maybe ClientVPNAuthorizationRuleStatus),
    _arAccessAll :: !(Maybe Bool),
    _arClientVPNEndpointId :: !(Maybe Text),
    _arGroupId :: !(Maybe Text),
    _arDestinationCidr :: !(Maybe Text),
    _arDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AuthorizationRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arStatus' - The current state of the authorization rule.
--
-- * 'arAccessAll' - Indicates whether the authorization rule grants access to all clients.
--
-- * 'arClientVPNEndpointId' - The ID of the Client VPN endpoint with which the authorization rule is associated.
--
-- * 'arGroupId' - The ID of the Active Directory group to which the authorization rule grants access.
--
-- * 'arDestinationCidr' - The IPv4 address range, in CIDR notation, of the network to which the authorization rule applies.
--
-- * 'arDescription' - A brief description of the authorization rule.
authorizationRule ::
  AuthorizationRule
authorizationRule =
  AuthorizationRule'
    { _arStatus = Nothing,
      _arAccessAll = Nothing,
      _arClientVPNEndpointId = Nothing,
      _arGroupId = Nothing,
      _arDestinationCidr = Nothing,
      _arDescription = Nothing
    }

-- | The current state of the authorization rule.
arStatus :: Lens' AuthorizationRule (Maybe ClientVPNAuthorizationRuleStatus)
arStatus = lens _arStatus (\s a -> s {_arStatus = a})

-- | Indicates whether the authorization rule grants access to all clients.
arAccessAll :: Lens' AuthorizationRule (Maybe Bool)
arAccessAll = lens _arAccessAll (\s a -> s {_arAccessAll = a})

-- | The ID of the Client VPN endpoint with which the authorization rule is associated.
arClientVPNEndpointId :: Lens' AuthorizationRule (Maybe Text)
arClientVPNEndpointId = lens _arClientVPNEndpointId (\s a -> s {_arClientVPNEndpointId = a})

-- | The ID of the Active Directory group to which the authorization rule grants access.
arGroupId :: Lens' AuthorizationRule (Maybe Text)
arGroupId = lens _arGroupId (\s a -> s {_arGroupId = a})

-- | The IPv4 address range, in CIDR notation, of the network to which the authorization rule applies.
arDestinationCidr :: Lens' AuthorizationRule (Maybe Text)
arDestinationCidr = lens _arDestinationCidr (\s a -> s {_arDestinationCidr = a})

-- | A brief description of the authorization rule.
arDescription :: Lens' AuthorizationRule (Maybe Text)
arDescription = lens _arDescription (\s a -> s {_arDescription = a})

instance FromXML AuthorizationRule where
  parseXML x =
    AuthorizationRule'
      <$> (x .@? "status")
      <*> (x .@? "accessAll")
      <*> (x .@? "clientVpnEndpointId")
      <*> (x .@? "groupId")
      <*> (x .@? "destinationCidr")
      <*> (x .@? "description")

instance Hashable AuthorizationRule

instance NFData AuthorizationRule
