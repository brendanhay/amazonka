{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AuthorizationRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AuthorizationRule where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ClientVpnAuthorizationRuleStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about an authorization rule.
--
-- /See:/ 'newAuthorizationRule' smart constructor.
data AuthorizationRule = AuthorizationRule'
  { -- | The ID of the Client VPN endpoint with which the authorization rule is
    -- associated.
    clientVpnEndpointId :: Prelude.Maybe Prelude.Text,
    -- | The current state of the authorization rule.
    status :: Prelude.Maybe ClientVpnAuthorizationRuleStatus,
    -- | The IPv4 address range, in CIDR notation, of the network to which the
    -- authorization rule applies.
    destinationCidr :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the authorization rule grants access to all clients.
    accessAll :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the Active Directory group to which the authorization rule
    -- grants access.
    groupId :: Prelude.Maybe Prelude.Text,
    -- | A brief description of the authorization rule.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AuthorizationRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientVpnEndpointId', 'authorizationRule_clientVpnEndpointId' - The ID of the Client VPN endpoint with which the authorization rule is
-- associated.
--
-- 'status', 'authorizationRule_status' - The current state of the authorization rule.
--
-- 'destinationCidr', 'authorizationRule_destinationCidr' - The IPv4 address range, in CIDR notation, of the network to which the
-- authorization rule applies.
--
-- 'accessAll', 'authorizationRule_accessAll' - Indicates whether the authorization rule grants access to all clients.
--
-- 'groupId', 'authorizationRule_groupId' - The ID of the Active Directory group to which the authorization rule
-- grants access.
--
-- 'description', 'authorizationRule_description' - A brief description of the authorization rule.
newAuthorizationRule ::
  AuthorizationRule
newAuthorizationRule =
  AuthorizationRule'
    { clientVpnEndpointId =
        Prelude.Nothing,
      status = Prelude.Nothing,
      destinationCidr = Prelude.Nothing,
      accessAll = Prelude.Nothing,
      groupId = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The ID of the Client VPN endpoint with which the authorization rule is
-- associated.
authorizationRule_clientVpnEndpointId :: Lens.Lens' AuthorizationRule (Prelude.Maybe Prelude.Text)
authorizationRule_clientVpnEndpointId = Lens.lens (\AuthorizationRule' {clientVpnEndpointId} -> clientVpnEndpointId) (\s@AuthorizationRule' {} a -> s {clientVpnEndpointId = a} :: AuthorizationRule)

-- | The current state of the authorization rule.
authorizationRule_status :: Lens.Lens' AuthorizationRule (Prelude.Maybe ClientVpnAuthorizationRuleStatus)
authorizationRule_status = Lens.lens (\AuthorizationRule' {status} -> status) (\s@AuthorizationRule' {} a -> s {status = a} :: AuthorizationRule)

-- | The IPv4 address range, in CIDR notation, of the network to which the
-- authorization rule applies.
authorizationRule_destinationCidr :: Lens.Lens' AuthorizationRule (Prelude.Maybe Prelude.Text)
authorizationRule_destinationCidr = Lens.lens (\AuthorizationRule' {destinationCidr} -> destinationCidr) (\s@AuthorizationRule' {} a -> s {destinationCidr = a} :: AuthorizationRule)

-- | Indicates whether the authorization rule grants access to all clients.
authorizationRule_accessAll :: Lens.Lens' AuthorizationRule (Prelude.Maybe Prelude.Bool)
authorizationRule_accessAll = Lens.lens (\AuthorizationRule' {accessAll} -> accessAll) (\s@AuthorizationRule' {} a -> s {accessAll = a} :: AuthorizationRule)

-- | The ID of the Active Directory group to which the authorization rule
-- grants access.
authorizationRule_groupId :: Lens.Lens' AuthorizationRule (Prelude.Maybe Prelude.Text)
authorizationRule_groupId = Lens.lens (\AuthorizationRule' {groupId} -> groupId) (\s@AuthorizationRule' {} a -> s {groupId = a} :: AuthorizationRule)

-- | A brief description of the authorization rule.
authorizationRule_description :: Lens.Lens' AuthorizationRule (Prelude.Maybe Prelude.Text)
authorizationRule_description = Lens.lens (\AuthorizationRule' {description} -> description) (\s@AuthorizationRule' {} a -> s {description = a} :: AuthorizationRule)

instance Prelude.FromXML AuthorizationRule where
  parseXML x =
    AuthorizationRule'
      Prelude.<$> (x Prelude..@? "clientVpnEndpointId")
      Prelude.<*> (x Prelude..@? "status")
      Prelude.<*> (x Prelude..@? "destinationCidr")
      Prelude.<*> (x Prelude..@? "accessAll")
      Prelude.<*> (x Prelude..@? "groupId")
      Prelude.<*> (x Prelude..@? "description")

instance Prelude.Hashable AuthorizationRule

instance Prelude.NFData AuthorizationRule
