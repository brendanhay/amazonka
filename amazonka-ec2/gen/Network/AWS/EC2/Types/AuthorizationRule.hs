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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ClientVpnAuthorizationRuleStatus
import qualified Network.AWS.Lens as Lens

-- | Information about an authorization rule.
--
-- /See:/ 'newAuthorizationRule' smart constructor.
data AuthorizationRule = AuthorizationRule'
  { -- | The ID of the Client VPN endpoint with which the authorization rule is
    -- associated.
    clientVpnEndpointId :: Core.Maybe Core.Text,
    -- | The current state of the authorization rule.
    status :: Core.Maybe ClientVpnAuthorizationRuleStatus,
    -- | The IPv4 address range, in CIDR notation, of the network to which the
    -- authorization rule applies.
    destinationCidr :: Core.Maybe Core.Text,
    -- | Indicates whether the authorization rule grants access to all clients.
    accessAll :: Core.Maybe Core.Bool,
    -- | The ID of the Active Directory group to which the authorization rule
    -- grants access.
    groupId :: Core.Maybe Core.Text,
    -- | A brief description of the authorization rule.
    description :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      status = Core.Nothing,
      destinationCidr = Core.Nothing,
      accessAll = Core.Nothing,
      groupId = Core.Nothing,
      description = Core.Nothing
    }

-- | The ID of the Client VPN endpoint with which the authorization rule is
-- associated.
authorizationRule_clientVpnEndpointId :: Lens.Lens' AuthorizationRule (Core.Maybe Core.Text)
authorizationRule_clientVpnEndpointId = Lens.lens (\AuthorizationRule' {clientVpnEndpointId} -> clientVpnEndpointId) (\s@AuthorizationRule' {} a -> s {clientVpnEndpointId = a} :: AuthorizationRule)

-- | The current state of the authorization rule.
authorizationRule_status :: Lens.Lens' AuthorizationRule (Core.Maybe ClientVpnAuthorizationRuleStatus)
authorizationRule_status = Lens.lens (\AuthorizationRule' {status} -> status) (\s@AuthorizationRule' {} a -> s {status = a} :: AuthorizationRule)

-- | The IPv4 address range, in CIDR notation, of the network to which the
-- authorization rule applies.
authorizationRule_destinationCidr :: Lens.Lens' AuthorizationRule (Core.Maybe Core.Text)
authorizationRule_destinationCidr = Lens.lens (\AuthorizationRule' {destinationCidr} -> destinationCidr) (\s@AuthorizationRule' {} a -> s {destinationCidr = a} :: AuthorizationRule)

-- | Indicates whether the authorization rule grants access to all clients.
authorizationRule_accessAll :: Lens.Lens' AuthorizationRule (Core.Maybe Core.Bool)
authorizationRule_accessAll = Lens.lens (\AuthorizationRule' {accessAll} -> accessAll) (\s@AuthorizationRule' {} a -> s {accessAll = a} :: AuthorizationRule)

-- | The ID of the Active Directory group to which the authorization rule
-- grants access.
authorizationRule_groupId :: Lens.Lens' AuthorizationRule (Core.Maybe Core.Text)
authorizationRule_groupId = Lens.lens (\AuthorizationRule' {groupId} -> groupId) (\s@AuthorizationRule' {} a -> s {groupId = a} :: AuthorizationRule)

-- | A brief description of the authorization rule.
authorizationRule_description :: Lens.Lens' AuthorizationRule (Core.Maybe Core.Text)
authorizationRule_description = Lens.lens (\AuthorizationRule' {description} -> description) (\s@AuthorizationRule' {} a -> s {description = a} :: AuthorizationRule)

instance Core.FromXML AuthorizationRule where
  parseXML x =
    AuthorizationRule'
      Core.<$> (x Core..@? "clientVpnEndpointId")
      Core.<*> (x Core..@? "status")
      Core.<*> (x Core..@? "destinationCidr")
      Core.<*> (x Core..@? "accessAll")
      Core.<*> (x Core..@? "groupId")
      Core.<*> (x Core..@? "description")

instance Core.Hashable AuthorizationRule

instance Core.NFData AuthorizationRule
