{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AuthorizationRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AuthorizationRule
  ( AuthorizationRule (..),

    -- * Smart constructor
    mkAuthorizationRule,

    -- * Lenses
    arAccessAll,
    arClientVpnEndpointId,
    arDescription,
    arDestinationCidr,
    arGroupId,
    arStatus,
  )
where

import qualified Network.AWS.EC2.Types.ClientVpnAuthorizationRuleStatus as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about an authorization rule.
--
-- /See:/ 'mkAuthorizationRule' smart constructor.
data AuthorizationRule = AuthorizationRule'
  { -- | Indicates whether the authorization rule grants access to all clients.
    accessAll :: Core.Maybe Core.Bool,
    -- | The ID of the Client VPN endpoint with which the authorization rule is associated.
    clientVpnEndpointId :: Core.Maybe Types.String,
    -- | A brief description of the authorization rule.
    description :: Core.Maybe Types.String,
    -- | The IPv4 address range, in CIDR notation, of the network to which the authorization rule applies.
    destinationCidr :: Core.Maybe Types.String,
    -- | The ID of the Active Directory group to which the authorization rule grants access.
    groupId :: Core.Maybe Types.String,
    -- | The current state of the authorization rule.
    status :: Core.Maybe Types.ClientVpnAuthorizationRuleStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AuthorizationRule' value with any optional fields omitted.
mkAuthorizationRule ::
  AuthorizationRule
mkAuthorizationRule =
  AuthorizationRule'
    { accessAll = Core.Nothing,
      clientVpnEndpointId = Core.Nothing,
      description = Core.Nothing,
      destinationCidr = Core.Nothing,
      groupId = Core.Nothing,
      status = Core.Nothing
    }

-- | Indicates whether the authorization rule grants access to all clients.
--
-- /Note:/ Consider using 'accessAll' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arAccessAll :: Lens.Lens' AuthorizationRule (Core.Maybe Core.Bool)
arAccessAll = Lens.field @"accessAll"
{-# DEPRECATED arAccessAll "Use generic-lens or generic-optics with 'accessAll' instead." #-}

-- | The ID of the Client VPN endpoint with which the authorization rule is associated.
--
-- /Note:/ Consider using 'clientVpnEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arClientVpnEndpointId :: Lens.Lens' AuthorizationRule (Core.Maybe Types.String)
arClientVpnEndpointId = Lens.field @"clientVpnEndpointId"
{-# DEPRECATED arClientVpnEndpointId "Use generic-lens or generic-optics with 'clientVpnEndpointId' instead." #-}

-- | A brief description of the authorization rule.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arDescription :: Lens.Lens' AuthorizationRule (Core.Maybe Types.String)
arDescription = Lens.field @"description"
{-# DEPRECATED arDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The IPv4 address range, in CIDR notation, of the network to which the authorization rule applies.
--
-- /Note:/ Consider using 'destinationCidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arDestinationCidr :: Lens.Lens' AuthorizationRule (Core.Maybe Types.String)
arDestinationCidr = Lens.field @"destinationCidr"
{-# DEPRECATED arDestinationCidr "Use generic-lens or generic-optics with 'destinationCidr' instead." #-}

-- | The ID of the Active Directory group to which the authorization rule grants access.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arGroupId :: Lens.Lens' AuthorizationRule (Core.Maybe Types.String)
arGroupId = Lens.field @"groupId"
{-# DEPRECATED arGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | The current state of the authorization rule.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arStatus :: Lens.Lens' AuthorizationRule (Core.Maybe Types.ClientVpnAuthorizationRuleStatus)
arStatus = Lens.field @"status"
{-# DEPRECATED arStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromXML AuthorizationRule where
  parseXML x =
    AuthorizationRule'
      Core.<$> (x Core..@? "accessAll")
      Core.<*> (x Core..@? "clientVpnEndpointId")
      Core.<*> (x Core..@? "description")
      Core.<*> (x Core..@? "destinationCidr")
      Core.<*> (x Core..@? "groupId")
      Core.<*> (x Core..@? "status")
