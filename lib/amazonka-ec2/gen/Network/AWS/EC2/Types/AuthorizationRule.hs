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
    arStatus,
    arAccessAll,
    arClientVPNEndpointId,
    arGroupId,
    arDestinationCidr,
    arDescription,
  )
where

import Network.AWS.EC2.Types.ClientVPNAuthorizationRuleStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about an authorization rule.
--
-- /See:/ 'mkAuthorizationRule' smart constructor.
data AuthorizationRule = AuthorizationRule'
  { -- | The current state of the authorization rule.
    status :: Lude.Maybe ClientVPNAuthorizationRuleStatus,
    -- | Indicates whether the authorization rule grants access to all clients.
    accessAll :: Lude.Maybe Lude.Bool,
    -- | The ID of the Client VPN endpoint with which the authorization rule is associated.
    clientVPNEndpointId :: Lude.Maybe Lude.Text,
    -- | The ID of the Active Directory group to which the authorization rule grants access.
    groupId :: Lude.Maybe Lude.Text,
    -- | The IPv4 address range, in CIDR notation, of the network to which the authorization rule applies.
    destinationCidr :: Lude.Maybe Lude.Text,
    -- | A brief description of the authorization rule.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AuthorizationRule' with the minimum fields required to make a request.
--
-- * 'status' - The current state of the authorization rule.
-- * 'accessAll' - Indicates whether the authorization rule grants access to all clients.
-- * 'clientVPNEndpointId' - The ID of the Client VPN endpoint with which the authorization rule is associated.
-- * 'groupId' - The ID of the Active Directory group to which the authorization rule grants access.
-- * 'destinationCidr' - The IPv4 address range, in CIDR notation, of the network to which the authorization rule applies.
-- * 'description' - A brief description of the authorization rule.
mkAuthorizationRule ::
  AuthorizationRule
mkAuthorizationRule =
  AuthorizationRule'
    { status = Lude.Nothing,
      accessAll = Lude.Nothing,
      clientVPNEndpointId = Lude.Nothing,
      groupId = Lude.Nothing,
      destinationCidr = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The current state of the authorization rule.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arStatus :: Lens.Lens' AuthorizationRule (Lude.Maybe ClientVPNAuthorizationRuleStatus)
arStatus = Lens.lens (status :: AuthorizationRule -> Lude.Maybe ClientVPNAuthorizationRuleStatus) (\s a -> s {status = a} :: AuthorizationRule)
{-# DEPRECATED arStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Indicates whether the authorization rule grants access to all clients.
--
-- /Note:/ Consider using 'accessAll' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arAccessAll :: Lens.Lens' AuthorizationRule (Lude.Maybe Lude.Bool)
arAccessAll = Lens.lens (accessAll :: AuthorizationRule -> Lude.Maybe Lude.Bool) (\s a -> s {accessAll = a} :: AuthorizationRule)
{-# DEPRECATED arAccessAll "Use generic-lens or generic-optics with 'accessAll' instead." #-}

-- | The ID of the Client VPN endpoint with which the authorization rule is associated.
--
-- /Note:/ Consider using 'clientVPNEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arClientVPNEndpointId :: Lens.Lens' AuthorizationRule (Lude.Maybe Lude.Text)
arClientVPNEndpointId = Lens.lens (clientVPNEndpointId :: AuthorizationRule -> Lude.Maybe Lude.Text) (\s a -> s {clientVPNEndpointId = a} :: AuthorizationRule)
{-# DEPRECATED arClientVPNEndpointId "Use generic-lens or generic-optics with 'clientVPNEndpointId' instead." #-}

-- | The ID of the Active Directory group to which the authorization rule grants access.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arGroupId :: Lens.Lens' AuthorizationRule (Lude.Maybe Lude.Text)
arGroupId = Lens.lens (groupId :: AuthorizationRule -> Lude.Maybe Lude.Text) (\s a -> s {groupId = a} :: AuthorizationRule)
{-# DEPRECATED arGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | The IPv4 address range, in CIDR notation, of the network to which the authorization rule applies.
--
-- /Note:/ Consider using 'destinationCidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arDestinationCidr :: Lens.Lens' AuthorizationRule (Lude.Maybe Lude.Text)
arDestinationCidr = Lens.lens (destinationCidr :: AuthorizationRule -> Lude.Maybe Lude.Text) (\s a -> s {destinationCidr = a} :: AuthorizationRule)
{-# DEPRECATED arDestinationCidr "Use generic-lens or generic-optics with 'destinationCidr' instead." #-}

-- | A brief description of the authorization rule.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arDescription :: Lens.Lens' AuthorizationRule (Lude.Maybe Lude.Text)
arDescription = Lens.lens (description :: AuthorizationRule -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: AuthorizationRule)
{-# DEPRECATED arDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromXML AuthorizationRule where
  parseXML x =
    AuthorizationRule'
      Lude.<$> (x Lude..@? "status")
      Lude.<*> (x Lude..@? "accessAll")
      Lude.<*> (x Lude..@? "clientVpnEndpointId")
      Lude.<*> (x Lude..@? "groupId")
      Lude.<*> (x Lude..@? "destinationCidr")
      Lude.<*> (x Lude..@? "description")
