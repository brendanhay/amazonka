{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientVpnAuthentication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ClientVpnAuthentication
  ( ClientVpnAuthentication (..)
  -- * Smart constructor
  , mkClientVpnAuthentication
  -- * Lenses
  , cvaActiveDirectory
  , cvaFederatedAuthentication
  , cvaMutualAuthentication
  , cvaType
  ) where

import qualified Network.AWS.EC2.Types.CertificateAuthentication as Types
import qualified Network.AWS.EC2.Types.ClientVpnAuthenticationType as Types
import qualified Network.AWS.EC2.Types.DirectoryServiceAuthentication as Types
import qualified Network.AWS.EC2.Types.FederatedAuthentication as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the authentication methods used by a Client VPN endpoint. For more information, see <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/client-authentication.html Authentication> in the /AWS Client VPN Administrator Guide/ .
--
-- /See:/ 'mkClientVpnAuthentication' smart constructor.
data ClientVpnAuthentication = ClientVpnAuthentication'
  { activeDirectory :: Core.Maybe Types.DirectoryServiceAuthentication
    -- ^ Information about the Active Directory, if applicable.
  , federatedAuthentication :: Core.Maybe Types.FederatedAuthentication
    -- ^ Information about the IAM SAML identity provider, if applicable.
  , mutualAuthentication :: Core.Maybe Types.CertificateAuthentication
    -- ^ Information about the authentication certificates, if applicable.
  , type' :: Core.Maybe Types.ClientVpnAuthenticationType
    -- ^ The authentication type used.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ClientVpnAuthentication' value with any optional fields omitted.
mkClientVpnAuthentication
    :: ClientVpnAuthentication
mkClientVpnAuthentication
  = ClientVpnAuthentication'{activeDirectory = Core.Nothing,
                             federatedAuthentication = Core.Nothing,
                             mutualAuthentication = Core.Nothing, type' = Core.Nothing}

-- | Information about the Active Directory, if applicable.
--
-- /Note:/ Consider using 'activeDirectory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvaActiveDirectory :: Lens.Lens' ClientVpnAuthentication (Core.Maybe Types.DirectoryServiceAuthentication)
cvaActiveDirectory = Lens.field @"activeDirectory"
{-# INLINEABLE cvaActiveDirectory #-}
{-# DEPRECATED activeDirectory "Use generic-lens or generic-optics with 'activeDirectory' instead"  #-}

-- | Information about the IAM SAML identity provider, if applicable.
--
-- /Note:/ Consider using 'federatedAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvaFederatedAuthentication :: Lens.Lens' ClientVpnAuthentication (Core.Maybe Types.FederatedAuthentication)
cvaFederatedAuthentication = Lens.field @"federatedAuthentication"
{-# INLINEABLE cvaFederatedAuthentication #-}
{-# DEPRECATED federatedAuthentication "Use generic-lens or generic-optics with 'federatedAuthentication' instead"  #-}

-- | Information about the authentication certificates, if applicable.
--
-- /Note:/ Consider using 'mutualAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvaMutualAuthentication :: Lens.Lens' ClientVpnAuthentication (Core.Maybe Types.CertificateAuthentication)
cvaMutualAuthentication = Lens.field @"mutualAuthentication"
{-# INLINEABLE cvaMutualAuthentication #-}
{-# DEPRECATED mutualAuthentication "Use generic-lens or generic-optics with 'mutualAuthentication' instead"  #-}

-- | The authentication type used.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvaType :: Lens.Lens' ClientVpnAuthentication (Core.Maybe Types.ClientVpnAuthenticationType)
cvaType = Lens.field @"type'"
{-# INLINEABLE cvaType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromXML ClientVpnAuthentication where
        parseXML x
          = ClientVpnAuthentication' Core.<$>
              (x Core..@? "activeDirectory") Core.<*>
                x Core..@? "federatedAuthentication"
                Core.<*> x Core..@? "mutualAuthentication"
                Core.<*> x Core..@? "type"
