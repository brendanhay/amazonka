{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientVpnAuthenticationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ClientVpnAuthenticationRequest
  ( ClientVpnAuthenticationRequest (..)
  -- * Smart constructor
  , mkClientVpnAuthenticationRequest
  -- * Lenses
  , cvarActiveDirectory
  , cvarFederatedAuthentication
  , cvarMutualAuthentication
  , cvarType
  ) where

import qualified Network.AWS.EC2.Types.CertificateAuthenticationRequest as Types
import qualified Network.AWS.EC2.Types.ClientVpnAuthenticationType as Types
import qualified Network.AWS.EC2.Types.DirectoryServiceAuthenticationRequest as Types
import qualified Network.AWS.EC2.Types.FederatedAuthenticationRequest as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the authentication method to be used by a Client VPN endpoint. For more information, see <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/authentication-authrization.html#client-authentication Authentication> in the /AWS Client VPN Administrator Guide/ .
--
-- /See:/ 'mkClientVpnAuthenticationRequest' smart constructor.
data ClientVpnAuthenticationRequest = ClientVpnAuthenticationRequest'
  { activeDirectory :: Core.Maybe Types.DirectoryServiceAuthenticationRequest
    -- ^ Information about the Active Directory to be used, if applicable. You must provide this information if __Type__ is @directory-service-authentication@ .
  , federatedAuthentication :: Core.Maybe Types.FederatedAuthenticationRequest
    -- ^ Information about the IAM SAML identity provider to be used, if applicable. You must provide this information if __Type__ is @federated-authentication@ .
  , mutualAuthentication :: Core.Maybe Types.CertificateAuthenticationRequest
    -- ^ Information about the authentication certificates to be used, if applicable. You must provide this information if __Type__ is @certificate-authentication@ .
  , type' :: Core.Maybe Types.ClientVpnAuthenticationType
    -- ^ The type of client authentication to be used.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ClientVpnAuthenticationRequest' value with any optional fields omitted.
mkClientVpnAuthenticationRequest
    :: ClientVpnAuthenticationRequest
mkClientVpnAuthenticationRequest
  = ClientVpnAuthenticationRequest'{activeDirectory = Core.Nothing,
                                    federatedAuthentication = Core.Nothing,
                                    mutualAuthentication = Core.Nothing, type' = Core.Nothing}

-- | Information about the Active Directory to be used, if applicable. You must provide this information if __Type__ is @directory-service-authentication@ .
--
-- /Note:/ Consider using 'activeDirectory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvarActiveDirectory :: Lens.Lens' ClientVpnAuthenticationRequest (Core.Maybe Types.DirectoryServiceAuthenticationRequest)
cvarActiveDirectory = Lens.field @"activeDirectory"
{-# INLINEABLE cvarActiveDirectory #-}
{-# DEPRECATED activeDirectory "Use generic-lens or generic-optics with 'activeDirectory' instead"  #-}

-- | Information about the IAM SAML identity provider to be used, if applicable. You must provide this information if __Type__ is @federated-authentication@ .
--
-- /Note:/ Consider using 'federatedAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvarFederatedAuthentication :: Lens.Lens' ClientVpnAuthenticationRequest (Core.Maybe Types.FederatedAuthenticationRequest)
cvarFederatedAuthentication = Lens.field @"federatedAuthentication"
{-# INLINEABLE cvarFederatedAuthentication #-}
{-# DEPRECATED federatedAuthentication "Use generic-lens or generic-optics with 'federatedAuthentication' instead"  #-}

-- | Information about the authentication certificates to be used, if applicable. You must provide this information if __Type__ is @certificate-authentication@ .
--
-- /Note:/ Consider using 'mutualAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvarMutualAuthentication :: Lens.Lens' ClientVpnAuthenticationRequest (Core.Maybe Types.CertificateAuthenticationRequest)
cvarMutualAuthentication = Lens.field @"mutualAuthentication"
{-# INLINEABLE cvarMutualAuthentication #-}
{-# DEPRECATED mutualAuthentication "Use generic-lens or generic-optics with 'mutualAuthentication' instead"  #-}

-- | The type of client authentication to be used.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvarType :: Lens.Lens' ClientVpnAuthenticationRequest (Core.Maybe Types.ClientVpnAuthenticationType)
cvarType = Lens.field @"type'"
{-# INLINEABLE cvarType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.ToQuery ClientVpnAuthenticationRequest where
        toQuery ClientVpnAuthenticationRequest{..}
          = Core.maybe Core.mempty (Core.toQueryPair "ActiveDirectory")
              activeDirectory
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "FederatedAuthentication")
                federatedAuthentication
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MutualAuthentication")
                mutualAuthentication
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Type") type'
