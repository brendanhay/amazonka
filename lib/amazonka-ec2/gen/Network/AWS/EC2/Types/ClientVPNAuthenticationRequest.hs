{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientVPNAuthenticationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientVPNAuthenticationRequest
  ( ClientVPNAuthenticationRequest (..),

    -- * Smart constructor
    mkClientVPNAuthenticationRequest,

    -- * Lenses
    cvarActiveDirectory,
    cvarFederatedAuthentication,
    cvarMutualAuthentication,
    cvarType,
  )
where

import Network.AWS.EC2.Types.CertificateAuthenticationRequest
import Network.AWS.EC2.Types.ClientVPNAuthenticationType
import Network.AWS.EC2.Types.DirectoryServiceAuthenticationRequest
import Network.AWS.EC2.Types.FederatedAuthenticationRequest
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the authentication method to be used by a Client VPN endpoint. For more information, see <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/authentication-authrization.html#client-authentication Authentication> in the /AWS Client VPN Administrator Guide/ .
--
-- /See:/ 'mkClientVPNAuthenticationRequest' smart constructor.
data ClientVPNAuthenticationRequest = ClientVPNAuthenticationRequest'
  { -- | Information about the Active Directory to be used, if applicable. You must provide this information if __Type__ is @directory-service-authentication@ .
    activeDirectory :: Lude.Maybe DirectoryServiceAuthenticationRequest,
    -- | Information about the IAM SAML identity provider to be used, if applicable. You must provide this information if __Type__ is @federated-authentication@ .
    federatedAuthentication :: Lude.Maybe FederatedAuthenticationRequest,
    -- | Information about the authentication certificates to be used, if applicable. You must provide this information if __Type__ is @certificate-authentication@ .
    mutualAuthentication :: Lude.Maybe CertificateAuthenticationRequest,
    -- | The type of client authentication to be used.
    type' :: Lude.Maybe ClientVPNAuthenticationType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClientVPNAuthenticationRequest' with the minimum fields required to make a request.
--
-- * 'activeDirectory' - Information about the Active Directory to be used, if applicable. You must provide this information if __Type__ is @directory-service-authentication@ .
-- * 'federatedAuthentication' - Information about the IAM SAML identity provider to be used, if applicable. You must provide this information if __Type__ is @federated-authentication@ .
-- * 'mutualAuthentication' - Information about the authentication certificates to be used, if applicable. You must provide this information if __Type__ is @certificate-authentication@ .
-- * 'type'' - The type of client authentication to be used.
mkClientVPNAuthenticationRequest ::
  ClientVPNAuthenticationRequest
mkClientVPNAuthenticationRequest =
  ClientVPNAuthenticationRequest'
    { activeDirectory = Lude.Nothing,
      federatedAuthentication = Lude.Nothing,
      mutualAuthentication = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | Information about the Active Directory to be used, if applicable. You must provide this information if __Type__ is @directory-service-authentication@ .
--
-- /Note:/ Consider using 'activeDirectory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvarActiveDirectory :: Lens.Lens' ClientVPNAuthenticationRequest (Lude.Maybe DirectoryServiceAuthenticationRequest)
cvarActiveDirectory = Lens.lens (activeDirectory :: ClientVPNAuthenticationRequest -> Lude.Maybe DirectoryServiceAuthenticationRequest) (\s a -> s {activeDirectory = a} :: ClientVPNAuthenticationRequest)
{-# DEPRECATED cvarActiveDirectory "Use generic-lens or generic-optics with 'activeDirectory' instead." #-}

-- | Information about the IAM SAML identity provider to be used, if applicable. You must provide this information if __Type__ is @federated-authentication@ .
--
-- /Note:/ Consider using 'federatedAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvarFederatedAuthentication :: Lens.Lens' ClientVPNAuthenticationRequest (Lude.Maybe FederatedAuthenticationRequest)
cvarFederatedAuthentication = Lens.lens (federatedAuthentication :: ClientVPNAuthenticationRequest -> Lude.Maybe FederatedAuthenticationRequest) (\s a -> s {federatedAuthentication = a} :: ClientVPNAuthenticationRequest)
{-# DEPRECATED cvarFederatedAuthentication "Use generic-lens or generic-optics with 'federatedAuthentication' instead." #-}

-- | Information about the authentication certificates to be used, if applicable. You must provide this information if __Type__ is @certificate-authentication@ .
--
-- /Note:/ Consider using 'mutualAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvarMutualAuthentication :: Lens.Lens' ClientVPNAuthenticationRequest (Lude.Maybe CertificateAuthenticationRequest)
cvarMutualAuthentication = Lens.lens (mutualAuthentication :: ClientVPNAuthenticationRequest -> Lude.Maybe CertificateAuthenticationRequest) (\s a -> s {mutualAuthentication = a} :: ClientVPNAuthenticationRequest)
{-# DEPRECATED cvarMutualAuthentication "Use generic-lens or generic-optics with 'mutualAuthentication' instead." #-}

-- | The type of client authentication to be used.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvarType :: Lens.Lens' ClientVPNAuthenticationRequest (Lude.Maybe ClientVPNAuthenticationType)
cvarType = Lens.lens (type' :: ClientVPNAuthenticationRequest -> Lude.Maybe ClientVPNAuthenticationType) (\s a -> s {type' = a} :: ClientVPNAuthenticationRequest)
{-# DEPRECATED cvarType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.ToQuery ClientVPNAuthenticationRequest where
  toQuery ClientVPNAuthenticationRequest' {..} =
    Lude.mconcat
      [ "ActiveDirectory" Lude.=: activeDirectory,
        "FederatedAuthentication" Lude.=: federatedAuthentication,
        "MutualAuthentication" Lude.=: mutualAuthentication,
        "Type" Lude.=: type'
      ]
