{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientVPNAuthentication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientVPNAuthentication
  ( ClientVPNAuthentication (..),

    -- * Smart constructor
    mkClientVPNAuthentication,

    -- * Lenses
    cvaActiveDirectory,
    cvaFederatedAuthentication,
    cvaMutualAuthentication,
    cvaType,
  )
where

import Network.AWS.EC2.Types.CertificateAuthentication
import Network.AWS.EC2.Types.ClientVPNAuthenticationType
import Network.AWS.EC2.Types.DirectoryServiceAuthentication
import Network.AWS.EC2.Types.FederatedAuthentication
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the authentication methods used by a Client VPN endpoint. For more information, see <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/client-authentication.html Authentication> in the /AWS Client VPN Administrator Guide/ .
--
-- /See:/ 'mkClientVPNAuthentication' smart constructor.
data ClientVPNAuthentication = ClientVPNAuthentication'
  { -- | Information about the Active Directory, if applicable.
    activeDirectory :: Lude.Maybe DirectoryServiceAuthentication,
    -- | Information about the IAM SAML identity provider, if applicable.
    federatedAuthentication :: Lude.Maybe FederatedAuthentication,
    -- | Information about the authentication certificates, if applicable.
    mutualAuthentication :: Lude.Maybe CertificateAuthentication,
    -- | The authentication type used.
    type' :: Lude.Maybe ClientVPNAuthenticationType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClientVPNAuthentication' with the minimum fields required to make a request.
--
-- * 'activeDirectory' - Information about the Active Directory, if applicable.
-- * 'federatedAuthentication' - Information about the IAM SAML identity provider, if applicable.
-- * 'mutualAuthentication' - Information about the authentication certificates, if applicable.
-- * 'type'' - The authentication type used.
mkClientVPNAuthentication ::
  ClientVPNAuthentication
mkClientVPNAuthentication =
  ClientVPNAuthentication'
    { activeDirectory = Lude.Nothing,
      federatedAuthentication = Lude.Nothing,
      mutualAuthentication = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | Information about the Active Directory, if applicable.
--
-- /Note:/ Consider using 'activeDirectory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvaActiveDirectory :: Lens.Lens' ClientVPNAuthentication (Lude.Maybe DirectoryServiceAuthentication)
cvaActiveDirectory = Lens.lens (activeDirectory :: ClientVPNAuthentication -> Lude.Maybe DirectoryServiceAuthentication) (\s a -> s {activeDirectory = a} :: ClientVPNAuthentication)
{-# DEPRECATED cvaActiveDirectory "Use generic-lens or generic-optics with 'activeDirectory' instead." #-}

-- | Information about the IAM SAML identity provider, if applicable.
--
-- /Note:/ Consider using 'federatedAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvaFederatedAuthentication :: Lens.Lens' ClientVPNAuthentication (Lude.Maybe FederatedAuthentication)
cvaFederatedAuthentication = Lens.lens (federatedAuthentication :: ClientVPNAuthentication -> Lude.Maybe FederatedAuthentication) (\s a -> s {federatedAuthentication = a} :: ClientVPNAuthentication)
{-# DEPRECATED cvaFederatedAuthentication "Use generic-lens or generic-optics with 'federatedAuthentication' instead." #-}

-- | Information about the authentication certificates, if applicable.
--
-- /Note:/ Consider using 'mutualAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvaMutualAuthentication :: Lens.Lens' ClientVPNAuthentication (Lude.Maybe CertificateAuthentication)
cvaMutualAuthentication = Lens.lens (mutualAuthentication :: ClientVPNAuthentication -> Lude.Maybe CertificateAuthentication) (\s a -> s {mutualAuthentication = a} :: ClientVPNAuthentication)
{-# DEPRECATED cvaMutualAuthentication "Use generic-lens or generic-optics with 'mutualAuthentication' instead." #-}

-- | The authentication type used.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvaType :: Lens.Lens' ClientVPNAuthentication (Lude.Maybe ClientVPNAuthenticationType)
cvaType = Lens.lens (type' :: ClientVPNAuthentication -> Lude.Maybe ClientVPNAuthenticationType) (\s a -> s {type' = a} :: ClientVPNAuthentication)
{-# DEPRECATED cvaType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromXML ClientVPNAuthentication where
  parseXML x =
    ClientVPNAuthentication'
      Lude.<$> (x Lude..@? "activeDirectory")
      Lude.<*> (x Lude..@? "federatedAuthentication")
      Lude.<*> (x Lude..@? "mutualAuthentication")
      Lude.<*> (x Lude..@? "type")
