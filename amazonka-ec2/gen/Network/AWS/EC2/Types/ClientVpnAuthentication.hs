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
-- Module      : Network.AWS.EC2.Types.ClientVpnAuthentication
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientVpnAuthentication where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.CertificateAuthentication
import Network.AWS.EC2.Types.ClientVpnAuthenticationType
import Network.AWS.EC2.Types.DirectoryServiceAuthentication
import Network.AWS.EC2.Types.FederatedAuthentication
import qualified Network.AWS.Lens as Lens

-- | Describes the authentication methods used by a Client VPN endpoint. For
-- more information, see
-- <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/client-authentication.html Authentication>
-- in the /AWS Client VPN Administrator Guide/.
--
-- /See:/ 'newClientVpnAuthentication' smart constructor.
data ClientVpnAuthentication = ClientVpnAuthentication'
  { -- | Information about the IAM SAML identity provider, if applicable.
    federatedAuthentication :: Core.Maybe FederatedAuthentication,
    -- | Information about the Active Directory, if applicable.
    activeDirectory :: Core.Maybe DirectoryServiceAuthentication,
    -- | Information about the authentication certificates, if applicable.
    mutualAuthentication :: Core.Maybe CertificateAuthentication,
    -- | The authentication type used.
    type' :: Core.Maybe ClientVpnAuthenticationType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ClientVpnAuthentication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'federatedAuthentication', 'clientVpnAuthentication_federatedAuthentication' - Information about the IAM SAML identity provider, if applicable.
--
-- 'activeDirectory', 'clientVpnAuthentication_activeDirectory' - Information about the Active Directory, if applicable.
--
-- 'mutualAuthentication', 'clientVpnAuthentication_mutualAuthentication' - Information about the authentication certificates, if applicable.
--
-- 'type'', 'clientVpnAuthentication_type' - The authentication type used.
newClientVpnAuthentication ::
  ClientVpnAuthentication
newClientVpnAuthentication =
  ClientVpnAuthentication'
    { federatedAuthentication =
        Core.Nothing,
      activeDirectory = Core.Nothing,
      mutualAuthentication = Core.Nothing,
      type' = Core.Nothing
    }

-- | Information about the IAM SAML identity provider, if applicable.
clientVpnAuthentication_federatedAuthentication :: Lens.Lens' ClientVpnAuthentication (Core.Maybe FederatedAuthentication)
clientVpnAuthentication_federatedAuthentication = Lens.lens (\ClientVpnAuthentication' {federatedAuthentication} -> federatedAuthentication) (\s@ClientVpnAuthentication' {} a -> s {federatedAuthentication = a} :: ClientVpnAuthentication)

-- | Information about the Active Directory, if applicable.
clientVpnAuthentication_activeDirectory :: Lens.Lens' ClientVpnAuthentication (Core.Maybe DirectoryServiceAuthentication)
clientVpnAuthentication_activeDirectory = Lens.lens (\ClientVpnAuthentication' {activeDirectory} -> activeDirectory) (\s@ClientVpnAuthentication' {} a -> s {activeDirectory = a} :: ClientVpnAuthentication)

-- | Information about the authentication certificates, if applicable.
clientVpnAuthentication_mutualAuthentication :: Lens.Lens' ClientVpnAuthentication (Core.Maybe CertificateAuthentication)
clientVpnAuthentication_mutualAuthentication = Lens.lens (\ClientVpnAuthentication' {mutualAuthentication} -> mutualAuthentication) (\s@ClientVpnAuthentication' {} a -> s {mutualAuthentication = a} :: ClientVpnAuthentication)

-- | The authentication type used.
clientVpnAuthentication_type :: Lens.Lens' ClientVpnAuthentication (Core.Maybe ClientVpnAuthenticationType)
clientVpnAuthentication_type = Lens.lens (\ClientVpnAuthentication' {type'} -> type') (\s@ClientVpnAuthentication' {} a -> s {type' = a} :: ClientVpnAuthentication)

instance Core.FromXML ClientVpnAuthentication where
  parseXML x =
    ClientVpnAuthentication'
      Core.<$> (x Core..@? "federatedAuthentication")
      Core.<*> (x Core..@? "activeDirectory")
      Core.<*> (x Core..@? "mutualAuthentication")
      Core.<*> (x Core..@? "type")

instance Core.Hashable ClientVpnAuthentication

instance Core.NFData ClientVpnAuthentication
