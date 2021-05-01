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
-- Module      : Network.AWS.EC2.Types.ClientVpnAuthenticationRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientVpnAuthenticationRequest where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.CertificateAuthenticationRequest
import Network.AWS.EC2.Types.ClientVpnAuthenticationType
import Network.AWS.EC2.Types.DirectoryServiceAuthenticationRequest
import Network.AWS.EC2.Types.FederatedAuthenticationRequest
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the authentication method to be used by a Client VPN endpoint.
-- For more information, see
-- <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/authentication-authrization.html#client-authentication Authentication>
-- in the /AWS Client VPN Administrator Guide/.
--
-- /See:/ 'newClientVpnAuthenticationRequest' smart constructor.
data ClientVpnAuthenticationRequest = ClientVpnAuthenticationRequest'
  { -- | Information about the IAM SAML identity provider to be used, if
    -- applicable. You must provide this information if __Type__ is
    -- @federated-authentication@.
    federatedAuthentication :: Prelude.Maybe FederatedAuthenticationRequest,
    -- | Information about the Active Directory to be used, if applicable. You
    -- must provide this information if __Type__ is
    -- @directory-service-authentication@.
    activeDirectory :: Prelude.Maybe DirectoryServiceAuthenticationRequest,
    -- | Information about the authentication certificates to be used, if
    -- applicable. You must provide this information if __Type__ is
    -- @certificate-authentication@.
    mutualAuthentication :: Prelude.Maybe CertificateAuthenticationRequest,
    -- | The type of client authentication to be used.
    type' :: Prelude.Maybe ClientVpnAuthenticationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ClientVpnAuthenticationRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'federatedAuthentication', 'clientVpnAuthenticationRequest_federatedAuthentication' - Information about the IAM SAML identity provider to be used, if
-- applicable. You must provide this information if __Type__ is
-- @federated-authentication@.
--
-- 'activeDirectory', 'clientVpnAuthenticationRequest_activeDirectory' - Information about the Active Directory to be used, if applicable. You
-- must provide this information if __Type__ is
-- @directory-service-authentication@.
--
-- 'mutualAuthentication', 'clientVpnAuthenticationRequest_mutualAuthentication' - Information about the authentication certificates to be used, if
-- applicable. You must provide this information if __Type__ is
-- @certificate-authentication@.
--
-- 'type'', 'clientVpnAuthenticationRequest_type' - The type of client authentication to be used.
newClientVpnAuthenticationRequest ::
  ClientVpnAuthenticationRequest
newClientVpnAuthenticationRequest =
  ClientVpnAuthenticationRequest'
    { federatedAuthentication =
        Prelude.Nothing,
      activeDirectory = Prelude.Nothing,
      mutualAuthentication = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | Information about the IAM SAML identity provider to be used, if
-- applicable. You must provide this information if __Type__ is
-- @federated-authentication@.
clientVpnAuthenticationRequest_federatedAuthentication :: Lens.Lens' ClientVpnAuthenticationRequest (Prelude.Maybe FederatedAuthenticationRequest)
clientVpnAuthenticationRequest_federatedAuthentication = Lens.lens (\ClientVpnAuthenticationRequest' {federatedAuthentication} -> federatedAuthentication) (\s@ClientVpnAuthenticationRequest' {} a -> s {federatedAuthentication = a} :: ClientVpnAuthenticationRequest)

-- | Information about the Active Directory to be used, if applicable. You
-- must provide this information if __Type__ is
-- @directory-service-authentication@.
clientVpnAuthenticationRequest_activeDirectory :: Lens.Lens' ClientVpnAuthenticationRequest (Prelude.Maybe DirectoryServiceAuthenticationRequest)
clientVpnAuthenticationRequest_activeDirectory = Lens.lens (\ClientVpnAuthenticationRequest' {activeDirectory} -> activeDirectory) (\s@ClientVpnAuthenticationRequest' {} a -> s {activeDirectory = a} :: ClientVpnAuthenticationRequest)

-- | Information about the authentication certificates to be used, if
-- applicable. You must provide this information if __Type__ is
-- @certificate-authentication@.
clientVpnAuthenticationRequest_mutualAuthentication :: Lens.Lens' ClientVpnAuthenticationRequest (Prelude.Maybe CertificateAuthenticationRequest)
clientVpnAuthenticationRequest_mutualAuthentication = Lens.lens (\ClientVpnAuthenticationRequest' {mutualAuthentication} -> mutualAuthentication) (\s@ClientVpnAuthenticationRequest' {} a -> s {mutualAuthentication = a} :: ClientVpnAuthenticationRequest)

-- | The type of client authentication to be used.
clientVpnAuthenticationRequest_type :: Lens.Lens' ClientVpnAuthenticationRequest (Prelude.Maybe ClientVpnAuthenticationType)
clientVpnAuthenticationRequest_type = Lens.lens (\ClientVpnAuthenticationRequest' {type'} -> type') (\s@ClientVpnAuthenticationRequest' {} a -> s {type' = a} :: ClientVpnAuthenticationRequest)

instance
  Prelude.Hashable
    ClientVpnAuthenticationRequest

instance
  Prelude.NFData
    ClientVpnAuthenticationRequest

instance
  Prelude.ToQuery
    ClientVpnAuthenticationRequest
  where
  toQuery ClientVpnAuthenticationRequest' {..} =
    Prelude.mconcat
      [ "FederatedAuthentication"
          Prelude.=: federatedAuthentication,
        "ActiveDirectory" Prelude.=: activeDirectory,
        "MutualAuthentication"
          Prelude.=: mutualAuthentication,
        "Type" Prelude.=: type'
      ]
