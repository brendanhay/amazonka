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
-- Module      : Amazonka.EC2.Types.ClientVpnAuthentication
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ClientVpnAuthentication where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.CertificateAuthentication
import Amazonka.EC2.Types.ClientVpnAuthenticationType
import Amazonka.EC2.Types.DirectoryServiceAuthentication
import Amazonka.EC2.Types.FederatedAuthentication
import qualified Amazonka.Prelude as Prelude

-- | Describes the authentication methods used by a Client VPN endpoint. For
-- more information, see
-- <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/client-authentication.html Authentication>
-- in the /Client VPN Administrator Guide/.
--
-- /See:/ 'newClientVpnAuthentication' smart constructor.
data ClientVpnAuthentication = ClientVpnAuthentication'
  { -- | The authentication type used.
    type' :: Prelude.Maybe ClientVpnAuthenticationType,
    -- | Information about the IAM SAML identity provider, if applicable.
    federatedAuthentication :: Prelude.Maybe FederatedAuthentication,
    -- | Information about the authentication certificates, if applicable.
    mutualAuthentication :: Prelude.Maybe CertificateAuthentication,
    -- | Information about the Active Directory, if applicable.
    activeDirectory :: Prelude.Maybe DirectoryServiceAuthentication
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClientVpnAuthentication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'clientVpnAuthentication_type' - The authentication type used.
--
-- 'federatedAuthentication', 'clientVpnAuthentication_federatedAuthentication' - Information about the IAM SAML identity provider, if applicable.
--
-- 'mutualAuthentication', 'clientVpnAuthentication_mutualAuthentication' - Information about the authentication certificates, if applicable.
--
-- 'activeDirectory', 'clientVpnAuthentication_activeDirectory' - Information about the Active Directory, if applicable.
newClientVpnAuthentication ::
  ClientVpnAuthentication
newClientVpnAuthentication =
  ClientVpnAuthentication'
    { type' = Prelude.Nothing,
      federatedAuthentication = Prelude.Nothing,
      mutualAuthentication = Prelude.Nothing,
      activeDirectory = Prelude.Nothing
    }

-- | The authentication type used.
clientVpnAuthentication_type :: Lens.Lens' ClientVpnAuthentication (Prelude.Maybe ClientVpnAuthenticationType)
clientVpnAuthentication_type = Lens.lens (\ClientVpnAuthentication' {type'} -> type') (\s@ClientVpnAuthentication' {} a -> s {type' = a} :: ClientVpnAuthentication)

-- | Information about the IAM SAML identity provider, if applicable.
clientVpnAuthentication_federatedAuthentication :: Lens.Lens' ClientVpnAuthentication (Prelude.Maybe FederatedAuthentication)
clientVpnAuthentication_federatedAuthentication = Lens.lens (\ClientVpnAuthentication' {federatedAuthentication} -> federatedAuthentication) (\s@ClientVpnAuthentication' {} a -> s {federatedAuthentication = a} :: ClientVpnAuthentication)

-- | Information about the authentication certificates, if applicable.
clientVpnAuthentication_mutualAuthentication :: Lens.Lens' ClientVpnAuthentication (Prelude.Maybe CertificateAuthentication)
clientVpnAuthentication_mutualAuthentication = Lens.lens (\ClientVpnAuthentication' {mutualAuthentication} -> mutualAuthentication) (\s@ClientVpnAuthentication' {} a -> s {mutualAuthentication = a} :: ClientVpnAuthentication)

-- | Information about the Active Directory, if applicable.
clientVpnAuthentication_activeDirectory :: Lens.Lens' ClientVpnAuthentication (Prelude.Maybe DirectoryServiceAuthentication)
clientVpnAuthentication_activeDirectory = Lens.lens (\ClientVpnAuthentication' {activeDirectory} -> activeDirectory) (\s@ClientVpnAuthentication' {} a -> s {activeDirectory = a} :: ClientVpnAuthentication)

instance Data.FromXML ClientVpnAuthentication where
  parseXML x =
    ClientVpnAuthentication'
      Prelude.<$> (x Data..@? "type")
      Prelude.<*> (x Data..@? "federatedAuthentication")
      Prelude.<*> (x Data..@? "mutualAuthentication")
      Prelude.<*> (x Data..@? "activeDirectory")

instance Prelude.Hashable ClientVpnAuthentication where
  hashWithSalt _salt ClientVpnAuthentication' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` federatedAuthentication
      `Prelude.hashWithSalt` mutualAuthentication
      `Prelude.hashWithSalt` activeDirectory

instance Prelude.NFData ClientVpnAuthentication where
  rnf ClientVpnAuthentication' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf federatedAuthentication
      `Prelude.seq` Prelude.rnf mutualAuthentication
      `Prelude.seq` Prelude.rnf activeDirectory
