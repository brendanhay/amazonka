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
-- Module      : Amazonka.NetworkFirewall.Types.ServerCertificateConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.ServerCertificateConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types.ServerCertificate
import Amazonka.NetworkFirewall.Types.ServerCertificateScope
import qualified Amazonka.Prelude as Prelude

-- | Configures the associated Certificate Manager Secure Sockets
-- Layer\/Transport Layer Security (SSL\/TLS) server certificates and scope
-- settings Network Firewall uses to decrypt traffic in a
-- TLSInspectionConfiguration. For information about working with SSL\/TLS
-- certificates for TLS inspection, see
-- <https://docs.aws.amazon.com/network-firewall/latest/developerguide/tls-inspection-certificate-requirements.html Requirements for using SSL\/TLS server certficiates with TLS inspection configurations>
-- in the /Network Firewall Developer Guide/.
--
-- If a server certificate that\'s associated with your
-- TLSInspectionConfiguration is revoked, deleted, or expired it can result
-- in client-side TLS errors.
--
-- /See:/ 'newServerCertificateConfiguration' smart constructor.
data ServerCertificateConfiguration = ServerCertificateConfiguration'
  { -- | A list of a server certificate configuration\'s scopes.
    scopes :: Prelude.Maybe [ServerCertificateScope],
    -- | The list of a server certificate configuration\'s Certificate Manager
    -- SSL\/TLS certificates.
    serverCertificates :: Prelude.Maybe [ServerCertificate]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServerCertificateConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scopes', 'serverCertificateConfiguration_scopes' - A list of a server certificate configuration\'s scopes.
--
-- 'serverCertificates', 'serverCertificateConfiguration_serverCertificates' - The list of a server certificate configuration\'s Certificate Manager
-- SSL\/TLS certificates.
newServerCertificateConfiguration ::
  ServerCertificateConfiguration
newServerCertificateConfiguration =
  ServerCertificateConfiguration'
    { scopes =
        Prelude.Nothing,
      serverCertificates = Prelude.Nothing
    }

-- | A list of a server certificate configuration\'s scopes.
serverCertificateConfiguration_scopes :: Lens.Lens' ServerCertificateConfiguration (Prelude.Maybe [ServerCertificateScope])
serverCertificateConfiguration_scopes = Lens.lens (\ServerCertificateConfiguration' {scopes} -> scopes) (\s@ServerCertificateConfiguration' {} a -> s {scopes = a} :: ServerCertificateConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The list of a server certificate configuration\'s Certificate Manager
-- SSL\/TLS certificates.
serverCertificateConfiguration_serverCertificates :: Lens.Lens' ServerCertificateConfiguration (Prelude.Maybe [ServerCertificate])
serverCertificateConfiguration_serverCertificates = Lens.lens (\ServerCertificateConfiguration' {serverCertificates} -> serverCertificates) (\s@ServerCertificateConfiguration' {} a -> s {serverCertificates = a} :: ServerCertificateConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ServerCertificateConfiguration where
  parseJSON =
    Data.withObject
      "ServerCertificateConfiguration"
      ( \x ->
          ServerCertificateConfiguration'
            Prelude.<$> (x Data..:? "Scopes" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "ServerCertificates"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    ServerCertificateConfiguration
  where
  hashWithSalt
    _salt
    ServerCertificateConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` scopes
        `Prelude.hashWithSalt` serverCertificates

instance
  Prelude.NFData
    ServerCertificateConfiguration
  where
  rnf ServerCertificateConfiguration' {..} =
    Prelude.rnf scopes
      `Prelude.seq` Prelude.rnf serverCertificates

instance Data.ToJSON ServerCertificateConfiguration where
  toJSON ServerCertificateConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Scopes" Data..=) Prelude.<$> scopes,
            ("ServerCertificates" Data..=)
              Prelude.<$> serverCertificates
          ]
      )
