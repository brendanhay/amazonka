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
-- Module      : Amazonka.NetworkFirewall.Types.TLSInspectionConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.TLSInspectionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types.ServerCertificateConfiguration
import qualified Amazonka.Prelude as Prelude

-- | The object that defines a TLS inspection configuration. This, along with
-- TLSInspectionConfigurationResponse, define the TLS inspection
-- configuration. You can retrieve all objects for a TLS inspection
-- configuration by calling DescribeTLSInspectionConfiguration.
--
-- Network Firewall uses a TLS inspection configuration to decrypt traffic.
-- Network Firewall re-encrypts the traffic before sending it to its
-- destination.
--
-- To use a TLS inspection configuration, you add it to a Network Firewall
-- firewall policy, then you apply the firewall policy to a firewall.
-- Network Firewall acts as a proxy service to decrypt and inspect inbound
-- traffic. You can reference a TLS inspection configuration from more than
-- one firewall policy, and you can use a firewall policy in more than one
-- firewall. For more information about using TLS inspection
-- configurations, see
-- <https://docs.aws.amazon.com/network-firewall/latest/developerguide/tls-inspection.html Decrypting SSL\/TLS traffic with TLS inspection configurations>
-- in the /Network Firewall Developer Guide/.
--
-- /See:/ 'newTLSInspectionConfiguration' smart constructor.
data TLSInspectionConfiguration = TLSInspectionConfiguration'
  { -- | Lists the server certificate configurations that are associated with the
    -- TLS configuration.
    serverCertificateConfigurations :: Prelude.Maybe [ServerCertificateConfiguration]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TLSInspectionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverCertificateConfigurations', 'tLSInspectionConfiguration_serverCertificateConfigurations' - Lists the server certificate configurations that are associated with the
-- TLS configuration.
newTLSInspectionConfiguration ::
  TLSInspectionConfiguration
newTLSInspectionConfiguration =
  TLSInspectionConfiguration'
    { serverCertificateConfigurations =
        Prelude.Nothing
    }

-- | Lists the server certificate configurations that are associated with the
-- TLS configuration.
tLSInspectionConfiguration_serverCertificateConfigurations :: Lens.Lens' TLSInspectionConfiguration (Prelude.Maybe [ServerCertificateConfiguration])
tLSInspectionConfiguration_serverCertificateConfigurations = Lens.lens (\TLSInspectionConfiguration' {serverCertificateConfigurations} -> serverCertificateConfigurations) (\s@TLSInspectionConfiguration' {} a -> s {serverCertificateConfigurations = a} :: TLSInspectionConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON TLSInspectionConfiguration where
  parseJSON =
    Data.withObject
      "TLSInspectionConfiguration"
      ( \x ->
          TLSInspectionConfiguration'
            Prelude.<$> ( x
                            Data..:? "ServerCertificateConfigurations"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable TLSInspectionConfiguration where
  hashWithSalt _salt TLSInspectionConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` serverCertificateConfigurations

instance Prelude.NFData TLSInspectionConfiguration where
  rnf TLSInspectionConfiguration' {..} =
    Prelude.rnf serverCertificateConfigurations

instance Data.ToJSON TLSInspectionConfiguration where
  toJSON TLSInspectionConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ServerCertificateConfigurations" Data..=)
              Prelude.<$> serverCertificateConfigurations
          ]
      )
