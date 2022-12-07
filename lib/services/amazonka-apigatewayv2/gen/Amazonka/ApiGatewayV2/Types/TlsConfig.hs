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
-- Module      : Amazonka.ApiGatewayV2.Types.TlsConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApiGatewayV2.Types.TlsConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The TLS configuration for a private integration. If you specify a TLS
-- configuration, private integration traffic uses the HTTPS protocol.
-- Supported only for HTTP APIs.
--
-- /See:/ 'newTlsConfig' smart constructor.
data TlsConfig = TlsConfig'
  { -- | If you specify a server name, API Gateway uses it to verify the hostname
    -- on the integration\'s certificate. The server name is also included in
    -- the TLS handshake to support Server Name Indication (SNI) or virtual
    -- hosting.
    serverNameToVerify :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TlsConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverNameToVerify', 'tlsConfig_serverNameToVerify' - If you specify a server name, API Gateway uses it to verify the hostname
-- on the integration\'s certificate. The server name is also included in
-- the TLS handshake to support Server Name Indication (SNI) or virtual
-- hosting.
newTlsConfig ::
  TlsConfig
newTlsConfig =
  TlsConfig' {serverNameToVerify = Prelude.Nothing}

-- | If you specify a server name, API Gateway uses it to verify the hostname
-- on the integration\'s certificate. The server name is also included in
-- the TLS handshake to support Server Name Indication (SNI) or virtual
-- hosting.
tlsConfig_serverNameToVerify :: Lens.Lens' TlsConfig (Prelude.Maybe Prelude.Text)
tlsConfig_serverNameToVerify = Lens.lens (\TlsConfig' {serverNameToVerify} -> serverNameToVerify) (\s@TlsConfig' {} a -> s {serverNameToVerify = a} :: TlsConfig)

instance Data.FromJSON TlsConfig where
  parseJSON =
    Data.withObject
      "TlsConfig"
      ( \x ->
          TlsConfig'
            Prelude.<$> (x Data..:? "serverNameToVerify")
      )

instance Prelude.Hashable TlsConfig where
  hashWithSalt _salt TlsConfig' {..} =
    _salt `Prelude.hashWithSalt` serverNameToVerify

instance Prelude.NFData TlsConfig where
  rnf TlsConfig' {..} = Prelude.rnf serverNameToVerify
