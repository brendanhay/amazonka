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
-- Module      : Amazonka.Kendra.Types.ProxyConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.ProxyConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides the configuration information for a web proxy to connect to
-- website hosts.
--
-- /See:/ 'newProxyConfiguration' smart constructor.
data ProxyConfiguration = ProxyConfiguration'
  { -- | Your secret ARN, which you can create in
    -- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/intro.html Secrets Manager>
    --
    -- The credentials are optional. You use a secret if web proxy credentials
    -- are required to connect to a website host. Amazon Kendra currently
    -- support basic authentication to connect to a web proxy server. The
    -- secret stores your credentials.
    credentials :: Prelude.Maybe Prelude.Text,
    -- | The name of the website host you want to connect to via a web proxy
    -- server.
    --
    -- For example, the host name of https:\/\/a.example.com\/page1.html is
    -- \"a.example.com\".
    host :: Prelude.Text,
    -- | The port number of the website host you want to connect to via a web
    -- proxy server.
    --
    -- For example, the port for https:\/\/a.example.com\/page1.html is 443,
    -- the standard port for HTTPS.
    port :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProxyConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'credentials', 'proxyConfiguration_credentials' - Your secret ARN, which you can create in
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/intro.html Secrets Manager>
--
-- The credentials are optional. You use a secret if web proxy credentials
-- are required to connect to a website host. Amazon Kendra currently
-- support basic authentication to connect to a web proxy server. The
-- secret stores your credentials.
--
-- 'host', 'proxyConfiguration_host' - The name of the website host you want to connect to via a web proxy
-- server.
--
-- For example, the host name of https:\/\/a.example.com\/page1.html is
-- \"a.example.com\".
--
-- 'port', 'proxyConfiguration_port' - The port number of the website host you want to connect to via a web
-- proxy server.
--
-- For example, the port for https:\/\/a.example.com\/page1.html is 443,
-- the standard port for HTTPS.
newProxyConfiguration ::
  -- | 'host'
  Prelude.Text ->
  -- | 'port'
  Prelude.Natural ->
  ProxyConfiguration
newProxyConfiguration pHost_ pPort_ =
  ProxyConfiguration'
    { credentials = Prelude.Nothing,
      host = pHost_,
      port = pPort_
    }

-- | Your secret ARN, which you can create in
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/intro.html Secrets Manager>
--
-- The credentials are optional. You use a secret if web proxy credentials
-- are required to connect to a website host. Amazon Kendra currently
-- support basic authentication to connect to a web proxy server. The
-- secret stores your credentials.
proxyConfiguration_credentials :: Lens.Lens' ProxyConfiguration (Prelude.Maybe Prelude.Text)
proxyConfiguration_credentials = Lens.lens (\ProxyConfiguration' {credentials} -> credentials) (\s@ProxyConfiguration' {} a -> s {credentials = a} :: ProxyConfiguration)

-- | The name of the website host you want to connect to via a web proxy
-- server.
--
-- For example, the host name of https:\/\/a.example.com\/page1.html is
-- \"a.example.com\".
proxyConfiguration_host :: Lens.Lens' ProxyConfiguration Prelude.Text
proxyConfiguration_host = Lens.lens (\ProxyConfiguration' {host} -> host) (\s@ProxyConfiguration' {} a -> s {host = a} :: ProxyConfiguration)

-- | The port number of the website host you want to connect to via a web
-- proxy server.
--
-- For example, the port for https:\/\/a.example.com\/page1.html is 443,
-- the standard port for HTTPS.
proxyConfiguration_port :: Lens.Lens' ProxyConfiguration Prelude.Natural
proxyConfiguration_port = Lens.lens (\ProxyConfiguration' {port} -> port) (\s@ProxyConfiguration' {} a -> s {port = a} :: ProxyConfiguration)

instance Data.FromJSON ProxyConfiguration where
  parseJSON =
    Data.withObject
      "ProxyConfiguration"
      ( \x ->
          ProxyConfiguration'
            Prelude.<$> (x Data..:? "Credentials")
            Prelude.<*> (x Data..: "Host")
            Prelude.<*> (x Data..: "Port")
      )

instance Prelude.Hashable ProxyConfiguration where
  hashWithSalt _salt ProxyConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` credentials
      `Prelude.hashWithSalt` host
      `Prelude.hashWithSalt` port

instance Prelude.NFData ProxyConfiguration where
  rnf ProxyConfiguration' {..} =
    Prelude.rnf credentials
      `Prelude.seq` Prelude.rnf host
      `Prelude.seq` Prelude.rnf port

instance Data.ToJSON ProxyConfiguration where
  toJSON ProxyConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Credentials" Data..=) Prelude.<$> credentials,
            Prelude.Just ("Host" Data..= host),
            Prelude.Just ("Port" Data..= port)
          ]
      )
