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
-- Module      : Amazonka.Kendra.Types.BasicAuthenticationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.BasicAuthenticationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides the configuration information to connect to websites that
-- require basic user authentication.
--
-- /See:/ 'newBasicAuthenticationConfiguration' smart constructor.
data BasicAuthenticationConfiguration = BasicAuthenticationConfiguration'
  { -- | The name of the website host you want to connect to using authentication
    -- credentials.
    --
    -- For example, the host name of https:\/\/a.example.com\/page1.html is
    -- \"a.example.com\".
    host :: Prelude.Text,
    -- | The port number of the website host you want to connect to using
    -- authentication credentials.
    --
    -- For example, the port for https:\/\/a.example.com\/page1.html is 443,
    -- the standard port for HTTPS.
    port :: Prelude.Natural,
    -- | Your secret ARN, which you can create in
    -- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/intro.html Secrets Manager>
    --
    -- You use a secret if basic authentication credentials are required to
    -- connect to a website. The secret stores your credentials of user name
    -- and password.
    credentials :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BasicAuthenticationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'host', 'basicAuthenticationConfiguration_host' - The name of the website host you want to connect to using authentication
-- credentials.
--
-- For example, the host name of https:\/\/a.example.com\/page1.html is
-- \"a.example.com\".
--
-- 'port', 'basicAuthenticationConfiguration_port' - The port number of the website host you want to connect to using
-- authentication credentials.
--
-- For example, the port for https:\/\/a.example.com\/page1.html is 443,
-- the standard port for HTTPS.
--
-- 'credentials', 'basicAuthenticationConfiguration_credentials' - Your secret ARN, which you can create in
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/intro.html Secrets Manager>
--
-- You use a secret if basic authentication credentials are required to
-- connect to a website. The secret stores your credentials of user name
-- and password.
newBasicAuthenticationConfiguration ::
  -- | 'host'
  Prelude.Text ->
  -- | 'port'
  Prelude.Natural ->
  -- | 'credentials'
  Prelude.Text ->
  BasicAuthenticationConfiguration
newBasicAuthenticationConfiguration
  pHost_
  pPort_
  pCredentials_ =
    BasicAuthenticationConfiguration'
      { host = pHost_,
        port = pPort_,
        credentials = pCredentials_
      }

-- | The name of the website host you want to connect to using authentication
-- credentials.
--
-- For example, the host name of https:\/\/a.example.com\/page1.html is
-- \"a.example.com\".
basicAuthenticationConfiguration_host :: Lens.Lens' BasicAuthenticationConfiguration Prelude.Text
basicAuthenticationConfiguration_host = Lens.lens (\BasicAuthenticationConfiguration' {host} -> host) (\s@BasicAuthenticationConfiguration' {} a -> s {host = a} :: BasicAuthenticationConfiguration)

-- | The port number of the website host you want to connect to using
-- authentication credentials.
--
-- For example, the port for https:\/\/a.example.com\/page1.html is 443,
-- the standard port for HTTPS.
basicAuthenticationConfiguration_port :: Lens.Lens' BasicAuthenticationConfiguration Prelude.Natural
basicAuthenticationConfiguration_port = Lens.lens (\BasicAuthenticationConfiguration' {port} -> port) (\s@BasicAuthenticationConfiguration' {} a -> s {port = a} :: BasicAuthenticationConfiguration)

-- | Your secret ARN, which you can create in
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/intro.html Secrets Manager>
--
-- You use a secret if basic authentication credentials are required to
-- connect to a website. The secret stores your credentials of user name
-- and password.
basicAuthenticationConfiguration_credentials :: Lens.Lens' BasicAuthenticationConfiguration Prelude.Text
basicAuthenticationConfiguration_credentials = Lens.lens (\BasicAuthenticationConfiguration' {credentials} -> credentials) (\s@BasicAuthenticationConfiguration' {} a -> s {credentials = a} :: BasicAuthenticationConfiguration)

instance
  Data.FromJSON
    BasicAuthenticationConfiguration
  where
  parseJSON =
    Data.withObject
      "BasicAuthenticationConfiguration"
      ( \x ->
          BasicAuthenticationConfiguration'
            Prelude.<$> (x Data..: "Host")
            Prelude.<*> (x Data..: "Port")
            Prelude.<*> (x Data..: "Credentials")
      )

instance
  Prelude.Hashable
    BasicAuthenticationConfiguration
  where
  hashWithSalt
    _salt
    BasicAuthenticationConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` host
        `Prelude.hashWithSalt` port
        `Prelude.hashWithSalt` credentials

instance
  Prelude.NFData
    BasicAuthenticationConfiguration
  where
  rnf BasicAuthenticationConfiguration' {..} =
    Prelude.rnf host
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf credentials

instance Data.ToJSON BasicAuthenticationConfiguration where
  toJSON BasicAuthenticationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Host" Data..= host),
            Prelude.Just ("Port" Data..= port),
            Prelude.Just ("Credentials" Data..= credentials)
          ]
      )
