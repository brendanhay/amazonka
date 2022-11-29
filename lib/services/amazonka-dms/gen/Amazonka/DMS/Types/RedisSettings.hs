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
-- Module      : Amazonka.DMS.Types.RedisSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.RedisSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types.RedisAuthTypeValue
import Amazonka.DMS.Types.SslSecurityProtocolValue
import qualified Amazonka.Prelude as Prelude

-- | Provides information that defines a Redis target endpoint.
--
-- /See:/ 'newRedisSettings' smart constructor.
data RedisSettings = RedisSettings'
  { -- | The user name provided with the @auth-role@ option of the @AuthType@
    -- setting for a Redis target endpoint.
    authUserName :: Prelude.Maybe Prelude.Text,
    -- | The connection to a Redis target endpoint using Transport Layer Security
    -- (TLS). Valid values include @plaintext@ and @ssl-encryption@. The
    -- default is @ssl-encryption@. The @ssl-encryption@ option makes an
    -- encrypted connection. Optionally, you can identify an Amazon Resource
    -- Name (ARN) for an SSL certificate authority (CA) using the
    -- @SslCaCertificateArn @setting. If an ARN isn\'t given for a CA, DMS uses
    -- the Amazon root CA.
    --
    -- The @plaintext@ option doesn\'t provide Transport Layer Security (TLS)
    -- encryption for traffic between endpoint and database.
    sslSecurityProtocol :: Prelude.Maybe SslSecurityProtocolValue,
    -- | The password provided with the @auth-role@ and @auth-token@ options of
    -- the @AuthType@ setting for a Redis target endpoint.
    authPassword :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The Amazon Resource Name (ARN) for the certificate authority (CA) that
    -- DMS uses to connect to your Redis target endpoint.
    sslCaCertificateArn :: Prelude.Maybe Prelude.Text,
    -- | The type of authentication to perform when connecting to a Redis target.
    -- Options include @none@, @auth-token@, and @auth-role@. The @auth-token@
    -- option requires an @AuthPassword@ value to be provided. The @auth-role@
    -- option requires @AuthUserName@ and @AuthPassword@ values to be provided.
    authType :: Prelude.Maybe RedisAuthTypeValue,
    -- | Fully qualified domain name of the endpoint.
    serverName :: Prelude.Text,
    -- | Transmission Control Protocol (TCP) port for the endpoint.
    port :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RedisSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authUserName', 'redisSettings_authUserName' - The user name provided with the @auth-role@ option of the @AuthType@
-- setting for a Redis target endpoint.
--
-- 'sslSecurityProtocol', 'redisSettings_sslSecurityProtocol' - The connection to a Redis target endpoint using Transport Layer Security
-- (TLS). Valid values include @plaintext@ and @ssl-encryption@. The
-- default is @ssl-encryption@. The @ssl-encryption@ option makes an
-- encrypted connection. Optionally, you can identify an Amazon Resource
-- Name (ARN) for an SSL certificate authority (CA) using the
-- @SslCaCertificateArn @setting. If an ARN isn\'t given for a CA, DMS uses
-- the Amazon root CA.
--
-- The @plaintext@ option doesn\'t provide Transport Layer Security (TLS)
-- encryption for traffic between endpoint and database.
--
-- 'authPassword', 'redisSettings_authPassword' - The password provided with the @auth-role@ and @auth-token@ options of
-- the @AuthType@ setting for a Redis target endpoint.
--
-- 'sslCaCertificateArn', 'redisSettings_sslCaCertificateArn' - The Amazon Resource Name (ARN) for the certificate authority (CA) that
-- DMS uses to connect to your Redis target endpoint.
--
-- 'authType', 'redisSettings_authType' - The type of authentication to perform when connecting to a Redis target.
-- Options include @none@, @auth-token@, and @auth-role@. The @auth-token@
-- option requires an @AuthPassword@ value to be provided. The @auth-role@
-- option requires @AuthUserName@ and @AuthPassword@ values to be provided.
--
-- 'serverName', 'redisSettings_serverName' - Fully qualified domain name of the endpoint.
--
-- 'port', 'redisSettings_port' - Transmission Control Protocol (TCP) port for the endpoint.
newRedisSettings ::
  -- | 'serverName'
  Prelude.Text ->
  -- | 'port'
  Prelude.Int ->
  RedisSettings
newRedisSettings pServerName_ pPort_ =
  RedisSettings'
    { authUserName = Prelude.Nothing,
      sslSecurityProtocol = Prelude.Nothing,
      authPassword = Prelude.Nothing,
      sslCaCertificateArn = Prelude.Nothing,
      authType = Prelude.Nothing,
      serverName = pServerName_,
      port = pPort_
    }

-- | The user name provided with the @auth-role@ option of the @AuthType@
-- setting for a Redis target endpoint.
redisSettings_authUserName :: Lens.Lens' RedisSettings (Prelude.Maybe Prelude.Text)
redisSettings_authUserName = Lens.lens (\RedisSettings' {authUserName} -> authUserName) (\s@RedisSettings' {} a -> s {authUserName = a} :: RedisSettings)

-- | The connection to a Redis target endpoint using Transport Layer Security
-- (TLS). Valid values include @plaintext@ and @ssl-encryption@. The
-- default is @ssl-encryption@. The @ssl-encryption@ option makes an
-- encrypted connection. Optionally, you can identify an Amazon Resource
-- Name (ARN) for an SSL certificate authority (CA) using the
-- @SslCaCertificateArn @setting. If an ARN isn\'t given for a CA, DMS uses
-- the Amazon root CA.
--
-- The @plaintext@ option doesn\'t provide Transport Layer Security (TLS)
-- encryption for traffic between endpoint and database.
redisSettings_sslSecurityProtocol :: Lens.Lens' RedisSettings (Prelude.Maybe SslSecurityProtocolValue)
redisSettings_sslSecurityProtocol = Lens.lens (\RedisSettings' {sslSecurityProtocol} -> sslSecurityProtocol) (\s@RedisSettings' {} a -> s {sslSecurityProtocol = a} :: RedisSettings)

-- | The password provided with the @auth-role@ and @auth-token@ options of
-- the @AuthType@ setting for a Redis target endpoint.
redisSettings_authPassword :: Lens.Lens' RedisSettings (Prelude.Maybe Prelude.Text)
redisSettings_authPassword = Lens.lens (\RedisSettings' {authPassword} -> authPassword) (\s@RedisSettings' {} a -> s {authPassword = a} :: RedisSettings) Prelude.. Lens.mapping Core._Sensitive

-- | The Amazon Resource Name (ARN) for the certificate authority (CA) that
-- DMS uses to connect to your Redis target endpoint.
redisSettings_sslCaCertificateArn :: Lens.Lens' RedisSettings (Prelude.Maybe Prelude.Text)
redisSettings_sslCaCertificateArn = Lens.lens (\RedisSettings' {sslCaCertificateArn} -> sslCaCertificateArn) (\s@RedisSettings' {} a -> s {sslCaCertificateArn = a} :: RedisSettings)

-- | The type of authentication to perform when connecting to a Redis target.
-- Options include @none@, @auth-token@, and @auth-role@. The @auth-token@
-- option requires an @AuthPassword@ value to be provided. The @auth-role@
-- option requires @AuthUserName@ and @AuthPassword@ values to be provided.
redisSettings_authType :: Lens.Lens' RedisSettings (Prelude.Maybe RedisAuthTypeValue)
redisSettings_authType = Lens.lens (\RedisSettings' {authType} -> authType) (\s@RedisSettings' {} a -> s {authType = a} :: RedisSettings)

-- | Fully qualified domain name of the endpoint.
redisSettings_serverName :: Lens.Lens' RedisSettings Prelude.Text
redisSettings_serverName = Lens.lens (\RedisSettings' {serverName} -> serverName) (\s@RedisSettings' {} a -> s {serverName = a} :: RedisSettings)

-- | Transmission Control Protocol (TCP) port for the endpoint.
redisSettings_port :: Lens.Lens' RedisSettings Prelude.Int
redisSettings_port = Lens.lens (\RedisSettings' {port} -> port) (\s@RedisSettings' {} a -> s {port = a} :: RedisSettings)

instance Core.FromJSON RedisSettings where
  parseJSON =
    Core.withObject
      "RedisSettings"
      ( \x ->
          RedisSettings'
            Prelude.<$> (x Core..:? "AuthUserName")
            Prelude.<*> (x Core..:? "SslSecurityProtocol")
            Prelude.<*> (x Core..:? "AuthPassword")
            Prelude.<*> (x Core..:? "SslCaCertificateArn")
            Prelude.<*> (x Core..:? "AuthType")
            Prelude.<*> (x Core..: "ServerName")
            Prelude.<*> (x Core..: "Port")
      )

instance Prelude.Hashable RedisSettings where
  hashWithSalt _salt RedisSettings' {..} =
    _salt `Prelude.hashWithSalt` authUserName
      `Prelude.hashWithSalt` sslSecurityProtocol
      `Prelude.hashWithSalt` authPassword
      `Prelude.hashWithSalt` sslCaCertificateArn
      `Prelude.hashWithSalt` authType
      `Prelude.hashWithSalt` serverName
      `Prelude.hashWithSalt` port

instance Prelude.NFData RedisSettings where
  rnf RedisSettings' {..} =
    Prelude.rnf authUserName
      `Prelude.seq` Prelude.rnf sslSecurityProtocol
      `Prelude.seq` Prelude.rnf authPassword
      `Prelude.seq` Prelude.rnf sslCaCertificateArn
      `Prelude.seq` Prelude.rnf authType
      `Prelude.seq` Prelude.rnf serverName
      `Prelude.seq` Prelude.rnf port

instance Core.ToJSON RedisSettings where
  toJSON RedisSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AuthUserName" Core..=) Prelude.<$> authUserName,
            ("SslSecurityProtocol" Core..=)
              Prelude.<$> sslSecurityProtocol,
            ("AuthPassword" Core..=) Prelude.<$> authPassword,
            ("SslCaCertificateArn" Core..=)
              Prelude.<$> sslCaCertificateArn,
            ("AuthType" Core..=) Prelude.<$> authType,
            Prelude.Just ("ServerName" Core..= serverName),
            Prelude.Just ("Port" Core..= port)
          ]
      )
