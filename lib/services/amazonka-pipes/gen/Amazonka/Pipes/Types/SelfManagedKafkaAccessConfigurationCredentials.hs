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
-- Module      : Amazonka.Pipes.Types.SelfManagedKafkaAccessConfigurationCredentials
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.SelfManagedKafkaAccessConfigurationCredentials where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Secrets Manager secret that stores your stream credentials.
--
-- /See:/ 'newSelfManagedKafkaAccessConfigurationCredentials' smart constructor.
data SelfManagedKafkaAccessConfigurationCredentials = SelfManagedKafkaAccessConfigurationCredentials'
  { -- | The ARN of the Secrets Manager secret.
    basicAuth :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the Secrets Manager secret.
    clientCertificateTlsAuth :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the Secrets Manager secret.
    saslScram256Auth :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the Secrets Manager secret.
    saslScram512Auth :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SelfManagedKafkaAccessConfigurationCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'basicAuth', 'selfManagedKafkaAccessConfigurationCredentials_basicAuth' - The ARN of the Secrets Manager secret.
--
-- 'clientCertificateTlsAuth', 'selfManagedKafkaAccessConfigurationCredentials_clientCertificateTlsAuth' - The ARN of the Secrets Manager secret.
--
-- 'saslScram256Auth', 'selfManagedKafkaAccessConfigurationCredentials_saslScram256Auth' - The ARN of the Secrets Manager secret.
--
-- 'saslScram512Auth', 'selfManagedKafkaAccessConfigurationCredentials_saslScram512Auth' - The ARN of the Secrets Manager secret.
newSelfManagedKafkaAccessConfigurationCredentials ::
  SelfManagedKafkaAccessConfigurationCredentials
newSelfManagedKafkaAccessConfigurationCredentials =
  SelfManagedKafkaAccessConfigurationCredentials'
    { basicAuth =
        Prelude.Nothing,
      clientCertificateTlsAuth =
        Prelude.Nothing,
      saslScram256Auth =
        Prelude.Nothing,
      saslScram512Auth =
        Prelude.Nothing
    }

-- | The ARN of the Secrets Manager secret.
selfManagedKafkaAccessConfigurationCredentials_basicAuth :: Lens.Lens' SelfManagedKafkaAccessConfigurationCredentials (Prelude.Maybe Prelude.Text)
selfManagedKafkaAccessConfigurationCredentials_basicAuth = Lens.lens (\SelfManagedKafkaAccessConfigurationCredentials' {basicAuth} -> basicAuth) (\s@SelfManagedKafkaAccessConfigurationCredentials' {} a -> s {basicAuth = a} :: SelfManagedKafkaAccessConfigurationCredentials)

-- | The ARN of the Secrets Manager secret.
selfManagedKafkaAccessConfigurationCredentials_clientCertificateTlsAuth :: Lens.Lens' SelfManagedKafkaAccessConfigurationCredentials (Prelude.Maybe Prelude.Text)
selfManagedKafkaAccessConfigurationCredentials_clientCertificateTlsAuth = Lens.lens (\SelfManagedKafkaAccessConfigurationCredentials' {clientCertificateTlsAuth} -> clientCertificateTlsAuth) (\s@SelfManagedKafkaAccessConfigurationCredentials' {} a -> s {clientCertificateTlsAuth = a} :: SelfManagedKafkaAccessConfigurationCredentials)

-- | The ARN of the Secrets Manager secret.
selfManagedKafkaAccessConfigurationCredentials_saslScram256Auth :: Lens.Lens' SelfManagedKafkaAccessConfigurationCredentials (Prelude.Maybe Prelude.Text)
selfManagedKafkaAccessConfigurationCredentials_saslScram256Auth = Lens.lens (\SelfManagedKafkaAccessConfigurationCredentials' {saslScram256Auth} -> saslScram256Auth) (\s@SelfManagedKafkaAccessConfigurationCredentials' {} a -> s {saslScram256Auth = a} :: SelfManagedKafkaAccessConfigurationCredentials)

-- | The ARN of the Secrets Manager secret.
selfManagedKafkaAccessConfigurationCredentials_saslScram512Auth :: Lens.Lens' SelfManagedKafkaAccessConfigurationCredentials (Prelude.Maybe Prelude.Text)
selfManagedKafkaAccessConfigurationCredentials_saslScram512Auth = Lens.lens (\SelfManagedKafkaAccessConfigurationCredentials' {saslScram512Auth} -> saslScram512Auth) (\s@SelfManagedKafkaAccessConfigurationCredentials' {} a -> s {saslScram512Auth = a} :: SelfManagedKafkaAccessConfigurationCredentials)

instance
  Data.FromJSON
    SelfManagedKafkaAccessConfigurationCredentials
  where
  parseJSON =
    Data.withObject
      "SelfManagedKafkaAccessConfigurationCredentials"
      ( \x ->
          SelfManagedKafkaAccessConfigurationCredentials'
            Prelude.<$> (x Data..:? "BasicAuth")
            Prelude.<*> (x Data..:? "ClientCertificateTlsAuth")
            Prelude.<*> (x Data..:? "SaslScram256Auth")
            Prelude.<*> (x Data..:? "SaslScram512Auth")
      )

instance
  Prelude.Hashable
    SelfManagedKafkaAccessConfigurationCredentials
  where
  hashWithSalt
    _salt
    SelfManagedKafkaAccessConfigurationCredentials' {..} =
      _salt
        `Prelude.hashWithSalt` basicAuth
        `Prelude.hashWithSalt` clientCertificateTlsAuth
        `Prelude.hashWithSalt` saslScram256Auth
        `Prelude.hashWithSalt` saslScram512Auth

instance
  Prelude.NFData
    SelfManagedKafkaAccessConfigurationCredentials
  where
  rnf
    SelfManagedKafkaAccessConfigurationCredentials' {..} =
      Prelude.rnf basicAuth
        `Prelude.seq` Prelude.rnf clientCertificateTlsAuth
        `Prelude.seq` Prelude.rnf saslScram256Auth
        `Prelude.seq` Prelude.rnf saslScram512Auth

instance
  Data.ToJSON
    SelfManagedKafkaAccessConfigurationCredentials
  where
  toJSON
    SelfManagedKafkaAccessConfigurationCredentials' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("BasicAuth" Data..=) Prelude.<$> basicAuth,
              ("ClientCertificateTlsAuth" Data..=)
                Prelude.<$> clientCertificateTlsAuth,
              ("SaslScram256Auth" Data..=)
                Prelude.<$> saslScram256Auth,
              ("SaslScram512Auth" Data..=)
                Prelude.<$> saslScram512Auth
            ]
        )
