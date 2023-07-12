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
-- Module      : Amazonka.Pipes.Types.MSKAccessCredentials
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.MSKAccessCredentials where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Secrets Manager secret that stores your stream credentials.
--
-- /See:/ 'newMSKAccessCredentials' smart constructor.
data MSKAccessCredentials = MSKAccessCredentials'
  { -- | The ARN of the Secrets Manager secret.
    clientCertificateTlsAuth :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the Secrets Manager secret.
    saslScram512Auth :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MSKAccessCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientCertificateTlsAuth', 'mSKAccessCredentials_clientCertificateTlsAuth' - The ARN of the Secrets Manager secret.
--
-- 'saslScram512Auth', 'mSKAccessCredentials_saslScram512Auth' - The ARN of the Secrets Manager secret.
newMSKAccessCredentials ::
  MSKAccessCredentials
newMSKAccessCredentials =
  MSKAccessCredentials'
    { clientCertificateTlsAuth =
        Prelude.Nothing,
      saslScram512Auth = Prelude.Nothing
    }

-- | The ARN of the Secrets Manager secret.
mSKAccessCredentials_clientCertificateTlsAuth :: Lens.Lens' MSKAccessCredentials (Prelude.Maybe Prelude.Text)
mSKAccessCredentials_clientCertificateTlsAuth = Lens.lens (\MSKAccessCredentials' {clientCertificateTlsAuth} -> clientCertificateTlsAuth) (\s@MSKAccessCredentials' {} a -> s {clientCertificateTlsAuth = a} :: MSKAccessCredentials)

-- | The ARN of the Secrets Manager secret.
mSKAccessCredentials_saslScram512Auth :: Lens.Lens' MSKAccessCredentials (Prelude.Maybe Prelude.Text)
mSKAccessCredentials_saslScram512Auth = Lens.lens (\MSKAccessCredentials' {saslScram512Auth} -> saslScram512Auth) (\s@MSKAccessCredentials' {} a -> s {saslScram512Auth = a} :: MSKAccessCredentials)

instance Data.FromJSON MSKAccessCredentials where
  parseJSON =
    Data.withObject
      "MSKAccessCredentials"
      ( \x ->
          MSKAccessCredentials'
            Prelude.<$> (x Data..:? "ClientCertificateTlsAuth")
            Prelude.<*> (x Data..:? "SaslScram512Auth")
      )

instance Prelude.Hashable MSKAccessCredentials where
  hashWithSalt _salt MSKAccessCredentials' {..} =
    _salt
      `Prelude.hashWithSalt` clientCertificateTlsAuth
      `Prelude.hashWithSalt` saslScram512Auth

instance Prelude.NFData MSKAccessCredentials where
  rnf MSKAccessCredentials' {..} =
    Prelude.rnf clientCertificateTlsAuth
      `Prelude.seq` Prelude.rnf saslScram512Auth

instance Data.ToJSON MSKAccessCredentials where
  toJSON MSKAccessCredentials' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientCertificateTlsAuth" Data..=)
              Prelude.<$> clientCertificateTlsAuth,
            ("SaslScram512Auth" Data..=)
              Prelude.<$> saslScram512Auth
          ]
      )
