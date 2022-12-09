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
-- Module      : Amazonka.AmplifyBackend.Types.BackendAuthSocialProviderConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyBackend.Types.BackendAuthSocialProviderConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes third-party social federation configurations for allowing your
-- app users to sign in using OAuth.
--
-- /See:/ 'newBackendAuthSocialProviderConfig' smart constructor.
data BackendAuthSocialProviderConfig = BackendAuthSocialProviderConfig'
  { -- | Describes the client_id, which can be obtained from the third-party
    -- social federation provider.
    clientId :: Prelude.Maybe Prelude.Text,
    -- | Describes the client_secret, which can be obtained from third-party
    -- social federation providers.
    clientSecret :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BackendAuthSocialProviderConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientId', 'backendAuthSocialProviderConfig_clientId' - Describes the client_id, which can be obtained from the third-party
-- social federation provider.
--
-- 'clientSecret', 'backendAuthSocialProviderConfig_clientSecret' - Describes the client_secret, which can be obtained from third-party
-- social federation providers.
newBackendAuthSocialProviderConfig ::
  BackendAuthSocialProviderConfig
newBackendAuthSocialProviderConfig =
  BackendAuthSocialProviderConfig'
    { clientId =
        Prelude.Nothing,
      clientSecret = Prelude.Nothing
    }

-- | Describes the client_id, which can be obtained from the third-party
-- social federation provider.
backendAuthSocialProviderConfig_clientId :: Lens.Lens' BackendAuthSocialProviderConfig (Prelude.Maybe Prelude.Text)
backendAuthSocialProviderConfig_clientId = Lens.lens (\BackendAuthSocialProviderConfig' {clientId} -> clientId) (\s@BackendAuthSocialProviderConfig' {} a -> s {clientId = a} :: BackendAuthSocialProviderConfig)

-- | Describes the client_secret, which can be obtained from third-party
-- social federation providers.
backendAuthSocialProviderConfig_clientSecret :: Lens.Lens' BackendAuthSocialProviderConfig (Prelude.Maybe Prelude.Text)
backendAuthSocialProviderConfig_clientSecret = Lens.lens (\BackendAuthSocialProviderConfig' {clientSecret} -> clientSecret) (\s@BackendAuthSocialProviderConfig' {} a -> s {clientSecret = a} :: BackendAuthSocialProviderConfig)

instance
  Data.FromJSON
    BackendAuthSocialProviderConfig
  where
  parseJSON =
    Data.withObject
      "BackendAuthSocialProviderConfig"
      ( \x ->
          BackendAuthSocialProviderConfig'
            Prelude.<$> (x Data..:? "client_id")
            Prelude.<*> (x Data..:? "client_secret")
      )

instance
  Prelude.Hashable
    BackendAuthSocialProviderConfig
  where
  hashWithSalt
    _salt
    BackendAuthSocialProviderConfig' {..} =
      _salt `Prelude.hashWithSalt` clientId
        `Prelude.hashWithSalt` clientSecret

instance
  Prelude.NFData
    BackendAuthSocialProviderConfig
  where
  rnf BackendAuthSocialProviderConfig' {..} =
    Prelude.rnf clientId
      `Prelude.seq` Prelude.rnf clientSecret

instance Data.ToJSON BackendAuthSocialProviderConfig where
  toJSON BackendAuthSocialProviderConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("client_id" Data..=) Prelude.<$> clientId,
            ("client_secret" Data..=) Prelude.<$> clientSecret
          ]
      )
