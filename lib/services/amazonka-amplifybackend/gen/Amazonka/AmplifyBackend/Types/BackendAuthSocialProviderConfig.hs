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
import qualified Amazonka.Prelude as Prelude

-- | Describes third-party social federation configurations for allowing your
-- app users to sign in using OAuth.
--
-- /See:/ 'newBackendAuthSocialProviderConfig' smart constructor.
data BackendAuthSocialProviderConfig = BackendAuthSocialProviderConfig'
  { -- | Describes the client_secret, which can be obtained from third-party
    -- social federation providers.
    clientSecret :: Prelude.Maybe Prelude.Text,
    -- | Describes the client_id, which can be obtained from the third-party
    -- social federation provider.
    clientId :: Prelude.Maybe Prelude.Text
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
-- 'clientSecret', 'backendAuthSocialProviderConfig_clientSecret' - Describes the client_secret, which can be obtained from third-party
-- social federation providers.
--
-- 'clientId', 'backendAuthSocialProviderConfig_clientId' - Describes the client_id, which can be obtained from the third-party
-- social federation provider.
newBackendAuthSocialProviderConfig ::
  BackendAuthSocialProviderConfig
newBackendAuthSocialProviderConfig =
  BackendAuthSocialProviderConfig'
    { clientSecret =
        Prelude.Nothing,
      clientId = Prelude.Nothing
    }

-- | Describes the client_secret, which can be obtained from third-party
-- social federation providers.
backendAuthSocialProviderConfig_clientSecret :: Lens.Lens' BackendAuthSocialProviderConfig (Prelude.Maybe Prelude.Text)
backendAuthSocialProviderConfig_clientSecret = Lens.lens (\BackendAuthSocialProviderConfig' {clientSecret} -> clientSecret) (\s@BackendAuthSocialProviderConfig' {} a -> s {clientSecret = a} :: BackendAuthSocialProviderConfig)

-- | Describes the client_id, which can be obtained from the third-party
-- social federation provider.
backendAuthSocialProviderConfig_clientId :: Lens.Lens' BackendAuthSocialProviderConfig (Prelude.Maybe Prelude.Text)
backendAuthSocialProviderConfig_clientId = Lens.lens (\BackendAuthSocialProviderConfig' {clientId} -> clientId) (\s@BackendAuthSocialProviderConfig' {} a -> s {clientId = a} :: BackendAuthSocialProviderConfig)

instance
  Core.FromJSON
    BackendAuthSocialProviderConfig
  where
  parseJSON =
    Core.withObject
      "BackendAuthSocialProviderConfig"
      ( \x ->
          BackendAuthSocialProviderConfig'
            Prelude.<$> (x Core..:? "client_secret")
            Prelude.<*> (x Core..:? "client_id")
      )

instance
  Prelude.Hashable
    BackendAuthSocialProviderConfig
  where
  hashWithSalt
    _salt
    BackendAuthSocialProviderConfig' {..} =
      _salt `Prelude.hashWithSalt` clientSecret
        `Prelude.hashWithSalt` clientId

instance
  Prelude.NFData
    BackendAuthSocialProviderConfig
  where
  rnf BackendAuthSocialProviderConfig' {..} =
    Prelude.rnf clientSecret
      `Prelude.seq` Prelude.rnf clientId

instance Core.ToJSON BackendAuthSocialProviderConfig where
  toJSON BackendAuthSocialProviderConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("client_secret" Core..=) Prelude.<$> clientSecret,
            ("client_id" Core..=) Prelude.<$> clientId
          ]
      )
