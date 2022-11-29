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
-- Module      : Amazonka.AmplifyBackend.Types.BackendAuthAppleProviderConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyBackend.Types.BackendAuthAppleProviderConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes Apple social federation configurations for allowing your app
-- users to sign in using OAuth.
--
-- /See:/ 'newBackendAuthAppleProviderConfig' smart constructor.
data BackendAuthAppleProviderConfig = BackendAuthAppleProviderConfig'
  { -- | Describes the client_id (also called Services ID) that comes from Apple.
    clientId :: Prelude.Maybe Prelude.Text,
    -- | Describes the private_key that comes from Apple.
    privateKey :: Prelude.Maybe Prelude.Text,
    -- | Describes the team_id that comes from Apple.
    teamId :: Prelude.Maybe Prelude.Text,
    -- | Describes the key_id that comes from Apple.
    keyId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BackendAuthAppleProviderConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientId', 'backendAuthAppleProviderConfig_clientId' - Describes the client_id (also called Services ID) that comes from Apple.
--
-- 'privateKey', 'backendAuthAppleProviderConfig_privateKey' - Describes the private_key that comes from Apple.
--
-- 'teamId', 'backendAuthAppleProviderConfig_teamId' - Describes the team_id that comes from Apple.
--
-- 'keyId', 'backendAuthAppleProviderConfig_keyId' - Describes the key_id that comes from Apple.
newBackendAuthAppleProviderConfig ::
  BackendAuthAppleProviderConfig
newBackendAuthAppleProviderConfig =
  BackendAuthAppleProviderConfig'
    { clientId =
        Prelude.Nothing,
      privateKey = Prelude.Nothing,
      teamId = Prelude.Nothing,
      keyId = Prelude.Nothing
    }

-- | Describes the client_id (also called Services ID) that comes from Apple.
backendAuthAppleProviderConfig_clientId :: Lens.Lens' BackendAuthAppleProviderConfig (Prelude.Maybe Prelude.Text)
backendAuthAppleProviderConfig_clientId = Lens.lens (\BackendAuthAppleProviderConfig' {clientId} -> clientId) (\s@BackendAuthAppleProviderConfig' {} a -> s {clientId = a} :: BackendAuthAppleProviderConfig)

-- | Describes the private_key that comes from Apple.
backendAuthAppleProviderConfig_privateKey :: Lens.Lens' BackendAuthAppleProviderConfig (Prelude.Maybe Prelude.Text)
backendAuthAppleProviderConfig_privateKey = Lens.lens (\BackendAuthAppleProviderConfig' {privateKey} -> privateKey) (\s@BackendAuthAppleProviderConfig' {} a -> s {privateKey = a} :: BackendAuthAppleProviderConfig)

-- | Describes the team_id that comes from Apple.
backendAuthAppleProviderConfig_teamId :: Lens.Lens' BackendAuthAppleProviderConfig (Prelude.Maybe Prelude.Text)
backendAuthAppleProviderConfig_teamId = Lens.lens (\BackendAuthAppleProviderConfig' {teamId} -> teamId) (\s@BackendAuthAppleProviderConfig' {} a -> s {teamId = a} :: BackendAuthAppleProviderConfig)

-- | Describes the key_id that comes from Apple.
backendAuthAppleProviderConfig_keyId :: Lens.Lens' BackendAuthAppleProviderConfig (Prelude.Maybe Prelude.Text)
backendAuthAppleProviderConfig_keyId = Lens.lens (\BackendAuthAppleProviderConfig' {keyId} -> keyId) (\s@BackendAuthAppleProviderConfig' {} a -> s {keyId = a} :: BackendAuthAppleProviderConfig)

instance Core.FromJSON BackendAuthAppleProviderConfig where
  parseJSON =
    Core.withObject
      "BackendAuthAppleProviderConfig"
      ( \x ->
          BackendAuthAppleProviderConfig'
            Prelude.<$> (x Core..:? "client_id")
            Prelude.<*> (x Core..:? "private_key")
            Prelude.<*> (x Core..:? "team_id")
            Prelude.<*> (x Core..:? "key_id")
      )

instance
  Prelude.Hashable
    BackendAuthAppleProviderConfig
  where
  hashWithSalt
    _salt
    BackendAuthAppleProviderConfig' {..} =
      _salt `Prelude.hashWithSalt` clientId
        `Prelude.hashWithSalt` privateKey
        `Prelude.hashWithSalt` teamId
        `Prelude.hashWithSalt` keyId

instance
  Prelude.NFData
    BackendAuthAppleProviderConfig
  where
  rnf BackendAuthAppleProviderConfig' {..} =
    Prelude.rnf clientId
      `Prelude.seq` Prelude.rnf privateKey
      `Prelude.seq` Prelude.rnf teamId
      `Prelude.seq` Prelude.rnf keyId

instance Core.ToJSON BackendAuthAppleProviderConfig where
  toJSON BackendAuthAppleProviderConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("client_id" Core..=) Prelude.<$> clientId,
            ("private_key" Core..=) Prelude.<$> privateKey,
            ("team_id" Core..=) Prelude.<$> teamId,
            ("key_id" Core..=) Prelude.<$> keyId
          ]
      )
