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
-- Module      : Amazonka.FinSpaceData.Types.Credentials
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpaceData.Types.Credentials where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Short term API credentials.
--
-- /See:/ 'newCredentials' smart constructor.
data Credentials = Credentials'
  { -- | The session token.
    sessionToken :: Prelude.Maybe Prelude.Text,
    -- | The access key.
    secretAccessKey :: Prelude.Maybe Prelude.Text,
    -- | The access key identifier.
    accessKeyId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Credentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionToken', 'credentials_sessionToken' - The session token.
--
-- 'secretAccessKey', 'credentials_secretAccessKey' - The access key.
--
-- 'accessKeyId', 'credentials_accessKeyId' - The access key identifier.
newCredentials ::
  Credentials
newCredentials =
  Credentials'
    { sessionToken = Prelude.Nothing,
      secretAccessKey = Prelude.Nothing,
      accessKeyId = Prelude.Nothing
    }

-- | The session token.
credentials_sessionToken :: Lens.Lens' Credentials (Prelude.Maybe Prelude.Text)
credentials_sessionToken = Lens.lens (\Credentials' {sessionToken} -> sessionToken) (\s@Credentials' {} a -> s {sessionToken = a} :: Credentials)

-- | The access key.
credentials_secretAccessKey :: Lens.Lens' Credentials (Prelude.Maybe Prelude.Text)
credentials_secretAccessKey = Lens.lens (\Credentials' {secretAccessKey} -> secretAccessKey) (\s@Credentials' {} a -> s {secretAccessKey = a} :: Credentials)

-- | The access key identifier.
credentials_accessKeyId :: Lens.Lens' Credentials (Prelude.Maybe Prelude.Text)
credentials_accessKeyId = Lens.lens (\Credentials' {accessKeyId} -> accessKeyId) (\s@Credentials' {} a -> s {accessKeyId = a} :: Credentials)

instance Core.FromJSON Credentials where
  parseJSON =
    Core.withObject
      "Credentials"
      ( \x ->
          Credentials'
            Prelude.<$> (x Core..:? "sessionToken")
            Prelude.<*> (x Core..:? "secretAccessKey")
            Prelude.<*> (x Core..:? "accessKeyId")
      )

instance Prelude.Hashable Credentials where
  hashWithSalt _salt Credentials' {..} =
    _salt `Prelude.hashWithSalt` sessionToken
      `Prelude.hashWithSalt` secretAccessKey
      `Prelude.hashWithSalt` accessKeyId

instance Prelude.NFData Credentials where
  rnf Credentials' {..} =
    Prelude.rnf sessionToken
      `Prelude.seq` Prelude.rnf secretAccessKey
      `Prelude.seq` Prelude.rnf accessKeyId
