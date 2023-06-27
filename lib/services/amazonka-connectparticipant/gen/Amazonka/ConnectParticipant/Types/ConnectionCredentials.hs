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
-- Module      : Amazonka.ConnectParticipant.Types.ConnectionCredentials
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectParticipant.Types.ConnectionCredentials where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Connection credentials.
--
-- /See:/ 'newConnectionCredentials' smart constructor.
data ConnectionCredentials = ConnectionCredentials'
  { -- | The connection token.
    connectionToken :: Prelude.Maybe Prelude.Text,
    -- | The expiration of the token.
    --
    -- It\'s specified in ISO 8601 format: yyyy-MM-ddThh:mm:ss.SSSZ. For
    -- example, 2019-11-08T02:41:28.172Z.
    expiry :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectionCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionToken', 'connectionCredentials_connectionToken' - The connection token.
--
-- 'expiry', 'connectionCredentials_expiry' - The expiration of the token.
--
-- It\'s specified in ISO 8601 format: yyyy-MM-ddThh:mm:ss.SSSZ. For
-- example, 2019-11-08T02:41:28.172Z.
newConnectionCredentials ::
  ConnectionCredentials
newConnectionCredentials =
  ConnectionCredentials'
    { connectionToken =
        Prelude.Nothing,
      expiry = Prelude.Nothing
    }

-- | The connection token.
connectionCredentials_connectionToken :: Lens.Lens' ConnectionCredentials (Prelude.Maybe Prelude.Text)
connectionCredentials_connectionToken = Lens.lens (\ConnectionCredentials' {connectionToken} -> connectionToken) (\s@ConnectionCredentials' {} a -> s {connectionToken = a} :: ConnectionCredentials)

-- | The expiration of the token.
--
-- It\'s specified in ISO 8601 format: yyyy-MM-ddThh:mm:ss.SSSZ. For
-- example, 2019-11-08T02:41:28.172Z.
connectionCredentials_expiry :: Lens.Lens' ConnectionCredentials (Prelude.Maybe Prelude.Text)
connectionCredentials_expiry = Lens.lens (\ConnectionCredentials' {expiry} -> expiry) (\s@ConnectionCredentials' {} a -> s {expiry = a} :: ConnectionCredentials)

instance Data.FromJSON ConnectionCredentials where
  parseJSON =
    Data.withObject
      "ConnectionCredentials"
      ( \x ->
          ConnectionCredentials'
            Prelude.<$> (x Data..:? "ConnectionToken")
            Prelude.<*> (x Data..:? "Expiry")
      )

instance Prelude.Hashable ConnectionCredentials where
  hashWithSalt _salt ConnectionCredentials' {..} =
    _salt
      `Prelude.hashWithSalt` connectionToken
      `Prelude.hashWithSalt` expiry

instance Prelude.NFData ConnectionCredentials where
  rnf ConnectionCredentials' {..} =
    Prelude.rnf connectionToken
      `Prelude.seq` Prelude.rnf expiry
