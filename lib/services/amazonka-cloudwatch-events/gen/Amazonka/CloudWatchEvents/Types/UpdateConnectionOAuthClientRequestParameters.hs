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
-- Module      : Amazonka.CloudWatchEvents.Types.UpdateConnectionOAuthClientRequestParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.UpdateConnectionOAuthClientRequestParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the OAuth authorization parameters to use for the connection.
--
-- /See:/ 'newUpdateConnectionOAuthClientRequestParameters' smart constructor.
data UpdateConnectionOAuthClientRequestParameters = UpdateConnectionOAuthClientRequestParameters'
  { -- | The client ID to use for OAuth authorization.
    clientID :: Prelude.Maybe Prelude.Text,
    -- | The client secret assciated with the client ID to use for OAuth
    -- authorization.
    clientSecret :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateConnectionOAuthClientRequestParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientID', 'updateConnectionOAuthClientRequestParameters_clientID' - The client ID to use for OAuth authorization.
--
-- 'clientSecret', 'updateConnectionOAuthClientRequestParameters_clientSecret' - The client secret assciated with the client ID to use for OAuth
-- authorization.
newUpdateConnectionOAuthClientRequestParameters ::
  UpdateConnectionOAuthClientRequestParameters
newUpdateConnectionOAuthClientRequestParameters =
  UpdateConnectionOAuthClientRequestParameters'
    { clientID =
        Prelude.Nothing,
      clientSecret =
        Prelude.Nothing
    }

-- | The client ID to use for OAuth authorization.
updateConnectionOAuthClientRequestParameters_clientID :: Lens.Lens' UpdateConnectionOAuthClientRequestParameters (Prelude.Maybe Prelude.Text)
updateConnectionOAuthClientRequestParameters_clientID = Lens.lens (\UpdateConnectionOAuthClientRequestParameters' {clientID} -> clientID) (\s@UpdateConnectionOAuthClientRequestParameters' {} a -> s {clientID = a} :: UpdateConnectionOAuthClientRequestParameters)

-- | The client secret assciated with the client ID to use for OAuth
-- authorization.
updateConnectionOAuthClientRequestParameters_clientSecret :: Lens.Lens' UpdateConnectionOAuthClientRequestParameters (Prelude.Maybe Prelude.Text)
updateConnectionOAuthClientRequestParameters_clientSecret = Lens.lens (\UpdateConnectionOAuthClientRequestParameters' {clientSecret} -> clientSecret) (\s@UpdateConnectionOAuthClientRequestParameters' {} a -> s {clientSecret = a} :: UpdateConnectionOAuthClientRequestParameters)

instance
  Prelude.Hashable
    UpdateConnectionOAuthClientRequestParameters
  where
  hashWithSalt
    _salt
    UpdateConnectionOAuthClientRequestParameters' {..} =
      _salt `Prelude.hashWithSalt` clientID
        `Prelude.hashWithSalt` clientSecret

instance
  Prelude.NFData
    UpdateConnectionOAuthClientRequestParameters
  where
  rnf UpdateConnectionOAuthClientRequestParameters' {..} =
    Prelude.rnf clientID
      `Prelude.seq` Prelude.rnf clientSecret

instance
  Data.ToJSON
    UpdateConnectionOAuthClientRequestParameters
  where
  toJSON
    UpdateConnectionOAuthClientRequestParameters' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("ClientID" Data..=) Prelude.<$> clientID,
              ("ClientSecret" Data..=) Prelude.<$> clientSecret
            ]
        )
