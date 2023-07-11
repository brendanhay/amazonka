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
-- Module      : Amazonka.CloudWatchEvents.Types.CreateConnectionOAuthClientRequestParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.CreateConnectionOAuthClientRequestParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the Basic authorization parameters to use for the connection.
--
-- /See:/ 'newCreateConnectionOAuthClientRequestParameters' smart constructor.
data CreateConnectionOAuthClientRequestParameters = CreateConnectionOAuthClientRequestParameters'
  { -- | The client ID to use for OAuth authorization for the connection.
    clientID :: Prelude.Text,
    -- | The client secret associated with the client ID to use for OAuth
    -- authorization for the connection.
    clientSecret :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConnectionOAuthClientRequestParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientID', 'createConnectionOAuthClientRequestParameters_clientID' - The client ID to use for OAuth authorization for the connection.
--
-- 'clientSecret', 'createConnectionOAuthClientRequestParameters_clientSecret' - The client secret associated with the client ID to use for OAuth
-- authorization for the connection.
newCreateConnectionOAuthClientRequestParameters ::
  -- | 'clientID'
  Prelude.Text ->
  -- | 'clientSecret'
  Prelude.Text ->
  CreateConnectionOAuthClientRequestParameters
newCreateConnectionOAuthClientRequestParameters
  pClientID_
  pClientSecret_ =
    CreateConnectionOAuthClientRequestParameters'
      { clientID =
          pClientID_,
        clientSecret = pClientSecret_
      }

-- | The client ID to use for OAuth authorization for the connection.
createConnectionOAuthClientRequestParameters_clientID :: Lens.Lens' CreateConnectionOAuthClientRequestParameters Prelude.Text
createConnectionOAuthClientRequestParameters_clientID = Lens.lens (\CreateConnectionOAuthClientRequestParameters' {clientID} -> clientID) (\s@CreateConnectionOAuthClientRequestParameters' {} a -> s {clientID = a} :: CreateConnectionOAuthClientRequestParameters)

-- | The client secret associated with the client ID to use for OAuth
-- authorization for the connection.
createConnectionOAuthClientRequestParameters_clientSecret :: Lens.Lens' CreateConnectionOAuthClientRequestParameters Prelude.Text
createConnectionOAuthClientRequestParameters_clientSecret = Lens.lens (\CreateConnectionOAuthClientRequestParameters' {clientSecret} -> clientSecret) (\s@CreateConnectionOAuthClientRequestParameters' {} a -> s {clientSecret = a} :: CreateConnectionOAuthClientRequestParameters)

instance
  Prelude.Hashable
    CreateConnectionOAuthClientRequestParameters
  where
  hashWithSalt
    _salt
    CreateConnectionOAuthClientRequestParameters' {..} =
      _salt
        `Prelude.hashWithSalt` clientID
        `Prelude.hashWithSalt` clientSecret

instance
  Prelude.NFData
    CreateConnectionOAuthClientRequestParameters
  where
  rnf CreateConnectionOAuthClientRequestParameters' {..} =
    Prelude.rnf clientID
      `Prelude.seq` Prelude.rnf clientSecret

instance
  Data.ToJSON
    CreateConnectionOAuthClientRequestParameters
  where
  toJSON
    CreateConnectionOAuthClientRequestParameters' {..} =
      Data.object
        ( Prelude.catMaybes
            [ Prelude.Just ("ClientID" Data..= clientID),
              Prelude.Just ("ClientSecret" Data..= clientSecret)
            ]
        )
