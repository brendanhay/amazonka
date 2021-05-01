{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudWatchEvents.Types.CreateConnectionOAuthClientRequestParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.CreateConnectionOAuthClientRequestParameters where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance
  Prelude.NFData
    CreateConnectionOAuthClientRequestParameters

instance
  Prelude.ToJSON
    CreateConnectionOAuthClientRequestParameters
  where
  toJSON
    CreateConnectionOAuthClientRequestParameters' {..} =
      Prelude.object
        ( Prelude.catMaybes
            [ Prelude.Just ("ClientID" Prelude..= clientID),
              Prelude.Just
                ("ClientSecret" Prelude..= clientSecret)
            ]
        )
