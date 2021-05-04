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
-- Module      : Network.AWS.CloudWatchEvents.Types.UpdateConnectionOAuthClientRequestParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.UpdateConnectionOAuthClientRequestParameters where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains the OAuth authorization parameters to use for the connection.
--
-- /See:/ 'newUpdateConnectionOAuthClientRequestParameters' smart constructor.
data UpdateConnectionOAuthClientRequestParameters = UpdateConnectionOAuthClientRequestParameters'
  { -- | The client secret assciated with the client ID to use for OAuth
    -- authorization.
    clientSecret :: Prelude.Maybe Prelude.Text,
    -- | The client ID to use for OAuth authorization.
    clientID :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateConnectionOAuthClientRequestParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientSecret', 'updateConnectionOAuthClientRequestParameters_clientSecret' - The client secret assciated with the client ID to use for OAuth
-- authorization.
--
-- 'clientID', 'updateConnectionOAuthClientRequestParameters_clientID' - The client ID to use for OAuth authorization.
newUpdateConnectionOAuthClientRequestParameters ::
  UpdateConnectionOAuthClientRequestParameters
newUpdateConnectionOAuthClientRequestParameters =
  UpdateConnectionOAuthClientRequestParameters'
    { clientSecret =
        Prelude.Nothing,
      clientID = Prelude.Nothing
    }

-- | The client secret assciated with the client ID to use for OAuth
-- authorization.
updateConnectionOAuthClientRequestParameters_clientSecret :: Lens.Lens' UpdateConnectionOAuthClientRequestParameters (Prelude.Maybe Prelude.Text)
updateConnectionOAuthClientRequestParameters_clientSecret = Lens.lens (\UpdateConnectionOAuthClientRequestParameters' {clientSecret} -> clientSecret) (\s@UpdateConnectionOAuthClientRequestParameters' {} a -> s {clientSecret = a} :: UpdateConnectionOAuthClientRequestParameters)

-- | The client ID to use for OAuth authorization.
updateConnectionOAuthClientRequestParameters_clientID :: Lens.Lens' UpdateConnectionOAuthClientRequestParameters (Prelude.Maybe Prelude.Text)
updateConnectionOAuthClientRequestParameters_clientID = Lens.lens (\UpdateConnectionOAuthClientRequestParameters' {clientID} -> clientID) (\s@UpdateConnectionOAuthClientRequestParameters' {} a -> s {clientID = a} :: UpdateConnectionOAuthClientRequestParameters)

instance
  Prelude.Hashable
    UpdateConnectionOAuthClientRequestParameters

instance
  Prelude.NFData
    UpdateConnectionOAuthClientRequestParameters

instance
  Prelude.ToJSON
    UpdateConnectionOAuthClientRequestParameters
  where
  toJSON
    UpdateConnectionOAuthClientRequestParameters' {..} =
      Prelude.object
        ( Prelude.catMaybes
            [ ("ClientSecret" Prelude..=)
                Prelude.<$> clientSecret,
              ("ClientID" Prelude..=) Prelude.<$> clientID
            ]
        )
