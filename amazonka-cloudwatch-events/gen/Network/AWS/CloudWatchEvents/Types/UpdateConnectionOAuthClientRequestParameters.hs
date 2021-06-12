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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains the OAuth authorization parameters to use for the connection.
--
-- /See:/ 'newUpdateConnectionOAuthClientRequestParameters' smart constructor.
data UpdateConnectionOAuthClientRequestParameters = UpdateConnectionOAuthClientRequestParameters'
  { -- | The client secret assciated with the client ID to use for OAuth
    -- authorization.
    clientSecret :: Core.Maybe Core.Text,
    -- | The client ID to use for OAuth authorization.
    clientID :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      clientID = Core.Nothing
    }

-- | The client secret assciated with the client ID to use for OAuth
-- authorization.
updateConnectionOAuthClientRequestParameters_clientSecret :: Lens.Lens' UpdateConnectionOAuthClientRequestParameters (Core.Maybe Core.Text)
updateConnectionOAuthClientRequestParameters_clientSecret = Lens.lens (\UpdateConnectionOAuthClientRequestParameters' {clientSecret} -> clientSecret) (\s@UpdateConnectionOAuthClientRequestParameters' {} a -> s {clientSecret = a} :: UpdateConnectionOAuthClientRequestParameters)

-- | The client ID to use for OAuth authorization.
updateConnectionOAuthClientRequestParameters_clientID :: Lens.Lens' UpdateConnectionOAuthClientRequestParameters (Core.Maybe Core.Text)
updateConnectionOAuthClientRequestParameters_clientID = Lens.lens (\UpdateConnectionOAuthClientRequestParameters' {clientID} -> clientID) (\s@UpdateConnectionOAuthClientRequestParameters' {} a -> s {clientID = a} :: UpdateConnectionOAuthClientRequestParameters)

instance
  Core.Hashable
    UpdateConnectionOAuthClientRequestParameters

instance
  Core.NFData
    UpdateConnectionOAuthClientRequestParameters

instance
  Core.ToJSON
    UpdateConnectionOAuthClientRequestParameters
  where
  toJSON
    UpdateConnectionOAuthClientRequestParameters' {..} =
      Core.object
        ( Core.catMaybes
            [ ("ClientSecret" Core..=) Core.<$> clientSecret,
              ("ClientID" Core..=) Core.<$> clientID
            ]
        )
