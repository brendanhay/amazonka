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
-- Module      : Network.AWS.CloudWatchEvents.Types.ConnectionOAuthClientResponseParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.ConnectionOAuthClientResponseParameters where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains the client response parameters for the connection when OAuth is
-- specified as the authorization type.
--
-- /See:/ 'newConnectionOAuthClientResponseParameters' smart constructor.
data ConnectionOAuthClientResponseParameters = ConnectionOAuthClientResponseParameters'
  { -- | The client ID associated with the response to the connection request.
    clientID :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ConnectionOAuthClientResponseParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientID', 'connectionOAuthClientResponseParameters_clientID' - The client ID associated with the response to the connection request.
newConnectionOAuthClientResponseParameters ::
  ConnectionOAuthClientResponseParameters
newConnectionOAuthClientResponseParameters =
  ConnectionOAuthClientResponseParameters'
    { clientID =
        Prelude.Nothing
    }

-- | The client ID associated with the response to the connection request.
connectionOAuthClientResponseParameters_clientID :: Lens.Lens' ConnectionOAuthClientResponseParameters (Prelude.Maybe Prelude.Text)
connectionOAuthClientResponseParameters_clientID = Lens.lens (\ConnectionOAuthClientResponseParameters' {clientID} -> clientID) (\s@ConnectionOAuthClientResponseParameters' {} a -> s {clientID = a} :: ConnectionOAuthClientResponseParameters)

instance
  Prelude.FromJSON
    ConnectionOAuthClientResponseParameters
  where
  parseJSON =
    Prelude.withObject
      "ConnectionOAuthClientResponseParameters"
      ( \x ->
          ConnectionOAuthClientResponseParameters'
            Prelude.<$> (x Prelude..:? "ClientID")
      )

instance
  Prelude.Hashable
    ConnectionOAuthClientResponseParameters

instance
  Prelude.NFData
    ConnectionOAuthClientResponseParameters
