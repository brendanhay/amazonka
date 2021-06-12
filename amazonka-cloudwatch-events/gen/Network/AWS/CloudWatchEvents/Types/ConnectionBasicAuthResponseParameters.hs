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
-- Module      : Network.AWS.CloudWatchEvents.Types.ConnectionBasicAuthResponseParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.ConnectionBasicAuthResponseParameters where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains the authorization parameters for the connection if Basic is
-- specified as the authorization type.
--
-- /See:/ 'newConnectionBasicAuthResponseParameters' smart constructor.
data ConnectionBasicAuthResponseParameters = ConnectionBasicAuthResponseParameters'
  { -- | The user name to use for Basic authorization.
    username :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ConnectionBasicAuthResponseParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'username', 'connectionBasicAuthResponseParameters_username' - The user name to use for Basic authorization.
newConnectionBasicAuthResponseParameters ::
  ConnectionBasicAuthResponseParameters
newConnectionBasicAuthResponseParameters =
  ConnectionBasicAuthResponseParameters'
    { username =
        Core.Nothing
    }

-- | The user name to use for Basic authorization.
connectionBasicAuthResponseParameters_username :: Lens.Lens' ConnectionBasicAuthResponseParameters (Core.Maybe Core.Text)
connectionBasicAuthResponseParameters_username = Lens.lens (\ConnectionBasicAuthResponseParameters' {username} -> username) (\s@ConnectionBasicAuthResponseParameters' {} a -> s {username = a} :: ConnectionBasicAuthResponseParameters)

instance
  Core.FromJSON
    ConnectionBasicAuthResponseParameters
  where
  parseJSON =
    Core.withObject
      "ConnectionBasicAuthResponseParameters"
      ( \x ->
          ConnectionBasicAuthResponseParameters'
            Core.<$> (x Core..:? "Username")
      )

instance
  Core.Hashable
    ConnectionBasicAuthResponseParameters

instance
  Core.NFData
    ConnectionBasicAuthResponseParameters
