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
-- Module      : Amazonka.CloudWatchEvents.Types.ConnectionBasicAuthResponseParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.ConnectionBasicAuthResponseParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the authorization parameters for the connection if Basic is
-- specified as the authorization type.
--
-- /See:/ 'newConnectionBasicAuthResponseParameters' smart constructor.
data ConnectionBasicAuthResponseParameters = ConnectionBasicAuthResponseParameters'
  { -- | The user name to use for Basic authorization.
    username :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing
    }

-- | The user name to use for Basic authorization.
connectionBasicAuthResponseParameters_username :: Lens.Lens' ConnectionBasicAuthResponseParameters (Prelude.Maybe Prelude.Text)
connectionBasicAuthResponseParameters_username = Lens.lens (\ConnectionBasicAuthResponseParameters' {username} -> username) (\s@ConnectionBasicAuthResponseParameters' {} a -> s {username = a} :: ConnectionBasicAuthResponseParameters)

instance
  Data.FromJSON
    ConnectionBasicAuthResponseParameters
  where
  parseJSON =
    Data.withObject
      "ConnectionBasicAuthResponseParameters"
      ( \x ->
          ConnectionBasicAuthResponseParameters'
            Prelude.<$> (x Data..:? "Username")
      )

instance
  Prelude.Hashable
    ConnectionBasicAuthResponseParameters
  where
  hashWithSalt
    _salt
    ConnectionBasicAuthResponseParameters' {..} =
      _salt `Prelude.hashWithSalt` username

instance
  Prelude.NFData
    ConnectionBasicAuthResponseParameters
  where
  rnf ConnectionBasicAuthResponseParameters' {..} =
    Prelude.rnf username
