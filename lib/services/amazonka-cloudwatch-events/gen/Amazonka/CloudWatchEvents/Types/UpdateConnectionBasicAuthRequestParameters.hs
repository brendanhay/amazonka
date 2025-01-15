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
-- Module      : Amazonka.CloudWatchEvents.Types.UpdateConnectionBasicAuthRequestParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.UpdateConnectionBasicAuthRequestParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the Basic authorization parameters for the connection.
--
-- /See:/ 'newUpdateConnectionBasicAuthRequestParameters' smart constructor.
data UpdateConnectionBasicAuthRequestParameters = UpdateConnectionBasicAuthRequestParameters'
  { -- | The password associated with the user name to use for Basic
    -- authorization.
    password :: Prelude.Maybe Prelude.Text,
    -- | The user name to use for Basic authorization.
    username :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateConnectionBasicAuthRequestParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'password', 'updateConnectionBasicAuthRequestParameters_password' - The password associated with the user name to use for Basic
-- authorization.
--
-- 'username', 'updateConnectionBasicAuthRequestParameters_username' - The user name to use for Basic authorization.
newUpdateConnectionBasicAuthRequestParameters ::
  UpdateConnectionBasicAuthRequestParameters
newUpdateConnectionBasicAuthRequestParameters =
  UpdateConnectionBasicAuthRequestParameters'
    { password =
        Prelude.Nothing,
      username = Prelude.Nothing
    }

-- | The password associated with the user name to use for Basic
-- authorization.
updateConnectionBasicAuthRequestParameters_password :: Lens.Lens' UpdateConnectionBasicAuthRequestParameters (Prelude.Maybe Prelude.Text)
updateConnectionBasicAuthRequestParameters_password = Lens.lens (\UpdateConnectionBasicAuthRequestParameters' {password} -> password) (\s@UpdateConnectionBasicAuthRequestParameters' {} a -> s {password = a} :: UpdateConnectionBasicAuthRequestParameters)

-- | The user name to use for Basic authorization.
updateConnectionBasicAuthRequestParameters_username :: Lens.Lens' UpdateConnectionBasicAuthRequestParameters (Prelude.Maybe Prelude.Text)
updateConnectionBasicAuthRequestParameters_username = Lens.lens (\UpdateConnectionBasicAuthRequestParameters' {username} -> username) (\s@UpdateConnectionBasicAuthRequestParameters' {} a -> s {username = a} :: UpdateConnectionBasicAuthRequestParameters)

instance
  Prelude.Hashable
    UpdateConnectionBasicAuthRequestParameters
  where
  hashWithSalt
    _salt
    UpdateConnectionBasicAuthRequestParameters' {..} =
      _salt
        `Prelude.hashWithSalt` password
        `Prelude.hashWithSalt` username

instance
  Prelude.NFData
    UpdateConnectionBasicAuthRequestParameters
  where
  rnf UpdateConnectionBasicAuthRequestParameters' {..} =
    Prelude.rnf password `Prelude.seq`
      Prelude.rnf username

instance
  Data.ToJSON
    UpdateConnectionBasicAuthRequestParameters
  where
  toJSON
    UpdateConnectionBasicAuthRequestParameters' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Password" Data..=) Prelude.<$> password,
              ("Username" Data..=) Prelude.<$> username
            ]
        )
