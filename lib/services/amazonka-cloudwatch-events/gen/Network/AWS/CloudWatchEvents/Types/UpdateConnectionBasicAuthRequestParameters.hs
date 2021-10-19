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
-- Module      : Network.AWS.CloudWatchEvents.Types.UpdateConnectionBasicAuthRequestParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.UpdateConnectionBasicAuthRequestParameters where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains the Basic authorization parameters for the connection.
--
-- /See:/ 'newUpdateConnectionBasicAuthRequestParameters' smart constructor.
data UpdateConnectionBasicAuthRequestParameters = UpdateConnectionBasicAuthRequestParameters'
  { -- | The user name to use for Basic authorization.
    username :: Prelude.Maybe Prelude.Text,
    -- | The password associated with the user name to use for Basic
    -- authorization.
    password :: Prelude.Maybe Prelude.Text
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
-- 'username', 'updateConnectionBasicAuthRequestParameters_username' - The user name to use for Basic authorization.
--
-- 'password', 'updateConnectionBasicAuthRequestParameters_password' - The password associated with the user name to use for Basic
-- authorization.
newUpdateConnectionBasicAuthRequestParameters ::
  UpdateConnectionBasicAuthRequestParameters
newUpdateConnectionBasicAuthRequestParameters =
  UpdateConnectionBasicAuthRequestParameters'
    { username =
        Prelude.Nothing,
      password = Prelude.Nothing
    }

-- | The user name to use for Basic authorization.
updateConnectionBasicAuthRequestParameters_username :: Lens.Lens' UpdateConnectionBasicAuthRequestParameters (Prelude.Maybe Prelude.Text)
updateConnectionBasicAuthRequestParameters_username = Lens.lens (\UpdateConnectionBasicAuthRequestParameters' {username} -> username) (\s@UpdateConnectionBasicAuthRequestParameters' {} a -> s {username = a} :: UpdateConnectionBasicAuthRequestParameters)

-- | The password associated with the user name to use for Basic
-- authorization.
updateConnectionBasicAuthRequestParameters_password :: Lens.Lens' UpdateConnectionBasicAuthRequestParameters (Prelude.Maybe Prelude.Text)
updateConnectionBasicAuthRequestParameters_password = Lens.lens (\UpdateConnectionBasicAuthRequestParameters' {password} -> password) (\s@UpdateConnectionBasicAuthRequestParameters' {} a -> s {password = a} :: UpdateConnectionBasicAuthRequestParameters)

instance
  Prelude.Hashable
    UpdateConnectionBasicAuthRequestParameters

instance
  Prelude.NFData
    UpdateConnectionBasicAuthRequestParameters

instance
  Core.ToJSON
    UpdateConnectionBasicAuthRequestParameters
  where
  toJSON
    UpdateConnectionBasicAuthRequestParameters' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("Username" Core..=) Prelude.<$> username,
              ("Password" Core..=) Prelude.<$> password
            ]
        )
