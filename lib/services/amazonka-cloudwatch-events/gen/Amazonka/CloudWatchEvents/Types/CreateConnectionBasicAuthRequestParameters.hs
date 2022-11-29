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
-- Module      : Amazonka.CloudWatchEvents.Types.CreateConnectionBasicAuthRequestParameters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.CreateConnectionBasicAuthRequestParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains the Basic authorization parameters to use for the connection.
--
-- /See:/ 'newCreateConnectionBasicAuthRequestParameters' smart constructor.
data CreateConnectionBasicAuthRequestParameters = CreateConnectionBasicAuthRequestParameters'
  { -- | The user name to use for Basic authorization.
    username :: Prelude.Text,
    -- | The password associated with the user name to use for Basic
    -- authorization.
    password :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConnectionBasicAuthRequestParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'username', 'createConnectionBasicAuthRequestParameters_username' - The user name to use for Basic authorization.
--
-- 'password', 'createConnectionBasicAuthRequestParameters_password' - The password associated with the user name to use for Basic
-- authorization.
newCreateConnectionBasicAuthRequestParameters ::
  -- | 'username'
  Prelude.Text ->
  -- | 'password'
  Prelude.Text ->
  CreateConnectionBasicAuthRequestParameters
newCreateConnectionBasicAuthRequestParameters
  pUsername_
  pPassword_ =
    CreateConnectionBasicAuthRequestParameters'
      { username =
          pUsername_,
        password = pPassword_
      }

-- | The user name to use for Basic authorization.
createConnectionBasicAuthRequestParameters_username :: Lens.Lens' CreateConnectionBasicAuthRequestParameters Prelude.Text
createConnectionBasicAuthRequestParameters_username = Lens.lens (\CreateConnectionBasicAuthRequestParameters' {username} -> username) (\s@CreateConnectionBasicAuthRequestParameters' {} a -> s {username = a} :: CreateConnectionBasicAuthRequestParameters)

-- | The password associated with the user name to use for Basic
-- authorization.
createConnectionBasicAuthRequestParameters_password :: Lens.Lens' CreateConnectionBasicAuthRequestParameters Prelude.Text
createConnectionBasicAuthRequestParameters_password = Lens.lens (\CreateConnectionBasicAuthRequestParameters' {password} -> password) (\s@CreateConnectionBasicAuthRequestParameters' {} a -> s {password = a} :: CreateConnectionBasicAuthRequestParameters)

instance
  Prelude.Hashable
    CreateConnectionBasicAuthRequestParameters
  where
  hashWithSalt
    _salt
    CreateConnectionBasicAuthRequestParameters' {..} =
      _salt `Prelude.hashWithSalt` username
        `Prelude.hashWithSalt` password

instance
  Prelude.NFData
    CreateConnectionBasicAuthRequestParameters
  where
  rnf CreateConnectionBasicAuthRequestParameters' {..} =
    Prelude.rnf username
      `Prelude.seq` Prelude.rnf password

instance
  Core.ToJSON
    CreateConnectionBasicAuthRequestParameters
  where
  toJSON
    CreateConnectionBasicAuthRequestParameters' {..} =
      Core.object
        ( Prelude.catMaybes
            [ Prelude.Just ("Username" Core..= username),
              Prelude.Just ("Password" Core..= password)
            ]
        )
