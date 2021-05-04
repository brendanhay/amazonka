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
-- Module      : Network.AWS.CloudWatchEvents.Types.CreateConnectionBasicAuthRequestParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.CreateConnectionBasicAuthRequestParameters where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance
  Prelude.NFData
    CreateConnectionBasicAuthRequestParameters

instance
  Prelude.ToJSON
    CreateConnectionBasicAuthRequestParameters
  where
  toJSON
    CreateConnectionBasicAuthRequestParameters' {..} =
      Prelude.object
        ( Prelude.catMaybes
            [ Prelude.Just ("Username" Prelude..= username),
              Prelude.Just ("Password" Prelude..= password)
            ]
        )
