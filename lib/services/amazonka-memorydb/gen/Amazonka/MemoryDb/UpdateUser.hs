{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MemoryDb.UpdateUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes user password(s) and\/or access string.
module Amazonka.MemoryDb.UpdateUser
  ( -- * Creating a Request
    UpdateUser (..),
    newUpdateUser,

    -- * Request Lenses
    updateUser_accessString,
    updateUser_authenticationMode,
    updateUser_userName,

    -- * Destructuring the Response
    UpdateUserResponse (..),
    newUpdateUserResponse,

    -- * Response Lenses
    updateUserResponse_user,
    updateUserResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MemoryDb.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateUser' smart constructor.
data UpdateUser = UpdateUser'
  { -- | Access permissions string used for this user.
    accessString :: Prelude.Maybe Prelude.Text,
    -- | Denotes the user\'s authentication properties, such as whether it
    -- requires a password to authenticate.
    authenticationMode :: Prelude.Maybe AuthenticationMode,
    -- | The name of the user
    userName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessString', 'updateUser_accessString' - Access permissions string used for this user.
--
-- 'authenticationMode', 'updateUser_authenticationMode' - Denotes the user\'s authentication properties, such as whether it
-- requires a password to authenticate.
--
-- 'userName', 'updateUser_userName' - The name of the user
newUpdateUser ::
  -- | 'userName'
  Prelude.Text ->
  UpdateUser
newUpdateUser pUserName_ =
  UpdateUser'
    { accessString = Prelude.Nothing,
      authenticationMode = Prelude.Nothing,
      userName = pUserName_
    }

-- | Access permissions string used for this user.
updateUser_accessString :: Lens.Lens' UpdateUser (Prelude.Maybe Prelude.Text)
updateUser_accessString = Lens.lens (\UpdateUser' {accessString} -> accessString) (\s@UpdateUser' {} a -> s {accessString = a} :: UpdateUser)

-- | Denotes the user\'s authentication properties, such as whether it
-- requires a password to authenticate.
updateUser_authenticationMode :: Lens.Lens' UpdateUser (Prelude.Maybe AuthenticationMode)
updateUser_authenticationMode = Lens.lens (\UpdateUser' {authenticationMode} -> authenticationMode) (\s@UpdateUser' {} a -> s {authenticationMode = a} :: UpdateUser)

-- | The name of the user
updateUser_userName :: Lens.Lens' UpdateUser Prelude.Text
updateUser_userName = Lens.lens (\UpdateUser' {userName} -> userName) (\s@UpdateUser' {} a -> s {userName = a} :: UpdateUser)

instance Core.AWSRequest UpdateUser where
  type AWSResponse UpdateUser = UpdateUserResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateUserResponse'
            Prelude.<$> (x Data..?> "User")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateUser where
  hashWithSalt _salt UpdateUser' {..} =
    _salt
      `Prelude.hashWithSalt` accessString
      `Prelude.hashWithSalt` authenticationMode
      `Prelude.hashWithSalt` userName

instance Prelude.NFData UpdateUser where
  rnf UpdateUser' {..} =
    Prelude.rnf accessString
      `Prelude.seq` Prelude.rnf authenticationMode
      `Prelude.seq` Prelude.rnf userName

instance Data.ToHeaders UpdateUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AmazonMemoryDB.UpdateUser" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateUser where
  toJSON UpdateUser' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccessString" Data..=) Prelude.<$> accessString,
            ("AuthenticationMode" Data..=)
              Prelude.<$> authenticationMode,
            Prelude.Just ("UserName" Data..= userName)
          ]
      )

instance Data.ToPath UpdateUser where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateUserResponse' smart constructor.
data UpdateUserResponse = UpdateUserResponse'
  { -- | The updated user
    user :: Prelude.Maybe User,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'user', 'updateUserResponse_user' - The updated user
--
-- 'httpStatus', 'updateUserResponse_httpStatus' - The response's http status code.
newUpdateUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateUserResponse
newUpdateUserResponse pHttpStatus_ =
  UpdateUserResponse'
    { user = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated user
updateUserResponse_user :: Lens.Lens' UpdateUserResponse (Prelude.Maybe User)
updateUserResponse_user = Lens.lens (\UpdateUserResponse' {user} -> user) (\s@UpdateUserResponse' {} a -> s {user = a} :: UpdateUserResponse)

-- | The response's http status code.
updateUserResponse_httpStatus :: Lens.Lens' UpdateUserResponse Prelude.Int
updateUserResponse_httpStatus = Lens.lens (\UpdateUserResponse' {httpStatus} -> httpStatus) (\s@UpdateUserResponse' {} a -> s {httpStatus = a} :: UpdateUserResponse)

instance Prelude.NFData UpdateUserResponse where
  rnf UpdateUserResponse' {..} =
    Prelude.rnf user
      `Prelude.seq` Prelude.rnf httpStatus
