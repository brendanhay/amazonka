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
-- Module      : Amazonka.Chime.UpdateUser
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates user details for a specified user ID. Currently, only
-- @LicenseType@ updates are supported for this action.
module Amazonka.Chime.UpdateUser
  ( -- * Creating a Request
    UpdateUser (..),
    newUpdateUser,

    -- * Request Lenses
    updateUser_licenseType,
    updateUser_alexaForBusinessMetadata,
    updateUser_userType,
    updateUser_accountId,
    updateUser_userId,

    -- * Destructuring the Response
    UpdateUserResponse (..),
    newUpdateUserResponse,

    -- * Response Lenses
    updateUserResponse_user,
    updateUserResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateUser' smart constructor.
data UpdateUser = UpdateUser'
  { -- | The user license type to update. This must be a supported license type
    -- for the Amazon Chime account that the user belongs to.
    licenseType :: Prelude.Maybe License,
    -- | The Alexa for Business metadata.
    alexaForBusinessMetadata :: Prelude.Maybe AlexaForBusinessMetadata,
    -- | The user type.
    userType :: Prelude.Maybe UserType,
    -- | The Amazon Chime account ID.
    accountId :: Prelude.Text,
    -- | The user ID.
    userId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'licenseType', 'updateUser_licenseType' - The user license type to update. This must be a supported license type
-- for the Amazon Chime account that the user belongs to.
--
-- 'alexaForBusinessMetadata', 'updateUser_alexaForBusinessMetadata' - The Alexa for Business metadata.
--
-- 'userType', 'updateUser_userType' - The user type.
--
-- 'accountId', 'updateUser_accountId' - The Amazon Chime account ID.
--
-- 'userId', 'updateUser_userId' - The user ID.
newUpdateUser ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'userId'
  Prelude.Text ->
  UpdateUser
newUpdateUser pAccountId_ pUserId_ =
  UpdateUser'
    { licenseType = Prelude.Nothing,
      alexaForBusinessMetadata = Prelude.Nothing,
      userType = Prelude.Nothing,
      accountId = pAccountId_,
      userId = pUserId_
    }

-- | The user license type to update. This must be a supported license type
-- for the Amazon Chime account that the user belongs to.
updateUser_licenseType :: Lens.Lens' UpdateUser (Prelude.Maybe License)
updateUser_licenseType = Lens.lens (\UpdateUser' {licenseType} -> licenseType) (\s@UpdateUser' {} a -> s {licenseType = a} :: UpdateUser)

-- | The Alexa for Business metadata.
updateUser_alexaForBusinessMetadata :: Lens.Lens' UpdateUser (Prelude.Maybe AlexaForBusinessMetadata)
updateUser_alexaForBusinessMetadata = Lens.lens (\UpdateUser' {alexaForBusinessMetadata} -> alexaForBusinessMetadata) (\s@UpdateUser' {} a -> s {alexaForBusinessMetadata = a} :: UpdateUser)

-- | The user type.
updateUser_userType :: Lens.Lens' UpdateUser (Prelude.Maybe UserType)
updateUser_userType = Lens.lens (\UpdateUser' {userType} -> userType) (\s@UpdateUser' {} a -> s {userType = a} :: UpdateUser)

-- | The Amazon Chime account ID.
updateUser_accountId :: Lens.Lens' UpdateUser Prelude.Text
updateUser_accountId = Lens.lens (\UpdateUser' {accountId} -> accountId) (\s@UpdateUser' {} a -> s {accountId = a} :: UpdateUser)

-- | The user ID.
updateUser_userId :: Lens.Lens' UpdateUser Prelude.Text
updateUser_userId = Lens.lens (\UpdateUser' {userId} -> userId) (\s@UpdateUser' {} a -> s {userId = a} :: UpdateUser)

instance Core.AWSRequest UpdateUser where
  type AWSResponse UpdateUser = UpdateUserResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateUserResponse'
            Prelude.<$> (x Core..?> "User")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateUser where
  hashWithSalt _salt UpdateUser' {..} =
    _salt `Prelude.hashWithSalt` licenseType
      `Prelude.hashWithSalt` alexaForBusinessMetadata
      `Prelude.hashWithSalt` userType
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` userId

instance Prelude.NFData UpdateUser where
  rnf UpdateUser' {..} =
    Prelude.rnf licenseType
      `Prelude.seq` Prelude.rnf alexaForBusinessMetadata
      `Prelude.seq` Prelude.rnf userType
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf userId

instance Core.ToHeaders UpdateUser where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON UpdateUser where
  toJSON UpdateUser' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("LicenseType" Core..=) Prelude.<$> licenseType,
            ("AlexaForBusinessMetadata" Core..=)
              Prelude.<$> alexaForBusinessMetadata,
            ("UserType" Core..=) Prelude.<$> userType
          ]
      )

instance Core.ToPath UpdateUser where
  toPath UpdateUser' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Core.toBS accountId,
        "/users/",
        Core.toBS userId
      ]

instance Core.ToQuery UpdateUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateUserResponse' smart constructor.
data UpdateUserResponse = UpdateUserResponse'
  { -- | The updated user details.
    user :: Prelude.Maybe User,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'user', 'updateUserResponse_user' - The updated user details.
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

-- | The updated user details.
updateUserResponse_user :: Lens.Lens' UpdateUserResponse (Prelude.Maybe User)
updateUserResponse_user = Lens.lens (\UpdateUserResponse' {user} -> user) (\s@UpdateUserResponse' {} a -> s {user = a} :: UpdateUserResponse)

-- | The response's http status code.
updateUserResponse_httpStatus :: Lens.Lens' UpdateUserResponse Prelude.Int
updateUserResponse_httpStatus = Lens.lens (\UpdateUserResponse' {httpStatus} -> httpStatus) (\s@UpdateUserResponse' {} a -> s {httpStatus = a} :: UpdateUserResponse)

instance Prelude.NFData UpdateUserResponse where
  rnf UpdateUserResponse' {..} =
    Prelude.rnf user
      `Prelude.seq` Prelude.rnf httpStatus
