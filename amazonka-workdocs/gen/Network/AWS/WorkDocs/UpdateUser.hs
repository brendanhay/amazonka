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
-- Module      : Network.AWS.WorkDocs.UpdateUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified attributes of the specified user, and grants or
-- revokes administrative privileges to the Amazon WorkDocs site.
module Network.AWS.WorkDocs.UpdateUser
  ( -- * Creating a Request
    UpdateUser (..),
    newUpdateUser,

    -- * Request Lenses
    updateUser_storageRule,
    updateUser_grantPoweruserPrivileges,
    updateUser_timeZoneId,
    updateUser_surname,
    updateUser_locale,
    updateUser_givenName,
    updateUser_authenticationToken,
    updateUser_type,
    updateUser_userId,

    -- * Destructuring the Response
    UpdateUserResponse (..),
    newUpdateUserResponse,

    -- * Response Lenses
    updateUserResponse_user,
    updateUserResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'newUpdateUser' smart constructor.
data UpdateUser = UpdateUser'
  { -- | The amount of storage for the user.
    storageRule :: Core.Maybe StorageRuleType,
    -- | Boolean value to determine whether the user is granted Poweruser
    -- privileges.
    grantPoweruserPrivileges :: Core.Maybe BooleanEnumType,
    -- | The time zone ID of the user.
    timeZoneId :: Core.Maybe Core.Text,
    -- | The surname of the user.
    surname :: Core.Maybe Core.Text,
    -- | The locale of the user.
    locale :: Core.Maybe LocaleType,
    -- | The given name of the user.
    givenName :: Core.Maybe Core.Text,
    -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The type of the user.
    type' :: Core.Maybe UserType,
    -- | The ID of the user.
    userId :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'storageRule', 'updateUser_storageRule' - The amount of storage for the user.
--
-- 'grantPoweruserPrivileges', 'updateUser_grantPoweruserPrivileges' - Boolean value to determine whether the user is granted Poweruser
-- privileges.
--
-- 'timeZoneId', 'updateUser_timeZoneId' - The time zone ID of the user.
--
-- 'surname', 'updateUser_surname' - The surname of the user.
--
-- 'locale', 'updateUser_locale' - The locale of the user.
--
-- 'givenName', 'updateUser_givenName' - The given name of the user.
--
-- 'authenticationToken', 'updateUser_authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
--
-- 'type'', 'updateUser_type' - The type of the user.
--
-- 'userId', 'updateUser_userId' - The ID of the user.
newUpdateUser ::
  -- | 'userId'
  Core.Text ->
  UpdateUser
newUpdateUser pUserId_ =
  UpdateUser'
    { storageRule = Core.Nothing,
      grantPoweruserPrivileges = Core.Nothing,
      timeZoneId = Core.Nothing,
      surname = Core.Nothing,
      locale = Core.Nothing,
      givenName = Core.Nothing,
      authenticationToken = Core.Nothing,
      type' = Core.Nothing,
      userId = pUserId_
    }

-- | The amount of storage for the user.
updateUser_storageRule :: Lens.Lens' UpdateUser (Core.Maybe StorageRuleType)
updateUser_storageRule = Lens.lens (\UpdateUser' {storageRule} -> storageRule) (\s@UpdateUser' {} a -> s {storageRule = a} :: UpdateUser)

-- | Boolean value to determine whether the user is granted Poweruser
-- privileges.
updateUser_grantPoweruserPrivileges :: Lens.Lens' UpdateUser (Core.Maybe BooleanEnumType)
updateUser_grantPoweruserPrivileges = Lens.lens (\UpdateUser' {grantPoweruserPrivileges} -> grantPoweruserPrivileges) (\s@UpdateUser' {} a -> s {grantPoweruserPrivileges = a} :: UpdateUser)

-- | The time zone ID of the user.
updateUser_timeZoneId :: Lens.Lens' UpdateUser (Core.Maybe Core.Text)
updateUser_timeZoneId = Lens.lens (\UpdateUser' {timeZoneId} -> timeZoneId) (\s@UpdateUser' {} a -> s {timeZoneId = a} :: UpdateUser)

-- | The surname of the user.
updateUser_surname :: Lens.Lens' UpdateUser (Core.Maybe Core.Text)
updateUser_surname = Lens.lens (\UpdateUser' {surname} -> surname) (\s@UpdateUser' {} a -> s {surname = a} :: UpdateUser)

-- | The locale of the user.
updateUser_locale :: Lens.Lens' UpdateUser (Core.Maybe LocaleType)
updateUser_locale = Lens.lens (\UpdateUser' {locale} -> locale) (\s@UpdateUser' {} a -> s {locale = a} :: UpdateUser)

-- | The given name of the user.
updateUser_givenName :: Lens.Lens' UpdateUser (Core.Maybe Core.Text)
updateUser_givenName = Lens.lens (\UpdateUser' {givenName} -> givenName) (\s@UpdateUser' {} a -> s {givenName = a} :: UpdateUser)

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
updateUser_authenticationToken :: Lens.Lens' UpdateUser (Core.Maybe Core.Text)
updateUser_authenticationToken = Lens.lens (\UpdateUser' {authenticationToken} -> authenticationToken) (\s@UpdateUser' {} a -> s {authenticationToken = a} :: UpdateUser) Core.. Lens.mapping Core._Sensitive

-- | The type of the user.
updateUser_type :: Lens.Lens' UpdateUser (Core.Maybe UserType)
updateUser_type = Lens.lens (\UpdateUser' {type'} -> type') (\s@UpdateUser' {} a -> s {type' = a} :: UpdateUser)

-- | The ID of the user.
updateUser_userId :: Lens.Lens' UpdateUser Core.Text
updateUser_userId = Lens.lens (\UpdateUser' {userId} -> userId) (\s@UpdateUser' {} a -> s {userId = a} :: UpdateUser)

instance Core.AWSRequest UpdateUser where
  type AWSResponse UpdateUser = UpdateUserResponse
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateUserResponse'
            Core.<$> (x Core..?> "User")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateUser

instance Core.NFData UpdateUser

instance Core.ToHeaders UpdateUser where
  toHeaders UpdateUser' {..} =
    Core.mconcat
      [ "Authentication" Core.=# authenticationToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
      ]

instance Core.ToJSON UpdateUser where
  toJSON UpdateUser' {..} =
    Core.object
      ( Core.catMaybes
          [ ("StorageRule" Core..=) Core.<$> storageRule,
            ("GrantPoweruserPrivileges" Core..=)
              Core.<$> grantPoweruserPrivileges,
            ("TimeZoneId" Core..=) Core.<$> timeZoneId,
            ("Surname" Core..=) Core.<$> surname,
            ("Locale" Core..=) Core.<$> locale,
            ("GivenName" Core..=) Core.<$> givenName,
            ("Type" Core..=) Core.<$> type'
          ]
      )

instance Core.ToPath UpdateUser where
  toPath UpdateUser' {..} =
    Core.mconcat ["/api/v1/users/", Core.toBS userId]

instance Core.ToQuery UpdateUser where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateUserResponse' smart constructor.
data UpdateUserResponse = UpdateUserResponse'
  { -- | The user information.
    user :: Core.Maybe User,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'user', 'updateUserResponse_user' - The user information.
--
-- 'httpStatus', 'updateUserResponse_httpStatus' - The response's http status code.
newUpdateUserResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateUserResponse
newUpdateUserResponse pHttpStatus_ =
  UpdateUserResponse'
    { user = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The user information.
updateUserResponse_user :: Lens.Lens' UpdateUserResponse (Core.Maybe User)
updateUserResponse_user = Lens.lens (\UpdateUserResponse' {user} -> user) (\s@UpdateUserResponse' {} a -> s {user = a} :: UpdateUserResponse)

-- | The response's http status code.
updateUserResponse_httpStatus :: Lens.Lens' UpdateUserResponse Core.Int
updateUserResponse_httpStatus = Lens.lens (\UpdateUserResponse' {httpStatus} -> httpStatus) (\s@UpdateUserResponse' {} a -> s {httpStatus = a} :: UpdateUserResponse)

instance Core.NFData UpdateUserResponse
