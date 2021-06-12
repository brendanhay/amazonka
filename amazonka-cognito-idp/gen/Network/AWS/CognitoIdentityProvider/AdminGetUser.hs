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
-- Module      : Network.AWS.CognitoIdentityProvider.AdminGetUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the specified user by user name in a user pool as an administrator.
-- Works on any user.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.AdminGetUser
  ( -- * Creating a Request
    AdminGetUser (..),
    newAdminGetUser,

    -- * Request Lenses
    adminGetUser_userPoolId,
    adminGetUser_username,

    -- * Destructuring the Response
    AdminGetUserResponse (..),
    newAdminGetUserResponse,

    -- * Response Lenses
    adminGetUserResponse_preferredMfaSetting,
    adminGetUserResponse_userCreateDate,
    adminGetUserResponse_userLastModifiedDate,
    adminGetUserResponse_enabled,
    adminGetUserResponse_userMFASettingList,
    adminGetUserResponse_userAttributes,
    adminGetUserResponse_userStatus,
    adminGetUserResponse_mfaOptions,
    adminGetUserResponse_httpStatus,
    adminGetUserResponse_username,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to get the specified user as an administrator.
--
-- /See:/ 'newAdminGetUser' smart constructor.
data AdminGetUser = AdminGetUser'
  { -- | The user pool ID for the user pool where you want to get information
    -- about the user.
    userPoolId :: Core.Text,
    -- | The user name of the user you wish to retrieve.
    username :: Core.Sensitive Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'AdminGetUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userPoolId', 'adminGetUser_userPoolId' - The user pool ID for the user pool where you want to get information
-- about the user.
--
-- 'username', 'adminGetUser_username' - The user name of the user you wish to retrieve.
newAdminGetUser ::
  -- | 'userPoolId'
  Core.Text ->
  -- | 'username'
  Core.Text ->
  AdminGetUser
newAdminGetUser pUserPoolId_ pUsername_ =
  AdminGetUser'
    { userPoolId = pUserPoolId_,
      username = Core._Sensitive Lens.# pUsername_
    }

-- | The user pool ID for the user pool where you want to get information
-- about the user.
adminGetUser_userPoolId :: Lens.Lens' AdminGetUser Core.Text
adminGetUser_userPoolId = Lens.lens (\AdminGetUser' {userPoolId} -> userPoolId) (\s@AdminGetUser' {} a -> s {userPoolId = a} :: AdminGetUser)

-- | The user name of the user you wish to retrieve.
adminGetUser_username :: Lens.Lens' AdminGetUser Core.Text
adminGetUser_username = Lens.lens (\AdminGetUser' {username} -> username) (\s@AdminGetUser' {} a -> s {username = a} :: AdminGetUser) Core.. Core._Sensitive

instance Core.AWSRequest AdminGetUser where
  type AWSResponse AdminGetUser = AdminGetUserResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AdminGetUserResponse'
            Core.<$> (x Core..?> "PreferredMfaSetting")
            Core.<*> (x Core..?> "UserCreateDate")
            Core.<*> (x Core..?> "UserLastModifiedDate")
            Core.<*> (x Core..?> "Enabled")
            Core.<*> ( x Core..?> "UserMFASettingList"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "UserAttributes" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "UserStatus")
            Core.<*> (x Core..?> "MFAOptions" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "Username")
      )

instance Core.Hashable AdminGetUser

instance Core.NFData AdminGetUser

instance Core.ToHeaders AdminGetUser where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.AdminGetUser" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AdminGetUser where
  toJSON AdminGetUser' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("Username" Core..= username)
          ]
      )

instance Core.ToPath AdminGetUser where
  toPath = Core.const "/"

instance Core.ToQuery AdminGetUser where
  toQuery = Core.const Core.mempty

-- | Represents the response from the server from the request to get the
-- specified user as an administrator.
--
-- /See:/ 'newAdminGetUserResponse' smart constructor.
data AdminGetUserResponse = AdminGetUserResponse'
  { -- | The user\'s preferred MFA setting.
    preferredMfaSetting :: Core.Maybe Core.Text,
    -- | The date the user was created.
    userCreateDate :: Core.Maybe Core.POSIX,
    -- | The date the user was last modified.
    userLastModifiedDate :: Core.Maybe Core.POSIX,
    -- | Indicates that the status is enabled.
    enabled :: Core.Maybe Core.Bool,
    -- | The MFA options that are enabled for the user. The possible values in
    -- this list are @SMS_MFA@ and @SOFTWARE_TOKEN_MFA@.
    userMFASettingList :: Core.Maybe [Core.Text],
    -- | An array of name-value pairs representing user attributes.
    userAttributes :: Core.Maybe [AttributeType],
    -- | The user status. Can be one of the following:
    --
    -- -   UNCONFIRMED - User has been created but not confirmed.
    --
    -- -   CONFIRMED - User has been confirmed.
    --
    -- -   ARCHIVED - User is no longer active.
    --
    -- -   COMPROMISED - User is disabled due to a potential security threat.
    --
    -- -   UNKNOWN - User status is not known.
    --
    -- -   RESET_REQUIRED - User is confirmed, but the user must request a code
    --     and reset his or her password before he or she can sign in.
    --
    -- -   FORCE_CHANGE_PASSWORD - The user is confirmed and the user can sign
    --     in using a temporary password, but on first sign-in, the user must
    --     change his or her password to a new value before doing anything
    --     else.
    userStatus :: Core.Maybe UserStatusType,
    -- | /This response parameter is no longer supported./ It provides
    -- information only about SMS MFA configurations. It doesn\'t provide
    -- information about TOTP software token MFA configurations. To look up
    -- information about either type of MFA configuration, use
    -- UserMFASettingList instead.
    mfaOptions :: Core.Maybe [MFAOptionType],
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The user name of the user about whom you are receiving information.
    username :: Core.Sensitive Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'AdminGetUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'preferredMfaSetting', 'adminGetUserResponse_preferredMfaSetting' - The user\'s preferred MFA setting.
--
-- 'userCreateDate', 'adminGetUserResponse_userCreateDate' - The date the user was created.
--
-- 'userLastModifiedDate', 'adminGetUserResponse_userLastModifiedDate' - The date the user was last modified.
--
-- 'enabled', 'adminGetUserResponse_enabled' - Indicates that the status is enabled.
--
-- 'userMFASettingList', 'adminGetUserResponse_userMFASettingList' - The MFA options that are enabled for the user. The possible values in
-- this list are @SMS_MFA@ and @SOFTWARE_TOKEN_MFA@.
--
-- 'userAttributes', 'adminGetUserResponse_userAttributes' - An array of name-value pairs representing user attributes.
--
-- 'userStatus', 'adminGetUserResponse_userStatus' - The user status. Can be one of the following:
--
-- -   UNCONFIRMED - User has been created but not confirmed.
--
-- -   CONFIRMED - User has been confirmed.
--
-- -   ARCHIVED - User is no longer active.
--
-- -   COMPROMISED - User is disabled due to a potential security threat.
--
-- -   UNKNOWN - User status is not known.
--
-- -   RESET_REQUIRED - User is confirmed, but the user must request a code
--     and reset his or her password before he or she can sign in.
--
-- -   FORCE_CHANGE_PASSWORD - The user is confirmed and the user can sign
--     in using a temporary password, but on first sign-in, the user must
--     change his or her password to a new value before doing anything
--     else.
--
-- 'mfaOptions', 'adminGetUserResponse_mfaOptions' - /This response parameter is no longer supported./ It provides
-- information only about SMS MFA configurations. It doesn\'t provide
-- information about TOTP software token MFA configurations. To look up
-- information about either type of MFA configuration, use
-- UserMFASettingList instead.
--
-- 'httpStatus', 'adminGetUserResponse_httpStatus' - The response's http status code.
--
-- 'username', 'adminGetUserResponse_username' - The user name of the user about whom you are receiving information.
newAdminGetUserResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'username'
  Core.Text ->
  AdminGetUserResponse
newAdminGetUserResponse pHttpStatus_ pUsername_ =
  AdminGetUserResponse'
    { preferredMfaSetting =
        Core.Nothing,
      userCreateDate = Core.Nothing,
      userLastModifiedDate = Core.Nothing,
      enabled = Core.Nothing,
      userMFASettingList = Core.Nothing,
      userAttributes = Core.Nothing,
      userStatus = Core.Nothing,
      mfaOptions = Core.Nothing,
      httpStatus = pHttpStatus_,
      username = Core._Sensitive Lens.# pUsername_
    }

-- | The user\'s preferred MFA setting.
adminGetUserResponse_preferredMfaSetting :: Lens.Lens' AdminGetUserResponse (Core.Maybe Core.Text)
adminGetUserResponse_preferredMfaSetting = Lens.lens (\AdminGetUserResponse' {preferredMfaSetting} -> preferredMfaSetting) (\s@AdminGetUserResponse' {} a -> s {preferredMfaSetting = a} :: AdminGetUserResponse)

-- | The date the user was created.
adminGetUserResponse_userCreateDate :: Lens.Lens' AdminGetUserResponse (Core.Maybe Core.UTCTime)
adminGetUserResponse_userCreateDate = Lens.lens (\AdminGetUserResponse' {userCreateDate} -> userCreateDate) (\s@AdminGetUserResponse' {} a -> s {userCreateDate = a} :: AdminGetUserResponse) Core.. Lens.mapping Core._Time

-- | The date the user was last modified.
adminGetUserResponse_userLastModifiedDate :: Lens.Lens' AdminGetUserResponse (Core.Maybe Core.UTCTime)
adminGetUserResponse_userLastModifiedDate = Lens.lens (\AdminGetUserResponse' {userLastModifiedDate} -> userLastModifiedDate) (\s@AdminGetUserResponse' {} a -> s {userLastModifiedDate = a} :: AdminGetUserResponse) Core.. Lens.mapping Core._Time

-- | Indicates that the status is enabled.
adminGetUserResponse_enabled :: Lens.Lens' AdminGetUserResponse (Core.Maybe Core.Bool)
adminGetUserResponse_enabled = Lens.lens (\AdminGetUserResponse' {enabled} -> enabled) (\s@AdminGetUserResponse' {} a -> s {enabled = a} :: AdminGetUserResponse)

-- | The MFA options that are enabled for the user. The possible values in
-- this list are @SMS_MFA@ and @SOFTWARE_TOKEN_MFA@.
adminGetUserResponse_userMFASettingList :: Lens.Lens' AdminGetUserResponse (Core.Maybe [Core.Text])
adminGetUserResponse_userMFASettingList = Lens.lens (\AdminGetUserResponse' {userMFASettingList} -> userMFASettingList) (\s@AdminGetUserResponse' {} a -> s {userMFASettingList = a} :: AdminGetUserResponse) Core.. Lens.mapping Lens._Coerce

-- | An array of name-value pairs representing user attributes.
adminGetUserResponse_userAttributes :: Lens.Lens' AdminGetUserResponse (Core.Maybe [AttributeType])
adminGetUserResponse_userAttributes = Lens.lens (\AdminGetUserResponse' {userAttributes} -> userAttributes) (\s@AdminGetUserResponse' {} a -> s {userAttributes = a} :: AdminGetUserResponse) Core.. Lens.mapping Lens._Coerce

-- | The user status. Can be one of the following:
--
-- -   UNCONFIRMED - User has been created but not confirmed.
--
-- -   CONFIRMED - User has been confirmed.
--
-- -   ARCHIVED - User is no longer active.
--
-- -   COMPROMISED - User is disabled due to a potential security threat.
--
-- -   UNKNOWN - User status is not known.
--
-- -   RESET_REQUIRED - User is confirmed, but the user must request a code
--     and reset his or her password before he or she can sign in.
--
-- -   FORCE_CHANGE_PASSWORD - The user is confirmed and the user can sign
--     in using a temporary password, but on first sign-in, the user must
--     change his or her password to a new value before doing anything
--     else.
adminGetUserResponse_userStatus :: Lens.Lens' AdminGetUserResponse (Core.Maybe UserStatusType)
adminGetUserResponse_userStatus = Lens.lens (\AdminGetUserResponse' {userStatus} -> userStatus) (\s@AdminGetUserResponse' {} a -> s {userStatus = a} :: AdminGetUserResponse)

-- | /This response parameter is no longer supported./ It provides
-- information only about SMS MFA configurations. It doesn\'t provide
-- information about TOTP software token MFA configurations. To look up
-- information about either type of MFA configuration, use
-- UserMFASettingList instead.
adminGetUserResponse_mfaOptions :: Lens.Lens' AdminGetUserResponse (Core.Maybe [MFAOptionType])
adminGetUserResponse_mfaOptions = Lens.lens (\AdminGetUserResponse' {mfaOptions} -> mfaOptions) (\s@AdminGetUserResponse' {} a -> s {mfaOptions = a} :: AdminGetUserResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
adminGetUserResponse_httpStatus :: Lens.Lens' AdminGetUserResponse Core.Int
adminGetUserResponse_httpStatus = Lens.lens (\AdminGetUserResponse' {httpStatus} -> httpStatus) (\s@AdminGetUserResponse' {} a -> s {httpStatus = a} :: AdminGetUserResponse)

-- | The user name of the user about whom you are receiving information.
adminGetUserResponse_username :: Lens.Lens' AdminGetUserResponse Core.Text
adminGetUserResponse_username = Lens.lens (\AdminGetUserResponse' {username} -> username) (\s@AdminGetUserResponse' {} a -> s {username = a} :: AdminGetUserResponse) Core.. Core._Sensitive

instance Core.NFData AdminGetUserResponse
