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
-- Module      : Network.AWS.CognitoIdentityProvider.GetUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the user attributes and metadata for a user.
module Network.AWS.CognitoIdentityProvider.GetUser
  ( -- * Creating a Request
    GetUser (..),
    newGetUser,

    -- * Request Lenses
    getUser_accessToken,

    -- * Destructuring the Response
    GetUserResponse (..),
    newGetUserResponse,

    -- * Response Lenses
    getUserResponse_preferredMfaSetting,
    getUserResponse_userMFASettingList,
    getUserResponse_mfaOptions,
    getUserResponse_httpStatus,
    getUserResponse_username,
    getUserResponse_userAttributes,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to get information about the user.
--
-- /See:/ 'newGetUser' smart constructor.
data GetUser = GetUser'
  { -- | The access token returned by the server response to get information
    -- about the user.
    accessToken :: Core.Sensitive Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessToken', 'getUser_accessToken' - The access token returned by the server response to get information
-- about the user.
newGetUser ::
  -- | 'accessToken'
  Core.Text ->
  GetUser
newGetUser pAccessToken_ =
  GetUser'
    { accessToken =
        Core._Sensitive Lens.# pAccessToken_
    }

-- | The access token returned by the server response to get information
-- about the user.
getUser_accessToken :: Lens.Lens' GetUser Core.Text
getUser_accessToken = Lens.lens (\GetUser' {accessToken} -> accessToken) (\s@GetUser' {} a -> s {accessToken = a} :: GetUser) Core.. Core._Sensitive

instance Core.AWSRequest GetUser where
  type AWSResponse GetUser = GetUserResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetUserResponse'
            Core.<$> (x Core..?> "PreferredMfaSetting")
            Core.<*> ( x Core..?> "UserMFASettingList"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "MFAOptions" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "Username")
            Core.<*> (x Core..?> "UserAttributes" Core..!@ Core.mempty)
      )

instance Core.Hashable GetUser

instance Core.NFData GetUser

instance Core.ToHeaders GetUser where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.GetUser" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetUser where
  toJSON GetUser' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("AccessToken" Core..= accessToken)]
      )

instance Core.ToPath GetUser where
  toPath = Core.const "/"

instance Core.ToQuery GetUser where
  toQuery = Core.const Core.mempty

-- | Represents the response from the server from the request to get
-- information about the user.
--
-- /See:/ 'newGetUserResponse' smart constructor.
data GetUserResponse = GetUserResponse'
  { -- | The user\'s preferred MFA setting.
    preferredMfaSetting :: Core.Maybe Core.Text,
    -- | The MFA options that are enabled for the user. The possible values in
    -- this list are @SMS_MFA@ and @SOFTWARE_TOKEN_MFA@.
    userMFASettingList :: Core.Maybe [Core.Text],
    -- | /This response parameter is no longer supported./ It provides
    -- information only about SMS MFA configurations. It doesn\'t provide
    -- information about TOTP software token MFA configurations. To look up
    -- information about either type of MFA configuration, use
    -- UserMFASettingList instead.
    mfaOptions :: Core.Maybe [MFAOptionType],
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The user name of the user you wish to retrieve from the get user
    -- request.
    username :: Core.Sensitive Core.Text,
    -- | An array of name-value pairs representing user attributes.
    --
    -- For custom attributes, you must prepend the @custom:@ prefix to the
    -- attribute name.
    userAttributes :: [AttributeType]
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'preferredMfaSetting', 'getUserResponse_preferredMfaSetting' - The user\'s preferred MFA setting.
--
-- 'userMFASettingList', 'getUserResponse_userMFASettingList' - The MFA options that are enabled for the user. The possible values in
-- this list are @SMS_MFA@ and @SOFTWARE_TOKEN_MFA@.
--
-- 'mfaOptions', 'getUserResponse_mfaOptions' - /This response parameter is no longer supported./ It provides
-- information only about SMS MFA configurations. It doesn\'t provide
-- information about TOTP software token MFA configurations. To look up
-- information about either type of MFA configuration, use
-- UserMFASettingList instead.
--
-- 'httpStatus', 'getUserResponse_httpStatus' - The response's http status code.
--
-- 'username', 'getUserResponse_username' - The user name of the user you wish to retrieve from the get user
-- request.
--
-- 'userAttributes', 'getUserResponse_userAttributes' - An array of name-value pairs representing user attributes.
--
-- For custom attributes, you must prepend the @custom:@ prefix to the
-- attribute name.
newGetUserResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'username'
  Core.Text ->
  GetUserResponse
newGetUserResponse pHttpStatus_ pUsername_ =
  GetUserResponse'
    { preferredMfaSetting =
        Core.Nothing,
      userMFASettingList = Core.Nothing,
      mfaOptions = Core.Nothing,
      httpStatus = pHttpStatus_,
      username = Core._Sensitive Lens.# pUsername_,
      userAttributes = Core.mempty
    }

-- | The user\'s preferred MFA setting.
getUserResponse_preferredMfaSetting :: Lens.Lens' GetUserResponse (Core.Maybe Core.Text)
getUserResponse_preferredMfaSetting = Lens.lens (\GetUserResponse' {preferredMfaSetting} -> preferredMfaSetting) (\s@GetUserResponse' {} a -> s {preferredMfaSetting = a} :: GetUserResponse)

-- | The MFA options that are enabled for the user. The possible values in
-- this list are @SMS_MFA@ and @SOFTWARE_TOKEN_MFA@.
getUserResponse_userMFASettingList :: Lens.Lens' GetUserResponse (Core.Maybe [Core.Text])
getUserResponse_userMFASettingList = Lens.lens (\GetUserResponse' {userMFASettingList} -> userMFASettingList) (\s@GetUserResponse' {} a -> s {userMFASettingList = a} :: GetUserResponse) Core.. Lens.mapping Lens._Coerce

-- | /This response parameter is no longer supported./ It provides
-- information only about SMS MFA configurations. It doesn\'t provide
-- information about TOTP software token MFA configurations. To look up
-- information about either type of MFA configuration, use
-- UserMFASettingList instead.
getUserResponse_mfaOptions :: Lens.Lens' GetUserResponse (Core.Maybe [MFAOptionType])
getUserResponse_mfaOptions = Lens.lens (\GetUserResponse' {mfaOptions} -> mfaOptions) (\s@GetUserResponse' {} a -> s {mfaOptions = a} :: GetUserResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getUserResponse_httpStatus :: Lens.Lens' GetUserResponse Core.Int
getUserResponse_httpStatus = Lens.lens (\GetUserResponse' {httpStatus} -> httpStatus) (\s@GetUserResponse' {} a -> s {httpStatus = a} :: GetUserResponse)

-- | The user name of the user you wish to retrieve from the get user
-- request.
getUserResponse_username :: Lens.Lens' GetUserResponse Core.Text
getUserResponse_username = Lens.lens (\GetUserResponse' {username} -> username) (\s@GetUserResponse' {} a -> s {username = a} :: GetUserResponse) Core.. Core._Sensitive

-- | An array of name-value pairs representing user attributes.
--
-- For custom attributes, you must prepend the @custom:@ prefix to the
-- attribute name.
getUserResponse_userAttributes :: Lens.Lens' GetUserResponse [AttributeType]
getUserResponse_userAttributes = Lens.lens (\GetUserResponse' {userAttributes} -> userAttributes) (\s@GetUserResponse' {} a -> s {userAttributes = a} :: GetUserResponse) Core.. Lens._Coerce

instance Core.NFData GetUserResponse
