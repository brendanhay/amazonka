{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to get information about the user.
--
-- /See:/ 'newGetUser' smart constructor.
data GetUser = GetUser'
  { -- | The access token returned by the server response to get information
    -- about the user.
    accessToken :: Prelude.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  GetUser
newGetUser pAccessToken_ =
  GetUser'
    { accessToken =
        Prelude._Sensitive Lens.# pAccessToken_
    }

-- | The access token returned by the server response to get information
-- about the user.
getUser_accessToken :: Lens.Lens' GetUser Prelude.Text
getUser_accessToken = Lens.lens (\GetUser' {accessToken} -> accessToken) (\s@GetUser' {} a -> s {accessToken = a} :: GetUser) Prelude.. Prelude._Sensitive

instance Prelude.AWSRequest GetUser where
  type Rs GetUser = GetUserResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetUserResponse'
            Prelude.<$> (x Prelude..?> "PreferredMfaSetting")
            Prelude.<*> ( x Prelude..?> "UserMFASettingList"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..?> "MFAOptions"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "Username")
            Prelude.<*> ( x Prelude..?> "UserAttributes"
                            Prelude..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable GetUser

instance Prelude.NFData GetUser

instance Prelude.ToHeaders GetUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCognitoIdentityProviderService.GetUser" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetUser where
  toJSON GetUser' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AccessToken" Prelude..= accessToken)
          ]
      )

instance Prelude.ToPath GetUser where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetUser where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the response from the server from the request to get
-- information about the user.
--
-- /See:/ 'newGetUserResponse' smart constructor.
data GetUserResponse = GetUserResponse'
  { -- | The user\'s preferred MFA setting.
    preferredMfaSetting :: Prelude.Maybe Prelude.Text,
    -- | The MFA options that are enabled for the user. The possible values in
    -- this list are @SMS_MFA@ and @SOFTWARE_TOKEN_MFA@.
    userMFASettingList :: Prelude.Maybe [Prelude.Text],
    -- | /This response parameter is no longer supported./ It provides
    -- information only about SMS MFA configurations. It doesn\'t provide
    -- information about TOTP software token MFA configurations. To look up
    -- information about either type of MFA configuration, use
    -- UserMFASettingList instead.
    mfaOptions :: Prelude.Maybe [MFAOptionType],
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The user name of the user you wish to retrieve from the get user
    -- request.
    username :: Prelude.Sensitive Prelude.Text,
    -- | An array of name-value pairs representing user attributes.
    --
    -- For custom attributes, you must prepend the @custom:@ prefix to the
    -- attribute name.
    userAttributes :: [AttributeType]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  -- | 'username'
  Prelude.Text ->
  GetUserResponse
newGetUserResponse pHttpStatus_ pUsername_ =
  GetUserResponse'
    { preferredMfaSetting =
        Prelude.Nothing,
      userMFASettingList = Prelude.Nothing,
      mfaOptions = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      username = Prelude._Sensitive Lens.# pUsername_,
      userAttributes = Prelude.mempty
    }

-- | The user\'s preferred MFA setting.
getUserResponse_preferredMfaSetting :: Lens.Lens' GetUserResponse (Prelude.Maybe Prelude.Text)
getUserResponse_preferredMfaSetting = Lens.lens (\GetUserResponse' {preferredMfaSetting} -> preferredMfaSetting) (\s@GetUserResponse' {} a -> s {preferredMfaSetting = a} :: GetUserResponse)

-- | The MFA options that are enabled for the user. The possible values in
-- this list are @SMS_MFA@ and @SOFTWARE_TOKEN_MFA@.
getUserResponse_userMFASettingList :: Lens.Lens' GetUserResponse (Prelude.Maybe [Prelude.Text])
getUserResponse_userMFASettingList = Lens.lens (\GetUserResponse' {userMFASettingList} -> userMFASettingList) (\s@GetUserResponse' {} a -> s {userMFASettingList = a} :: GetUserResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | /This response parameter is no longer supported./ It provides
-- information only about SMS MFA configurations. It doesn\'t provide
-- information about TOTP software token MFA configurations. To look up
-- information about either type of MFA configuration, use
-- UserMFASettingList instead.
getUserResponse_mfaOptions :: Lens.Lens' GetUserResponse (Prelude.Maybe [MFAOptionType])
getUserResponse_mfaOptions = Lens.lens (\GetUserResponse' {mfaOptions} -> mfaOptions) (\s@GetUserResponse' {} a -> s {mfaOptions = a} :: GetUserResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
getUserResponse_httpStatus :: Lens.Lens' GetUserResponse Prelude.Int
getUserResponse_httpStatus = Lens.lens (\GetUserResponse' {httpStatus} -> httpStatus) (\s@GetUserResponse' {} a -> s {httpStatus = a} :: GetUserResponse)

-- | The user name of the user you wish to retrieve from the get user
-- request.
getUserResponse_username :: Lens.Lens' GetUserResponse Prelude.Text
getUserResponse_username = Lens.lens (\GetUserResponse' {username} -> username) (\s@GetUserResponse' {} a -> s {username = a} :: GetUserResponse) Prelude.. Prelude._Sensitive

-- | An array of name-value pairs representing user attributes.
--
-- For custom attributes, you must prepend the @custom:@ prefix to the
-- attribute name.
getUserResponse_userAttributes :: Lens.Lens' GetUserResponse [AttributeType]
getUserResponse_userAttributes = Lens.lens (\GetUserResponse' {userAttributes} -> userAttributes) (\s@GetUserResponse' {} a -> s {userAttributes = a} :: GetUserResponse) Prelude.. Prelude._Coerce

instance Prelude.NFData GetUserResponse
