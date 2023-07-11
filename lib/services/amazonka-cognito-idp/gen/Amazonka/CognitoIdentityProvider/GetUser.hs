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
-- Module      : Amazonka.CognitoIdentityProvider.GetUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the user attributes and metadata for a user.
module Amazonka.CognitoIdentityProvider.GetUser
  ( -- * Creating a Request
    GetUser (..),
    newGetUser,

    -- * Request Lenses
    getUser_accessToken,

    -- * Destructuring the Response
    GetUserResponse (..),
    newGetUserResponse,

    -- * Response Lenses
    getUserResponse_mfaOptions,
    getUserResponse_preferredMfaSetting,
    getUserResponse_userMFASettingList,
    getUserResponse_httpStatus,
    getUserResponse_username,
    getUserResponse_userAttributes,
  )
where

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the request to get information about the user.
--
-- /See:/ 'newGetUser' smart constructor.
data GetUser = GetUser'
  { -- | A non-expired access token for the user whose information you want to
    -- query.
    accessToken :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessToken', 'getUser_accessToken' - A non-expired access token for the user whose information you want to
-- query.
newGetUser ::
  -- | 'accessToken'
  Prelude.Text ->
  GetUser
newGetUser pAccessToken_ =
  GetUser'
    { accessToken =
        Data._Sensitive Lens.# pAccessToken_
    }

-- | A non-expired access token for the user whose information you want to
-- query.
getUser_accessToken :: Lens.Lens' GetUser Prelude.Text
getUser_accessToken = Lens.lens (\GetUser' {accessToken} -> accessToken) (\s@GetUser' {} a -> s {accessToken = a} :: GetUser) Prelude.. Data._Sensitive

instance Core.AWSRequest GetUser where
  type AWSResponse GetUser = GetUserResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetUserResponse'
            Prelude.<$> (x Data..?> "MFAOptions" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "PreferredMfaSetting")
            Prelude.<*> ( x
                            Data..?> "UserMFASettingList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Username")
            Prelude.<*> ( x
                            Data..?> "UserAttributes"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable GetUser where
  hashWithSalt _salt GetUser' {..} =
    _salt `Prelude.hashWithSalt` accessToken

instance Prelude.NFData GetUser where
  rnf GetUser' {..} = Prelude.rnf accessToken

instance Data.ToHeaders GetUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.GetUser" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetUser where
  toJSON GetUser' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("AccessToken" Data..= accessToken)]
      )

instance Data.ToPath GetUser where
  toPath = Prelude.const "/"

instance Data.ToQuery GetUser where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the response from the server from the request to get
-- information about the user.
--
-- /See:/ 'newGetUserResponse' smart constructor.
data GetUserResponse = GetUserResponse'
  { -- | /This response parameter is no longer supported./ It provides
    -- information only about SMS MFA configurations. It doesn\'t provide
    -- information about time-based one-time password (TOTP) software token MFA
    -- configurations. To look up information about either type of MFA
    -- configuration, use UserMFASettingList instead.
    mfaOptions :: Prelude.Maybe [MFAOptionType],
    -- | The user\'s preferred MFA setting.
    preferredMfaSetting :: Prelude.Maybe Prelude.Text,
    -- | The MFA options that are activated for the user. The possible values in
    -- this list are @SMS_MFA@ and @SOFTWARE_TOKEN_MFA@.
    userMFASettingList :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The user name of the user you want to retrieve from the get user
    -- request.
    username :: Data.Sensitive Prelude.Text,
    -- | An array of name-value pairs representing user attributes.
    --
    -- For custom attributes, you must prepend the @custom:@ prefix to the
    -- attribute name.
    userAttributes :: [AttributeType]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mfaOptions', 'getUserResponse_mfaOptions' - /This response parameter is no longer supported./ It provides
-- information only about SMS MFA configurations. It doesn\'t provide
-- information about time-based one-time password (TOTP) software token MFA
-- configurations. To look up information about either type of MFA
-- configuration, use UserMFASettingList instead.
--
-- 'preferredMfaSetting', 'getUserResponse_preferredMfaSetting' - The user\'s preferred MFA setting.
--
-- 'userMFASettingList', 'getUserResponse_userMFASettingList' - The MFA options that are activated for the user. The possible values in
-- this list are @SMS_MFA@ and @SOFTWARE_TOKEN_MFA@.
--
-- 'httpStatus', 'getUserResponse_httpStatus' - The response's http status code.
--
-- 'username', 'getUserResponse_username' - The user name of the user you want to retrieve from the get user
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
    { mfaOptions = Prelude.Nothing,
      preferredMfaSetting = Prelude.Nothing,
      userMFASettingList = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      username = Data._Sensitive Lens.# pUsername_,
      userAttributes = Prelude.mempty
    }

-- | /This response parameter is no longer supported./ It provides
-- information only about SMS MFA configurations. It doesn\'t provide
-- information about time-based one-time password (TOTP) software token MFA
-- configurations. To look up information about either type of MFA
-- configuration, use UserMFASettingList instead.
getUserResponse_mfaOptions :: Lens.Lens' GetUserResponse (Prelude.Maybe [MFAOptionType])
getUserResponse_mfaOptions = Lens.lens (\GetUserResponse' {mfaOptions} -> mfaOptions) (\s@GetUserResponse' {} a -> s {mfaOptions = a} :: GetUserResponse) Prelude.. Lens.mapping Lens.coerced

-- | The user\'s preferred MFA setting.
getUserResponse_preferredMfaSetting :: Lens.Lens' GetUserResponse (Prelude.Maybe Prelude.Text)
getUserResponse_preferredMfaSetting = Lens.lens (\GetUserResponse' {preferredMfaSetting} -> preferredMfaSetting) (\s@GetUserResponse' {} a -> s {preferredMfaSetting = a} :: GetUserResponse)

-- | The MFA options that are activated for the user. The possible values in
-- this list are @SMS_MFA@ and @SOFTWARE_TOKEN_MFA@.
getUserResponse_userMFASettingList :: Lens.Lens' GetUserResponse (Prelude.Maybe [Prelude.Text])
getUserResponse_userMFASettingList = Lens.lens (\GetUserResponse' {userMFASettingList} -> userMFASettingList) (\s@GetUserResponse' {} a -> s {userMFASettingList = a} :: GetUserResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getUserResponse_httpStatus :: Lens.Lens' GetUserResponse Prelude.Int
getUserResponse_httpStatus = Lens.lens (\GetUserResponse' {httpStatus} -> httpStatus) (\s@GetUserResponse' {} a -> s {httpStatus = a} :: GetUserResponse)

-- | The user name of the user you want to retrieve from the get user
-- request.
getUserResponse_username :: Lens.Lens' GetUserResponse Prelude.Text
getUserResponse_username = Lens.lens (\GetUserResponse' {username} -> username) (\s@GetUserResponse' {} a -> s {username = a} :: GetUserResponse) Prelude.. Data._Sensitive

-- | An array of name-value pairs representing user attributes.
--
-- For custom attributes, you must prepend the @custom:@ prefix to the
-- attribute name.
getUserResponse_userAttributes :: Lens.Lens' GetUserResponse [AttributeType]
getUserResponse_userAttributes = Lens.lens (\GetUserResponse' {userAttributes} -> userAttributes) (\s@GetUserResponse' {} a -> s {userAttributes = a} :: GetUserResponse) Prelude.. Lens.coerced

instance Prelude.NFData GetUserResponse where
  rnf GetUserResponse' {..} =
    Prelude.rnf mfaOptions
      `Prelude.seq` Prelude.rnf preferredMfaSetting
      `Prelude.seq` Prelude.rnf userMFASettingList
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf username
      `Prelude.seq` Prelude.rnf userAttributes
