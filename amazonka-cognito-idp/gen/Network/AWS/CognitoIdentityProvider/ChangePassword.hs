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
-- Module      : Network.AWS.CognitoIdentityProvider.ChangePassword
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the password for a specified user in a user pool.
module Network.AWS.CognitoIdentityProvider.ChangePassword
  ( -- * Creating a Request
    ChangePassword (..),
    newChangePassword,

    -- * Request Lenses
    changePassword_previousPassword,
    changePassword_proposedPassword,
    changePassword_accessToken,

    -- * Destructuring the Response
    ChangePasswordResponse (..),
    newChangePasswordResponse,

    -- * Response Lenses
    changePasswordResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to change a user password.
--
-- /See:/ 'newChangePassword' smart constructor.
data ChangePassword = ChangePassword'
  { -- | The old password.
    previousPassword :: Prelude.Sensitive Prelude.Text,
    -- | The new password.
    proposedPassword :: Prelude.Sensitive Prelude.Text,
    -- | The access token.
    accessToken :: Prelude.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ChangePassword' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'previousPassword', 'changePassword_previousPassword' - The old password.
--
-- 'proposedPassword', 'changePassword_proposedPassword' - The new password.
--
-- 'accessToken', 'changePassword_accessToken' - The access token.
newChangePassword ::
  -- | 'previousPassword'
  Prelude.Text ->
  -- | 'proposedPassword'
  Prelude.Text ->
  -- | 'accessToken'
  Prelude.Text ->
  ChangePassword
newChangePassword
  pPreviousPassword_
  pProposedPassword_
  pAccessToken_ =
    ChangePassword'
      { previousPassword =
          Prelude._Sensitive Lens.# pPreviousPassword_,
        proposedPassword =
          Prelude._Sensitive Lens.# pProposedPassword_,
        accessToken =
          Prelude._Sensitive Lens.# pAccessToken_
      }

-- | The old password.
changePassword_previousPassword :: Lens.Lens' ChangePassword Prelude.Text
changePassword_previousPassword = Lens.lens (\ChangePassword' {previousPassword} -> previousPassword) (\s@ChangePassword' {} a -> s {previousPassword = a} :: ChangePassword) Prelude.. Prelude._Sensitive

-- | The new password.
changePassword_proposedPassword :: Lens.Lens' ChangePassword Prelude.Text
changePassword_proposedPassword = Lens.lens (\ChangePassword' {proposedPassword} -> proposedPassword) (\s@ChangePassword' {} a -> s {proposedPassword = a} :: ChangePassword) Prelude.. Prelude._Sensitive

-- | The access token.
changePassword_accessToken :: Lens.Lens' ChangePassword Prelude.Text
changePassword_accessToken = Lens.lens (\ChangePassword' {accessToken} -> accessToken) (\s@ChangePassword' {} a -> s {accessToken = a} :: ChangePassword) Prelude.. Prelude._Sensitive

instance Prelude.AWSRequest ChangePassword where
  type Rs ChangePassword = ChangePasswordResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          ChangePasswordResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ChangePassword

instance Prelude.NFData ChangePassword

instance Prelude.ToHeaders ChangePassword where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCognitoIdentityProviderService.ChangePassword" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ChangePassword where
  toJSON ChangePassword' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("PreviousPassword" Prelude..= previousPassword),
            Prelude.Just
              ("ProposedPassword" Prelude..= proposedPassword),
            Prelude.Just ("AccessToken" Prelude..= accessToken)
          ]
      )

instance Prelude.ToPath ChangePassword where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ChangePassword where
  toQuery = Prelude.const Prelude.mempty

-- | The response from the server to the change password request.
--
-- /See:/ 'newChangePasswordResponse' smart constructor.
data ChangePasswordResponse = ChangePasswordResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ChangePasswordResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'changePasswordResponse_httpStatus' - The response's http status code.
newChangePasswordResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ChangePasswordResponse
newChangePasswordResponse pHttpStatus_ =
  ChangePasswordResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
changePasswordResponse_httpStatus :: Lens.Lens' ChangePasswordResponse Prelude.Int
changePasswordResponse_httpStatus = Lens.lens (\ChangePasswordResponse' {httpStatus} -> httpStatus) (\s@ChangePasswordResponse' {} a -> s {httpStatus = a} :: ChangePasswordResponse)

instance Prelude.NFData ChangePasswordResponse
