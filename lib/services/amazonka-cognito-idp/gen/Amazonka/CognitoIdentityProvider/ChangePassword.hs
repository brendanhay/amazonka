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
-- Module      : Amazonka.CognitoIdentityProvider.ChangePassword
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the password for a specified user in a user pool.
module Amazonka.CognitoIdentityProvider.ChangePassword
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

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the request to change a user password.
--
-- /See:/ 'newChangePassword' smart constructor.
data ChangePassword = ChangePassword'
  { -- | The old password.
    previousPassword :: Data.Sensitive Prelude.Text,
    -- | The new password.
    proposedPassword :: Data.Sensitive Prelude.Text,
    -- | A valid access token that Amazon Cognito issued to the user whose
    -- password you want to change.
    accessToken :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
-- 'accessToken', 'changePassword_accessToken' - A valid access token that Amazon Cognito issued to the user whose
-- password you want to change.
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
          Data._Sensitive Lens.# pPreviousPassword_,
        proposedPassword =
          Data._Sensitive Lens.# pProposedPassword_,
        accessToken = Data._Sensitive Lens.# pAccessToken_
      }

-- | The old password.
changePassword_previousPassword :: Lens.Lens' ChangePassword Prelude.Text
changePassword_previousPassword = Lens.lens (\ChangePassword' {previousPassword} -> previousPassword) (\s@ChangePassword' {} a -> s {previousPassword = a} :: ChangePassword) Prelude.. Data._Sensitive

-- | The new password.
changePassword_proposedPassword :: Lens.Lens' ChangePassword Prelude.Text
changePassword_proposedPassword = Lens.lens (\ChangePassword' {proposedPassword} -> proposedPassword) (\s@ChangePassword' {} a -> s {proposedPassword = a} :: ChangePassword) Prelude.. Data._Sensitive

-- | A valid access token that Amazon Cognito issued to the user whose
-- password you want to change.
changePassword_accessToken :: Lens.Lens' ChangePassword Prelude.Text
changePassword_accessToken = Lens.lens (\ChangePassword' {accessToken} -> accessToken) (\s@ChangePassword' {} a -> s {accessToken = a} :: ChangePassword) Prelude.. Data._Sensitive

instance Core.AWSRequest ChangePassword where
  type
    AWSResponse ChangePassword =
      ChangePasswordResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          ChangePasswordResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ChangePassword where
  hashWithSalt _salt ChangePassword' {..} =
    _salt
      `Prelude.hashWithSalt` previousPassword
      `Prelude.hashWithSalt` proposedPassword
      `Prelude.hashWithSalt` accessToken

instance Prelude.NFData ChangePassword where
  rnf ChangePassword' {..} =
    Prelude.rnf previousPassword `Prelude.seq`
      Prelude.rnf proposedPassword `Prelude.seq`
        Prelude.rnf accessToken

instance Data.ToHeaders ChangePassword where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.ChangePassword" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ChangePassword where
  toJSON ChangePassword' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("PreviousPassword" Data..= previousPassword),
            Prelude.Just
              ("ProposedPassword" Data..= proposedPassword),
            Prelude.Just ("AccessToken" Data..= accessToken)
          ]
      )

instance Data.ToPath ChangePassword where
  toPath = Prelude.const "/"

instance Data.ToQuery ChangePassword where
  toQuery = Prelude.const Prelude.mempty

-- | The response from the server to the change password request.
--
-- /See:/ 'newChangePasswordResponse' smart constructor.
data ChangePasswordResponse = ChangePasswordResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData ChangePasswordResponse where
  rnf ChangePasswordResponse' {..} =
    Prelude.rnf httpStatus
