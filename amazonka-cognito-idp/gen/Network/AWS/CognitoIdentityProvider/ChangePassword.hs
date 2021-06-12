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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to change a user password.
--
-- /See:/ 'newChangePassword' smart constructor.
data ChangePassword = ChangePassword'
  { -- | The old password.
    previousPassword :: Core.Sensitive Core.Text,
    -- | The new password.
    proposedPassword :: Core.Sensitive Core.Text,
    -- | The access token.
    accessToken :: Core.Sensitive Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'proposedPassword'
  Core.Text ->
  -- | 'accessToken'
  Core.Text ->
  ChangePassword
newChangePassword
  pPreviousPassword_
  pProposedPassword_
  pAccessToken_ =
    ChangePassword'
      { previousPassword =
          Core._Sensitive Lens.# pPreviousPassword_,
        proposedPassword =
          Core._Sensitive Lens.# pProposedPassword_,
        accessToken = Core._Sensitive Lens.# pAccessToken_
      }

-- | The old password.
changePassword_previousPassword :: Lens.Lens' ChangePassword Core.Text
changePassword_previousPassword = Lens.lens (\ChangePassword' {previousPassword} -> previousPassword) (\s@ChangePassword' {} a -> s {previousPassword = a} :: ChangePassword) Core.. Core._Sensitive

-- | The new password.
changePassword_proposedPassword :: Lens.Lens' ChangePassword Core.Text
changePassword_proposedPassword = Lens.lens (\ChangePassword' {proposedPassword} -> proposedPassword) (\s@ChangePassword' {} a -> s {proposedPassword = a} :: ChangePassword) Core.. Core._Sensitive

-- | The access token.
changePassword_accessToken :: Lens.Lens' ChangePassword Core.Text
changePassword_accessToken = Lens.lens (\ChangePassword' {accessToken} -> accessToken) (\s@ChangePassword' {} a -> s {accessToken = a} :: ChangePassword) Core.. Core._Sensitive

instance Core.AWSRequest ChangePassword where
  type
    AWSResponse ChangePassword =
      ChangePasswordResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          ChangePasswordResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ChangePassword

instance Core.NFData ChangePassword

instance Core.ToHeaders ChangePassword where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.ChangePassword" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ChangePassword where
  toJSON ChangePassword' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("PreviousPassword" Core..= previousPassword),
            Core.Just
              ("ProposedPassword" Core..= proposedPassword),
            Core.Just ("AccessToken" Core..= accessToken)
          ]
      )

instance Core.ToPath ChangePassword where
  toPath = Core.const "/"

instance Core.ToQuery ChangePassword where
  toQuery = Core.const Core.mempty

-- | The response from the server to the change password request.
--
-- /See:/ 'newChangePasswordResponse' smart constructor.
data ChangePasswordResponse = ChangePasswordResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ChangePasswordResponse
newChangePasswordResponse pHttpStatus_ =
  ChangePasswordResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
changePasswordResponse_httpStatus :: Lens.Lens' ChangePasswordResponse Core.Int
changePasswordResponse_httpStatus = Lens.lens (\ChangePasswordResponse' {httpStatus} -> httpStatus) (\s@ChangePasswordResponse' {} a -> s {httpStatus = a} :: ChangePasswordResponse)

instance Core.NFData ChangePasswordResponse
