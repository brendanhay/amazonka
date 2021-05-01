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
-- Module      : Network.AWS.WorkMail.ResetPassword
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows the administrator to reset the password for a user.
module Network.AWS.WorkMail.ResetPassword
  ( -- * Creating a Request
    ResetPassword (..),
    newResetPassword,

    -- * Request Lenses
    resetPassword_organizationId,
    resetPassword_userId,
    resetPassword_password,

    -- * Destructuring the Response
    ResetPasswordResponse (..),
    newResetPasswordResponse,

    -- * Response Lenses
    resetPasswordResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newResetPassword' smart constructor.
data ResetPassword = ResetPassword'
  { -- | The identifier of the organization that contains the user for which the
    -- password is reset.
    organizationId :: Prelude.Text,
    -- | The identifier of the user for whom the password is reset.
    userId :: Prelude.Text,
    -- | The new password for the user.
    password :: Prelude.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResetPassword' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'resetPassword_organizationId' - The identifier of the organization that contains the user for which the
-- password is reset.
--
-- 'userId', 'resetPassword_userId' - The identifier of the user for whom the password is reset.
--
-- 'password', 'resetPassword_password' - The new password for the user.
newResetPassword ::
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'userId'
  Prelude.Text ->
  -- | 'password'
  Prelude.Text ->
  ResetPassword
newResetPassword pOrganizationId_ pUserId_ pPassword_ =
  ResetPassword'
    { organizationId = pOrganizationId_,
      userId = pUserId_,
      password = Prelude._Sensitive Lens.# pPassword_
    }

-- | The identifier of the organization that contains the user for which the
-- password is reset.
resetPassword_organizationId :: Lens.Lens' ResetPassword Prelude.Text
resetPassword_organizationId = Lens.lens (\ResetPassword' {organizationId} -> organizationId) (\s@ResetPassword' {} a -> s {organizationId = a} :: ResetPassword)

-- | The identifier of the user for whom the password is reset.
resetPassword_userId :: Lens.Lens' ResetPassword Prelude.Text
resetPassword_userId = Lens.lens (\ResetPassword' {userId} -> userId) (\s@ResetPassword' {} a -> s {userId = a} :: ResetPassword)

-- | The new password for the user.
resetPassword_password :: Lens.Lens' ResetPassword Prelude.Text
resetPassword_password = Lens.lens (\ResetPassword' {password} -> password) (\s@ResetPassword' {} a -> s {password = a} :: ResetPassword) Prelude.. Prelude._Sensitive

instance Prelude.AWSRequest ResetPassword where
  type Rs ResetPassword = ResetPasswordResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          ResetPasswordResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ResetPassword

instance Prelude.NFData ResetPassword

instance Prelude.ToHeaders ResetPassword where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "WorkMailService.ResetPassword" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ResetPassword where
  toJSON ResetPassword' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OrganizationId" Prelude..= organizationId),
            Prelude.Just ("UserId" Prelude..= userId),
            Prelude.Just ("Password" Prelude..= password)
          ]
      )

instance Prelude.ToPath ResetPassword where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ResetPassword where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newResetPasswordResponse' smart constructor.
data ResetPasswordResponse = ResetPasswordResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResetPasswordResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'resetPasswordResponse_httpStatus' - The response's http status code.
newResetPasswordResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ResetPasswordResponse
newResetPasswordResponse pHttpStatus_ =
  ResetPasswordResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
resetPasswordResponse_httpStatus :: Lens.Lens' ResetPasswordResponse Prelude.Int
resetPasswordResponse_httpStatus = Lens.lens (\ResetPasswordResponse' {httpStatus} -> httpStatus) (\s@ResetPasswordResponse' {} a -> s {httpStatus = a} :: ResetPasswordResponse)

instance Prelude.NFData ResetPasswordResponse
