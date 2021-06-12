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
-- Module      : Network.AWS.AppStream.DeleteUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a user from the user pool.
module Network.AWS.AppStream.DeleteUser
  ( -- * Creating a Request
    DeleteUser (..),
    newDeleteUser,

    -- * Request Lenses
    deleteUser_userName,
    deleteUser_authenticationType,

    -- * Destructuring the Response
    DeleteUserResponse (..),
    newDeleteUserResponse,

    -- * Response Lenses
    deleteUserResponse_httpStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteUser' smart constructor.
data DeleteUser = DeleteUser'
  { -- | The email address of the user.
    --
    -- Users\' email addresses are case-sensitive.
    userName :: Core.Sensitive Core.Text,
    -- | The authentication type for the user. You must specify USERPOOL.
    authenticationType :: AuthenticationType
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'deleteUser_userName' - The email address of the user.
--
-- Users\' email addresses are case-sensitive.
--
-- 'authenticationType', 'deleteUser_authenticationType' - The authentication type for the user. You must specify USERPOOL.
newDeleteUser ::
  -- | 'userName'
  Core.Text ->
  -- | 'authenticationType'
  AuthenticationType ->
  DeleteUser
newDeleteUser pUserName_ pAuthenticationType_ =
  DeleteUser'
    { userName =
        Core._Sensitive Lens.# pUserName_,
      authenticationType = pAuthenticationType_
    }

-- | The email address of the user.
--
-- Users\' email addresses are case-sensitive.
deleteUser_userName :: Lens.Lens' DeleteUser Core.Text
deleteUser_userName = Lens.lens (\DeleteUser' {userName} -> userName) (\s@DeleteUser' {} a -> s {userName = a} :: DeleteUser) Core.. Core._Sensitive

-- | The authentication type for the user. You must specify USERPOOL.
deleteUser_authenticationType :: Lens.Lens' DeleteUser AuthenticationType
deleteUser_authenticationType = Lens.lens (\DeleteUser' {authenticationType} -> authenticationType) (\s@DeleteUser' {} a -> s {authenticationType = a} :: DeleteUser)

instance Core.AWSRequest DeleteUser where
  type AWSResponse DeleteUser = DeleteUserResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteUserResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteUser

instance Core.NFData DeleteUser

instance Core.ToHeaders DeleteUser where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.DeleteUser" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteUser where
  toJSON DeleteUser' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserName" Core..= userName),
            Core.Just
              ("AuthenticationType" Core..= authenticationType)
          ]
      )

instance Core.ToPath DeleteUser where
  toPath = Core.const "/"

instance Core.ToQuery DeleteUser where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteUserResponse' smart constructor.
data DeleteUserResponse = DeleteUserResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteUserResponse_httpStatus' - The response's http status code.
newDeleteUserResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteUserResponse
newDeleteUserResponse pHttpStatus_ =
  DeleteUserResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteUserResponse_httpStatus :: Lens.Lens' DeleteUserResponse Core.Int
deleteUserResponse_httpStatus = Lens.lens (\DeleteUserResponse' {httpStatus} -> httpStatus) (\s@DeleteUserResponse' {} a -> s {httpStatus = a} :: DeleteUserResponse)

instance Core.NFData DeleteUserResponse
