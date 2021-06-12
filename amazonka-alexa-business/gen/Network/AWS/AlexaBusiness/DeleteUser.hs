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
-- Module      : Network.AWS.AlexaBusiness.DeleteUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified user by user ARN and enrollment ARN.
module Network.AWS.AlexaBusiness.DeleteUser
  ( -- * Creating a Request
    DeleteUser (..),
    newDeleteUser,

    -- * Request Lenses
    deleteUser_userArn,
    deleteUser_enrollmentId,

    -- * Destructuring the Response
    DeleteUserResponse (..),
    newDeleteUserResponse,

    -- * Response Lenses
    deleteUserResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteUser' smart constructor.
data DeleteUser = DeleteUser'
  { -- | The ARN of the user to delete in the organization. Required.
    userArn :: Core.Maybe Core.Text,
    -- | The ARN of the user\'s enrollment in the organization. Required.
    enrollmentId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userArn', 'deleteUser_userArn' - The ARN of the user to delete in the organization. Required.
--
-- 'enrollmentId', 'deleteUser_enrollmentId' - The ARN of the user\'s enrollment in the organization. Required.
newDeleteUser ::
  -- | 'enrollmentId'
  Core.Text ->
  DeleteUser
newDeleteUser pEnrollmentId_ =
  DeleteUser'
    { userArn = Core.Nothing,
      enrollmentId = pEnrollmentId_
    }

-- | The ARN of the user to delete in the organization. Required.
deleteUser_userArn :: Lens.Lens' DeleteUser (Core.Maybe Core.Text)
deleteUser_userArn = Lens.lens (\DeleteUser' {userArn} -> userArn) (\s@DeleteUser' {} a -> s {userArn = a} :: DeleteUser)

-- | The ARN of the user\'s enrollment in the organization. Required.
deleteUser_enrollmentId :: Lens.Lens' DeleteUser Core.Text
deleteUser_enrollmentId = Lens.lens (\DeleteUser' {enrollmentId} -> enrollmentId) (\s@DeleteUser' {} a -> s {enrollmentId = a} :: DeleteUser)

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
              Core.=# ("AlexaForBusiness.DeleteUser" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteUser where
  toJSON DeleteUser' {..} =
    Core.object
      ( Core.catMaybes
          [ ("UserArn" Core..=) Core.<$> userArn,
            Core.Just ("EnrollmentId" Core..= enrollmentId)
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
