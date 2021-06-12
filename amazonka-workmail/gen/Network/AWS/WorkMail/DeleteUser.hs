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
-- Module      : Network.AWS.WorkMail.DeleteUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a user from Amazon WorkMail and all subsequent systems. Before
-- you can delete a user, the user state must be @DISABLED@. Use the
-- DescribeUser action to confirm the user state.
--
-- Deleting a user is permanent and cannot be undone. WorkMail archives
-- user mailboxes for 30 days before they are permanently removed.
module Network.AWS.WorkMail.DeleteUser
  ( -- * Creating a Request
    DeleteUser (..),
    newDeleteUser,

    -- * Request Lenses
    deleteUser_organizationId,
    deleteUser_userId,

    -- * Destructuring the Response
    DeleteUserResponse (..),
    newDeleteUserResponse,

    -- * Response Lenses
    deleteUserResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newDeleteUser' smart constructor.
data DeleteUser = DeleteUser'
  { -- | The organization that contains the user to be deleted.
    organizationId :: Core.Text,
    -- | The identifier of the user to be deleted.
    userId :: Core.Text
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
-- 'organizationId', 'deleteUser_organizationId' - The organization that contains the user to be deleted.
--
-- 'userId', 'deleteUser_userId' - The identifier of the user to be deleted.
newDeleteUser ::
  -- | 'organizationId'
  Core.Text ->
  -- | 'userId'
  Core.Text ->
  DeleteUser
newDeleteUser pOrganizationId_ pUserId_ =
  DeleteUser'
    { organizationId = pOrganizationId_,
      userId = pUserId_
    }

-- | The organization that contains the user to be deleted.
deleteUser_organizationId :: Lens.Lens' DeleteUser Core.Text
deleteUser_organizationId = Lens.lens (\DeleteUser' {organizationId} -> organizationId) (\s@DeleteUser' {} a -> s {organizationId = a} :: DeleteUser)

-- | The identifier of the user to be deleted.
deleteUser_userId :: Lens.Lens' DeleteUser Core.Text
deleteUser_userId = Lens.lens (\DeleteUser' {userId} -> userId) (\s@DeleteUser' {} a -> s {userId = a} :: DeleteUser)

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
              Core.=# ("WorkMailService.DeleteUser" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteUser where
  toJSON DeleteUser' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("OrganizationId" Core..= organizationId),
            Core.Just ("UserId" Core..= userId)
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
