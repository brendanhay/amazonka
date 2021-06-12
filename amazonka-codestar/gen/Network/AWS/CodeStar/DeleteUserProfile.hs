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
-- Module      : Network.AWS.CodeStar.DeleteUserProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a user profile in AWS CodeStar, including all personal
-- preference data associated with that profile, such as display name and
-- email address. It does not delete the history of that user, for example
-- the history of commits made by that user.
module Network.AWS.CodeStar.DeleteUserProfile
  ( -- * Creating a Request
    DeleteUserProfile (..),
    newDeleteUserProfile,

    -- * Request Lenses
    deleteUserProfile_userArn,

    -- * Destructuring the Response
    DeleteUserProfileResponse (..),
    newDeleteUserProfileResponse,

    -- * Response Lenses
    deleteUserProfileResponse_httpStatus,
    deleteUserProfileResponse_userArn,
  )
where

import Network.AWS.CodeStar.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteUserProfile' smart constructor.
data DeleteUserProfile = DeleteUserProfile'
  { -- | The Amazon Resource Name (ARN) of the user to delete from AWS CodeStar.
    userArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteUserProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userArn', 'deleteUserProfile_userArn' - The Amazon Resource Name (ARN) of the user to delete from AWS CodeStar.
newDeleteUserProfile ::
  -- | 'userArn'
  Core.Text ->
  DeleteUserProfile
newDeleteUserProfile pUserArn_ =
  DeleteUserProfile' {userArn = pUserArn_}

-- | The Amazon Resource Name (ARN) of the user to delete from AWS CodeStar.
deleteUserProfile_userArn :: Lens.Lens' DeleteUserProfile Core.Text
deleteUserProfile_userArn = Lens.lens (\DeleteUserProfile' {userArn} -> userArn) (\s@DeleteUserProfile' {} a -> s {userArn = a} :: DeleteUserProfile)

instance Core.AWSRequest DeleteUserProfile where
  type
    AWSResponse DeleteUserProfile =
      DeleteUserProfileResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteUserProfileResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "userArn")
      )

instance Core.Hashable DeleteUserProfile

instance Core.NFData DeleteUserProfile

instance Core.ToHeaders DeleteUserProfile where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeStar_20170419.DeleteUserProfile" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteUserProfile where
  toJSON DeleteUserProfile' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("userArn" Core..= userArn)]
      )

instance Core.ToPath DeleteUserProfile where
  toPath = Core.const "/"

instance Core.ToQuery DeleteUserProfile where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteUserProfileResponse' smart constructor.
data DeleteUserProfileResponse = DeleteUserProfileResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The Amazon Resource Name (ARN) of the user deleted from AWS CodeStar.
    userArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteUserProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteUserProfileResponse_httpStatus' - The response's http status code.
--
-- 'userArn', 'deleteUserProfileResponse_userArn' - The Amazon Resource Name (ARN) of the user deleted from AWS CodeStar.
newDeleteUserProfileResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'userArn'
  Core.Text ->
  DeleteUserProfileResponse
newDeleteUserProfileResponse pHttpStatus_ pUserArn_ =
  DeleteUserProfileResponse'
    { httpStatus =
        pHttpStatus_,
      userArn = pUserArn_
    }

-- | The response's http status code.
deleteUserProfileResponse_httpStatus :: Lens.Lens' DeleteUserProfileResponse Core.Int
deleteUserProfileResponse_httpStatus = Lens.lens (\DeleteUserProfileResponse' {httpStatus} -> httpStatus) (\s@DeleteUserProfileResponse' {} a -> s {httpStatus = a} :: DeleteUserProfileResponse)

-- | The Amazon Resource Name (ARN) of the user deleted from AWS CodeStar.
deleteUserProfileResponse_userArn :: Lens.Lens' DeleteUserProfileResponse Core.Text
deleteUserProfileResponse_userArn = Lens.lens (\DeleteUserProfileResponse' {userArn} -> userArn) (\s@DeleteUserProfileResponse' {} a -> s {userArn = a} :: DeleteUserProfileResponse)

instance Core.NFData DeleteUserProfileResponse
