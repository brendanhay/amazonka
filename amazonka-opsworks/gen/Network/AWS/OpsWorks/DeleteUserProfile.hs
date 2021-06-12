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
-- Module      : Network.AWS.OpsWorks.DeleteUserProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a user profile.
--
-- __Required Permissions__: To use this action, an IAM user must have an
-- attached policy that explicitly grants permissions. For more information
-- about user permissions, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Network.AWS.OpsWorks.DeleteUserProfile
  ( -- * Creating a Request
    DeleteUserProfile (..),
    newDeleteUserProfile,

    -- * Request Lenses
    deleteUserProfile_iamUserArn,

    -- * Destructuring the Response
    DeleteUserProfileResponse (..),
    newDeleteUserProfileResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteUserProfile' smart constructor.
data DeleteUserProfile = DeleteUserProfile'
  { -- | The user\'s IAM ARN. This can also be a federated user\'s ARN.
    iamUserArn :: Core.Text
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
-- 'iamUserArn', 'deleteUserProfile_iamUserArn' - The user\'s IAM ARN. This can also be a federated user\'s ARN.
newDeleteUserProfile ::
  -- | 'iamUserArn'
  Core.Text ->
  DeleteUserProfile
newDeleteUserProfile pIamUserArn_ =
  DeleteUserProfile' {iamUserArn = pIamUserArn_}

-- | The user\'s IAM ARN. This can also be a federated user\'s ARN.
deleteUserProfile_iamUserArn :: Lens.Lens' DeleteUserProfile Core.Text
deleteUserProfile_iamUserArn = Lens.lens (\DeleteUserProfile' {iamUserArn} -> iamUserArn) (\s@DeleteUserProfile' {} a -> s {iamUserArn = a} :: DeleteUserProfile)

instance Core.AWSRequest DeleteUserProfile where
  type
    AWSResponse DeleteUserProfile =
      DeleteUserProfileResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteUserProfileResponse'

instance Core.Hashable DeleteUserProfile

instance Core.NFData DeleteUserProfile

instance Core.ToHeaders DeleteUserProfile where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorks_20130218.DeleteUserProfile" ::
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
          [Core.Just ("IamUserArn" Core..= iamUserArn)]
      )

instance Core.ToPath DeleteUserProfile where
  toPath = Core.const "/"

instance Core.ToQuery DeleteUserProfile where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteUserProfileResponse' smart constructor.
data DeleteUserProfileResponse = DeleteUserProfileResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteUserProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteUserProfileResponse ::
  DeleteUserProfileResponse
newDeleteUserProfileResponse =
  DeleteUserProfileResponse'

instance Core.NFData DeleteUserProfileResponse
