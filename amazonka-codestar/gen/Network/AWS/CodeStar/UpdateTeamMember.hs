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
-- Module      : Network.AWS.CodeStar.UpdateTeamMember
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a team member\'s attributes in an AWS CodeStar project. For
-- example, you can change a team member\'s role in the project, or change
-- whether they have remote access to project resources.
module Network.AWS.CodeStar.UpdateTeamMember
  ( -- * Creating a Request
    UpdateTeamMember (..),
    newUpdateTeamMember,

    -- * Request Lenses
    updateTeamMember_projectRole,
    updateTeamMember_remoteAccessAllowed,
    updateTeamMember_projectId,
    updateTeamMember_userArn,

    -- * Destructuring the Response
    UpdateTeamMemberResponse (..),
    newUpdateTeamMemberResponse,

    -- * Response Lenses
    updateTeamMemberResponse_userArn,
    updateTeamMemberResponse_projectRole,
    updateTeamMemberResponse_remoteAccessAllowed,
    updateTeamMemberResponse_httpStatus,
  )
where

import Network.AWS.CodeStar.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateTeamMember' smart constructor.
data UpdateTeamMember = UpdateTeamMember'
  { -- | The role assigned to the user in the project. Project roles have
    -- different levels of access. For more information, see
    -- <http://docs.aws.amazon.com/codestar/latest/userguide/working-with-teams.html Working with Teams>
    -- in the /AWS CodeStar User Guide/.
    projectRole :: Core.Maybe Core.Text,
    -- | Whether a team member is allowed to remotely access project resources
    -- using the SSH public key associated with the user\'s profile. Even if
    -- this is set to True, the user must associate a public key with their
    -- profile before the user can access resources.
    remoteAccessAllowed :: Core.Maybe Core.Bool,
    -- | The ID of the project.
    projectId :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the user for whom you want to change
    -- team membership attributes.
    userArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateTeamMember' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'projectRole', 'updateTeamMember_projectRole' - The role assigned to the user in the project. Project roles have
-- different levels of access. For more information, see
-- <http://docs.aws.amazon.com/codestar/latest/userguide/working-with-teams.html Working with Teams>
-- in the /AWS CodeStar User Guide/.
--
-- 'remoteAccessAllowed', 'updateTeamMember_remoteAccessAllowed' - Whether a team member is allowed to remotely access project resources
-- using the SSH public key associated with the user\'s profile. Even if
-- this is set to True, the user must associate a public key with their
-- profile before the user can access resources.
--
-- 'projectId', 'updateTeamMember_projectId' - The ID of the project.
--
-- 'userArn', 'updateTeamMember_userArn' - The Amazon Resource Name (ARN) of the user for whom you want to change
-- team membership attributes.
newUpdateTeamMember ::
  -- | 'projectId'
  Core.Text ->
  -- | 'userArn'
  Core.Text ->
  UpdateTeamMember
newUpdateTeamMember pProjectId_ pUserArn_ =
  UpdateTeamMember'
    { projectRole = Core.Nothing,
      remoteAccessAllowed = Core.Nothing,
      projectId = pProjectId_,
      userArn = pUserArn_
    }

-- | The role assigned to the user in the project. Project roles have
-- different levels of access. For more information, see
-- <http://docs.aws.amazon.com/codestar/latest/userguide/working-with-teams.html Working with Teams>
-- in the /AWS CodeStar User Guide/.
updateTeamMember_projectRole :: Lens.Lens' UpdateTeamMember (Core.Maybe Core.Text)
updateTeamMember_projectRole = Lens.lens (\UpdateTeamMember' {projectRole} -> projectRole) (\s@UpdateTeamMember' {} a -> s {projectRole = a} :: UpdateTeamMember)

-- | Whether a team member is allowed to remotely access project resources
-- using the SSH public key associated with the user\'s profile. Even if
-- this is set to True, the user must associate a public key with their
-- profile before the user can access resources.
updateTeamMember_remoteAccessAllowed :: Lens.Lens' UpdateTeamMember (Core.Maybe Core.Bool)
updateTeamMember_remoteAccessAllowed = Lens.lens (\UpdateTeamMember' {remoteAccessAllowed} -> remoteAccessAllowed) (\s@UpdateTeamMember' {} a -> s {remoteAccessAllowed = a} :: UpdateTeamMember)

-- | The ID of the project.
updateTeamMember_projectId :: Lens.Lens' UpdateTeamMember Core.Text
updateTeamMember_projectId = Lens.lens (\UpdateTeamMember' {projectId} -> projectId) (\s@UpdateTeamMember' {} a -> s {projectId = a} :: UpdateTeamMember)

-- | The Amazon Resource Name (ARN) of the user for whom you want to change
-- team membership attributes.
updateTeamMember_userArn :: Lens.Lens' UpdateTeamMember Core.Text
updateTeamMember_userArn = Lens.lens (\UpdateTeamMember' {userArn} -> userArn) (\s@UpdateTeamMember' {} a -> s {userArn = a} :: UpdateTeamMember)

instance Core.AWSRequest UpdateTeamMember where
  type
    AWSResponse UpdateTeamMember =
      UpdateTeamMemberResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTeamMemberResponse'
            Core.<$> (x Core..?> "userArn")
            Core.<*> (x Core..?> "projectRole")
            Core.<*> (x Core..?> "remoteAccessAllowed")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateTeamMember

instance Core.NFData UpdateTeamMember

instance Core.ToHeaders UpdateTeamMember where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeStar_20170419.UpdateTeamMember" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateTeamMember where
  toJSON UpdateTeamMember' {..} =
    Core.object
      ( Core.catMaybes
          [ ("projectRole" Core..=) Core.<$> projectRole,
            ("remoteAccessAllowed" Core..=)
              Core.<$> remoteAccessAllowed,
            Core.Just ("projectId" Core..= projectId),
            Core.Just ("userArn" Core..= userArn)
          ]
      )

instance Core.ToPath UpdateTeamMember where
  toPath = Core.const "/"

instance Core.ToQuery UpdateTeamMember where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateTeamMemberResponse' smart constructor.
data UpdateTeamMemberResponse = UpdateTeamMemberResponse'
  { -- | The Amazon Resource Name (ARN) of the user whose team membership
    -- attributes were updated.
    userArn :: Core.Maybe Core.Text,
    -- | The project role granted to the user.
    projectRole :: Core.Maybe Core.Text,
    -- | Whether a team member is allowed to remotely access project resources
    -- using the SSH public key associated with the user\'s profile.
    remoteAccessAllowed :: Core.Maybe Core.Bool,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateTeamMemberResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userArn', 'updateTeamMemberResponse_userArn' - The Amazon Resource Name (ARN) of the user whose team membership
-- attributes were updated.
--
-- 'projectRole', 'updateTeamMemberResponse_projectRole' - The project role granted to the user.
--
-- 'remoteAccessAllowed', 'updateTeamMemberResponse_remoteAccessAllowed' - Whether a team member is allowed to remotely access project resources
-- using the SSH public key associated with the user\'s profile.
--
-- 'httpStatus', 'updateTeamMemberResponse_httpStatus' - The response's http status code.
newUpdateTeamMemberResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateTeamMemberResponse
newUpdateTeamMemberResponse pHttpStatus_ =
  UpdateTeamMemberResponse'
    { userArn = Core.Nothing,
      projectRole = Core.Nothing,
      remoteAccessAllowed = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the user whose team membership
-- attributes were updated.
updateTeamMemberResponse_userArn :: Lens.Lens' UpdateTeamMemberResponse (Core.Maybe Core.Text)
updateTeamMemberResponse_userArn = Lens.lens (\UpdateTeamMemberResponse' {userArn} -> userArn) (\s@UpdateTeamMemberResponse' {} a -> s {userArn = a} :: UpdateTeamMemberResponse)

-- | The project role granted to the user.
updateTeamMemberResponse_projectRole :: Lens.Lens' UpdateTeamMemberResponse (Core.Maybe Core.Text)
updateTeamMemberResponse_projectRole = Lens.lens (\UpdateTeamMemberResponse' {projectRole} -> projectRole) (\s@UpdateTeamMemberResponse' {} a -> s {projectRole = a} :: UpdateTeamMemberResponse)

-- | Whether a team member is allowed to remotely access project resources
-- using the SSH public key associated with the user\'s profile.
updateTeamMemberResponse_remoteAccessAllowed :: Lens.Lens' UpdateTeamMemberResponse (Core.Maybe Core.Bool)
updateTeamMemberResponse_remoteAccessAllowed = Lens.lens (\UpdateTeamMemberResponse' {remoteAccessAllowed} -> remoteAccessAllowed) (\s@UpdateTeamMemberResponse' {} a -> s {remoteAccessAllowed = a} :: UpdateTeamMemberResponse)

-- | The response's http status code.
updateTeamMemberResponse_httpStatus :: Lens.Lens' UpdateTeamMemberResponse Core.Int
updateTeamMemberResponse_httpStatus = Lens.lens (\UpdateTeamMemberResponse' {httpStatus} -> httpStatus) (\s@UpdateTeamMemberResponse' {} a -> s {httpStatus = a} :: UpdateTeamMemberResponse)

instance Core.NFData UpdateTeamMemberResponse
