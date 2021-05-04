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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateTeamMember' smart constructor.
data UpdateTeamMember = UpdateTeamMember'
  { -- | The role assigned to the user in the project. Project roles have
    -- different levels of access. For more information, see
    -- <http://docs.aws.amazon.com/codestar/latest/userguide/working-with-teams.html Working with Teams>
    -- in the /AWS CodeStar User Guide/.
    projectRole :: Prelude.Maybe Prelude.Text,
    -- | Whether a team member is allowed to remotely access project resources
    -- using the SSH public key associated with the user\'s profile. Even if
    -- this is set to True, the user must associate a public key with their
    -- profile before the user can access resources.
    remoteAccessAllowed :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the project.
    projectId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the user for whom you want to change
    -- team membership attributes.
    userArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'userArn'
  Prelude.Text ->
  UpdateTeamMember
newUpdateTeamMember pProjectId_ pUserArn_ =
  UpdateTeamMember'
    { projectRole = Prelude.Nothing,
      remoteAccessAllowed = Prelude.Nothing,
      projectId = pProjectId_,
      userArn = pUserArn_
    }

-- | The role assigned to the user in the project. Project roles have
-- different levels of access. For more information, see
-- <http://docs.aws.amazon.com/codestar/latest/userguide/working-with-teams.html Working with Teams>
-- in the /AWS CodeStar User Guide/.
updateTeamMember_projectRole :: Lens.Lens' UpdateTeamMember (Prelude.Maybe Prelude.Text)
updateTeamMember_projectRole = Lens.lens (\UpdateTeamMember' {projectRole} -> projectRole) (\s@UpdateTeamMember' {} a -> s {projectRole = a} :: UpdateTeamMember)

-- | Whether a team member is allowed to remotely access project resources
-- using the SSH public key associated with the user\'s profile. Even if
-- this is set to True, the user must associate a public key with their
-- profile before the user can access resources.
updateTeamMember_remoteAccessAllowed :: Lens.Lens' UpdateTeamMember (Prelude.Maybe Prelude.Bool)
updateTeamMember_remoteAccessAllowed = Lens.lens (\UpdateTeamMember' {remoteAccessAllowed} -> remoteAccessAllowed) (\s@UpdateTeamMember' {} a -> s {remoteAccessAllowed = a} :: UpdateTeamMember)

-- | The ID of the project.
updateTeamMember_projectId :: Lens.Lens' UpdateTeamMember Prelude.Text
updateTeamMember_projectId = Lens.lens (\UpdateTeamMember' {projectId} -> projectId) (\s@UpdateTeamMember' {} a -> s {projectId = a} :: UpdateTeamMember)

-- | The Amazon Resource Name (ARN) of the user for whom you want to change
-- team membership attributes.
updateTeamMember_userArn :: Lens.Lens' UpdateTeamMember Prelude.Text
updateTeamMember_userArn = Lens.lens (\UpdateTeamMember' {userArn} -> userArn) (\s@UpdateTeamMember' {} a -> s {userArn = a} :: UpdateTeamMember)

instance Prelude.AWSRequest UpdateTeamMember where
  type Rs UpdateTeamMember = UpdateTeamMemberResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTeamMemberResponse'
            Prelude.<$> (x Prelude..?> "userArn")
            Prelude.<*> (x Prelude..?> "projectRole")
            Prelude.<*> (x Prelude..?> "remoteAccessAllowed")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateTeamMember

instance Prelude.NFData UpdateTeamMember

instance Prelude.ToHeaders UpdateTeamMember where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeStar_20170419.UpdateTeamMember" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateTeamMember where
  toJSON UpdateTeamMember' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("projectRole" Prelude..=) Prelude.<$> projectRole,
            ("remoteAccessAllowed" Prelude..=)
              Prelude.<$> remoteAccessAllowed,
            Prelude.Just ("projectId" Prelude..= projectId),
            Prelude.Just ("userArn" Prelude..= userArn)
          ]
      )

instance Prelude.ToPath UpdateTeamMember where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateTeamMember where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateTeamMemberResponse' smart constructor.
data UpdateTeamMemberResponse = UpdateTeamMemberResponse'
  { -- | The Amazon Resource Name (ARN) of the user whose team membership
    -- attributes were updated.
    userArn :: Prelude.Maybe Prelude.Text,
    -- | The project role granted to the user.
    projectRole :: Prelude.Maybe Prelude.Text,
    -- | Whether a team member is allowed to remotely access project resources
    -- using the SSH public key associated with the user\'s profile.
    remoteAccessAllowed :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  UpdateTeamMemberResponse
newUpdateTeamMemberResponse pHttpStatus_ =
  UpdateTeamMemberResponse'
    { userArn =
        Prelude.Nothing,
      projectRole = Prelude.Nothing,
      remoteAccessAllowed = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the user whose team membership
-- attributes were updated.
updateTeamMemberResponse_userArn :: Lens.Lens' UpdateTeamMemberResponse (Prelude.Maybe Prelude.Text)
updateTeamMemberResponse_userArn = Lens.lens (\UpdateTeamMemberResponse' {userArn} -> userArn) (\s@UpdateTeamMemberResponse' {} a -> s {userArn = a} :: UpdateTeamMemberResponse)

-- | The project role granted to the user.
updateTeamMemberResponse_projectRole :: Lens.Lens' UpdateTeamMemberResponse (Prelude.Maybe Prelude.Text)
updateTeamMemberResponse_projectRole = Lens.lens (\UpdateTeamMemberResponse' {projectRole} -> projectRole) (\s@UpdateTeamMemberResponse' {} a -> s {projectRole = a} :: UpdateTeamMemberResponse)

-- | Whether a team member is allowed to remotely access project resources
-- using the SSH public key associated with the user\'s profile.
updateTeamMemberResponse_remoteAccessAllowed :: Lens.Lens' UpdateTeamMemberResponse (Prelude.Maybe Prelude.Bool)
updateTeamMemberResponse_remoteAccessAllowed = Lens.lens (\UpdateTeamMemberResponse' {remoteAccessAllowed} -> remoteAccessAllowed) (\s@UpdateTeamMemberResponse' {} a -> s {remoteAccessAllowed = a} :: UpdateTeamMemberResponse)

-- | The response's http status code.
updateTeamMemberResponse_httpStatus :: Lens.Lens' UpdateTeamMemberResponse Prelude.Int
updateTeamMemberResponse_httpStatus = Lens.lens (\UpdateTeamMemberResponse' {httpStatus} -> httpStatus) (\s@UpdateTeamMemberResponse' {} a -> s {httpStatus = a} :: UpdateTeamMemberResponse)

instance Prelude.NFData UpdateTeamMemberResponse
