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
-- Module      : Network.AWS.CodeStar.AssociateTeamMember
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an IAM user to the team for an AWS CodeStar project.
module Network.AWS.CodeStar.AssociateTeamMember
  ( -- * Creating a Request
    AssociateTeamMember (..),
    newAssociateTeamMember,

    -- * Request Lenses
    associateTeamMember_remoteAccessAllowed,
    associateTeamMember_clientRequestToken,
    associateTeamMember_projectId,
    associateTeamMember_userArn,
    associateTeamMember_projectRole,

    -- * Destructuring the Response
    AssociateTeamMemberResponse (..),
    newAssociateTeamMemberResponse,

    -- * Response Lenses
    associateTeamMemberResponse_clientRequestToken,
    associateTeamMemberResponse_httpStatus,
  )
where

import Network.AWS.CodeStar.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateTeamMember' smart constructor.
data AssociateTeamMember = AssociateTeamMember'
  { -- | Whether the team member is allowed to use an SSH public\/private key
    -- pair to remotely access project resources, for example Amazon EC2
    -- instances.
    remoteAccessAllowed :: Core.Maybe Core.Bool,
    -- | A user- or system-generated token that identifies the entity that
    -- requested the team member association to the project. This token can be
    -- used to repeat the request.
    clientRequestToken :: Core.Maybe Core.Text,
    -- | The ID of the project to which you will add the IAM user.
    projectId :: Core.Text,
    -- | The Amazon Resource Name (ARN) for the IAM user you want to add to the
    -- AWS CodeStar project.
    userArn :: Core.Text,
    -- | The AWS CodeStar project role that will apply to this user. This role
    -- determines what actions a user can take in an AWS CodeStar project.
    projectRole :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateTeamMember' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'remoteAccessAllowed', 'associateTeamMember_remoteAccessAllowed' - Whether the team member is allowed to use an SSH public\/private key
-- pair to remotely access project resources, for example Amazon EC2
-- instances.
--
-- 'clientRequestToken', 'associateTeamMember_clientRequestToken' - A user- or system-generated token that identifies the entity that
-- requested the team member association to the project. This token can be
-- used to repeat the request.
--
-- 'projectId', 'associateTeamMember_projectId' - The ID of the project to which you will add the IAM user.
--
-- 'userArn', 'associateTeamMember_userArn' - The Amazon Resource Name (ARN) for the IAM user you want to add to the
-- AWS CodeStar project.
--
-- 'projectRole', 'associateTeamMember_projectRole' - The AWS CodeStar project role that will apply to this user. This role
-- determines what actions a user can take in an AWS CodeStar project.
newAssociateTeamMember ::
  -- | 'projectId'
  Core.Text ->
  -- | 'userArn'
  Core.Text ->
  -- | 'projectRole'
  Core.Text ->
  AssociateTeamMember
newAssociateTeamMember
  pProjectId_
  pUserArn_
  pProjectRole_ =
    AssociateTeamMember'
      { remoteAccessAllowed =
          Core.Nothing,
        clientRequestToken = Core.Nothing,
        projectId = pProjectId_,
        userArn = pUserArn_,
        projectRole = pProjectRole_
      }

-- | Whether the team member is allowed to use an SSH public\/private key
-- pair to remotely access project resources, for example Amazon EC2
-- instances.
associateTeamMember_remoteAccessAllowed :: Lens.Lens' AssociateTeamMember (Core.Maybe Core.Bool)
associateTeamMember_remoteAccessAllowed = Lens.lens (\AssociateTeamMember' {remoteAccessAllowed} -> remoteAccessAllowed) (\s@AssociateTeamMember' {} a -> s {remoteAccessAllowed = a} :: AssociateTeamMember)

-- | A user- or system-generated token that identifies the entity that
-- requested the team member association to the project. This token can be
-- used to repeat the request.
associateTeamMember_clientRequestToken :: Lens.Lens' AssociateTeamMember (Core.Maybe Core.Text)
associateTeamMember_clientRequestToken = Lens.lens (\AssociateTeamMember' {clientRequestToken} -> clientRequestToken) (\s@AssociateTeamMember' {} a -> s {clientRequestToken = a} :: AssociateTeamMember)

-- | The ID of the project to which you will add the IAM user.
associateTeamMember_projectId :: Lens.Lens' AssociateTeamMember Core.Text
associateTeamMember_projectId = Lens.lens (\AssociateTeamMember' {projectId} -> projectId) (\s@AssociateTeamMember' {} a -> s {projectId = a} :: AssociateTeamMember)

-- | The Amazon Resource Name (ARN) for the IAM user you want to add to the
-- AWS CodeStar project.
associateTeamMember_userArn :: Lens.Lens' AssociateTeamMember Core.Text
associateTeamMember_userArn = Lens.lens (\AssociateTeamMember' {userArn} -> userArn) (\s@AssociateTeamMember' {} a -> s {userArn = a} :: AssociateTeamMember)

-- | The AWS CodeStar project role that will apply to this user. This role
-- determines what actions a user can take in an AWS CodeStar project.
associateTeamMember_projectRole :: Lens.Lens' AssociateTeamMember Core.Text
associateTeamMember_projectRole = Lens.lens (\AssociateTeamMember' {projectRole} -> projectRole) (\s@AssociateTeamMember' {} a -> s {projectRole = a} :: AssociateTeamMember)

instance Core.AWSRequest AssociateTeamMember where
  type
    AWSResponse AssociateTeamMember =
      AssociateTeamMemberResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateTeamMemberResponse'
            Core.<$> (x Core..?> "clientRequestToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AssociateTeamMember

instance Core.NFData AssociateTeamMember

instance Core.ToHeaders AssociateTeamMember where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeStar_20170419.AssociateTeamMember" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AssociateTeamMember where
  toJSON AssociateTeamMember' {..} =
    Core.object
      ( Core.catMaybes
          [ ("remoteAccessAllowed" Core..=)
              Core.<$> remoteAccessAllowed,
            ("clientRequestToken" Core..=)
              Core.<$> clientRequestToken,
            Core.Just ("projectId" Core..= projectId),
            Core.Just ("userArn" Core..= userArn),
            Core.Just ("projectRole" Core..= projectRole)
          ]
      )

instance Core.ToPath AssociateTeamMember where
  toPath = Core.const "/"

instance Core.ToQuery AssociateTeamMember where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAssociateTeamMemberResponse' smart constructor.
data AssociateTeamMemberResponse = AssociateTeamMemberResponse'
  { -- | The user- or system-generated token from the initial request that can be
    -- used to repeat the request.
    clientRequestToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateTeamMemberResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'associateTeamMemberResponse_clientRequestToken' - The user- or system-generated token from the initial request that can be
-- used to repeat the request.
--
-- 'httpStatus', 'associateTeamMemberResponse_httpStatus' - The response's http status code.
newAssociateTeamMemberResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AssociateTeamMemberResponse
newAssociateTeamMemberResponse pHttpStatus_ =
  AssociateTeamMemberResponse'
    { clientRequestToken =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The user- or system-generated token from the initial request that can be
-- used to repeat the request.
associateTeamMemberResponse_clientRequestToken :: Lens.Lens' AssociateTeamMemberResponse (Core.Maybe Core.Text)
associateTeamMemberResponse_clientRequestToken = Lens.lens (\AssociateTeamMemberResponse' {clientRequestToken} -> clientRequestToken) (\s@AssociateTeamMemberResponse' {} a -> s {clientRequestToken = a} :: AssociateTeamMemberResponse)

-- | The response's http status code.
associateTeamMemberResponse_httpStatus :: Lens.Lens' AssociateTeamMemberResponse Core.Int
associateTeamMemberResponse_httpStatus = Lens.lens (\AssociateTeamMemberResponse' {httpStatus} -> httpStatus) (\s@AssociateTeamMemberResponse' {} a -> s {httpStatus = a} :: AssociateTeamMemberResponse)

instance Core.NFData AssociateTeamMemberResponse
