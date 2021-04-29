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
-- Module      : Network.AWS.CodeStar.DisassociateTeamMember
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a user from a project. Removing a user from a project also
-- removes the IAM policies from that user that allowed access to the
-- project and its resources. Disassociating a team member does not remove
-- that user\'s profile from AWS CodeStar. It does not remove the user from
-- IAM.
module Network.AWS.CodeStar.DisassociateTeamMember
  ( -- * Creating a Request
    DisassociateTeamMember (..),
    newDisassociateTeamMember,

    -- * Request Lenses
    disassociateTeamMember_projectId,
    disassociateTeamMember_userArn,

    -- * Destructuring the Response
    DisassociateTeamMemberResponse (..),
    newDisassociateTeamMemberResponse,

    -- * Response Lenses
    disassociateTeamMemberResponse_httpStatus,
  )
where

import Network.AWS.CodeStar.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisassociateTeamMember' smart constructor.
data DisassociateTeamMember = DisassociateTeamMember'
  { -- | The ID of the AWS CodeStar project from which you want to remove a team
    -- member.
    projectId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM user or group whom you want to
    -- remove from the project.
    userArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisassociateTeamMember' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'projectId', 'disassociateTeamMember_projectId' - The ID of the AWS CodeStar project from which you want to remove a team
-- member.
--
-- 'userArn', 'disassociateTeamMember_userArn' - The Amazon Resource Name (ARN) of the IAM user or group whom you want to
-- remove from the project.
newDisassociateTeamMember ::
  -- | 'projectId'
  Prelude.Text ->
  -- | 'userArn'
  Prelude.Text ->
  DisassociateTeamMember
newDisassociateTeamMember pProjectId_ pUserArn_ =
  DisassociateTeamMember'
    { projectId = pProjectId_,
      userArn = pUserArn_
    }

-- | The ID of the AWS CodeStar project from which you want to remove a team
-- member.
disassociateTeamMember_projectId :: Lens.Lens' DisassociateTeamMember Prelude.Text
disassociateTeamMember_projectId = Lens.lens (\DisassociateTeamMember' {projectId} -> projectId) (\s@DisassociateTeamMember' {} a -> s {projectId = a} :: DisassociateTeamMember)

-- | The Amazon Resource Name (ARN) of the IAM user or group whom you want to
-- remove from the project.
disassociateTeamMember_userArn :: Lens.Lens' DisassociateTeamMember Prelude.Text
disassociateTeamMember_userArn = Lens.lens (\DisassociateTeamMember' {userArn} -> userArn) (\s@DisassociateTeamMember' {} a -> s {userArn = a} :: DisassociateTeamMember)

instance Prelude.AWSRequest DisassociateTeamMember where
  type
    Rs DisassociateTeamMember =
      DisassociateTeamMemberResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateTeamMemberResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateTeamMember

instance Prelude.NFData DisassociateTeamMember

instance Prelude.ToHeaders DisassociateTeamMember where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeStar_20170419.DisassociateTeamMember" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DisassociateTeamMember where
  toJSON DisassociateTeamMember' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("projectId" Prelude..= projectId),
            Prelude.Just ("userArn" Prelude..= userArn)
          ]
      )

instance Prelude.ToPath DisassociateTeamMember where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DisassociateTeamMember where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateTeamMemberResponse' smart constructor.
data DisassociateTeamMemberResponse = DisassociateTeamMemberResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisassociateTeamMemberResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateTeamMemberResponse_httpStatus' - The response's http status code.
newDisassociateTeamMemberResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateTeamMemberResponse
newDisassociateTeamMemberResponse pHttpStatus_ =
  DisassociateTeamMemberResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disassociateTeamMemberResponse_httpStatus :: Lens.Lens' DisassociateTeamMemberResponse Prelude.Int
disassociateTeamMemberResponse_httpStatus = Lens.lens (\DisassociateTeamMemberResponse' {httpStatus} -> httpStatus) (\s@DisassociateTeamMemberResponse' {} a -> s {httpStatus = a} :: DisassociateTeamMemberResponse)

instance
  Prelude.NFData
    DisassociateTeamMemberResponse
