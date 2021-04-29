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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateTeamMember' smart constructor.
data AssociateTeamMember = AssociateTeamMember'
  { -- | Whether the team member is allowed to use an SSH public\/private key
    -- pair to remotely access project resources, for example Amazon EC2
    -- instances.
    remoteAccessAllowed :: Prelude.Maybe Prelude.Bool,
    -- | A user- or system-generated token that identifies the entity that
    -- requested the team member association to the project. This token can be
    -- used to repeat the request.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the project to which you will add the IAM user.
    projectId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the IAM user you want to add to the
    -- AWS CodeStar project.
    userArn :: Prelude.Text,
    -- | The AWS CodeStar project role that will apply to this user. This role
    -- determines what actions a user can take in an AWS CodeStar project.
    projectRole :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'userArn'
  Prelude.Text ->
  -- | 'projectRole'
  Prelude.Text ->
  AssociateTeamMember
newAssociateTeamMember
  pProjectId_
  pUserArn_
  pProjectRole_ =
    AssociateTeamMember'
      { remoteAccessAllowed =
          Prelude.Nothing,
        clientRequestToken = Prelude.Nothing,
        projectId = pProjectId_,
        userArn = pUserArn_,
        projectRole = pProjectRole_
      }

-- | Whether the team member is allowed to use an SSH public\/private key
-- pair to remotely access project resources, for example Amazon EC2
-- instances.
associateTeamMember_remoteAccessAllowed :: Lens.Lens' AssociateTeamMember (Prelude.Maybe Prelude.Bool)
associateTeamMember_remoteAccessAllowed = Lens.lens (\AssociateTeamMember' {remoteAccessAllowed} -> remoteAccessAllowed) (\s@AssociateTeamMember' {} a -> s {remoteAccessAllowed = a} :: AssociateTeamMember)

-- | A user- or system-generated token that identifies the entity that
-- requested the team member association to the project. This token can be
-- used to repeat the request.
associateTeamMember_clientRequestToken :: Lens.Lens' AssociateTeamMember (Prelude.Maybe Prelude.Text)
associateTeamMember_clientRequestToken = Lens.lens (\AssociateTeamMember' {clientRequestToken} -> clientRequestToken) (\s@AssociateTeamMember' {} a -> s {clientRequestToken = a} :: AssociateTeamMember)

-- | The ID of the project to which you will add the IAM user.
associateTeamMember_projectId :: Lens.Lens' AssociateTeamMember Prelude.Text
associateTeamMember_projectId = Lens.lens (\AssociateTeamMember' {projectId} -> projectId) (\s@AssociateTeamMember' {} a -> s {projectId = a} :: AssociateTeamMember)

-- | The Amazon Resource Name (ARN) for the IAM user you want to add to the
-- AWS CodeStar project.
associateTeamMember_userArn :: Lens.Lens' AssociateTeamMember Prelude.Text
associateTeamMember_userArn = Lens.lens (\AssociateTeamMember' {userArn} -> userArn) (\s@AssociateTeamMember' {} a -> s {userArn = a} :: AssociateTeamMember)

-- | The AWS CodeStar project role that will apply to this user. This role
-- determines what actions a user can take in an AWS CodeStar project.
associateTeamMember_projectRole :: Lens.Lens' AssociateTeamMember Prelude.Text
associateTeamMember_projectRole = Lens.lens (\AssociateTeamMember' {projectRole} -> projectRole) (\s@AssociateTeamMember' {} a -> s {projectRole = a} :: AssociateTeamMember)

instance Prelude.AWSRequest AssociateTeamMember where
  type
    Rs AssociateTeamMember =
      AssociateTeamMemberResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateTeamMemberResponse'
            Prelude.<$> (x Prelude..?> "clientRequestToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateTeamMember

instance Prelude.NFData AssociateTeamMember

instance Prelude.ToHeaders AssociateTeamMember where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeStar_20170419.AssociateTeamMember" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AssociateTeamMember where
  toJSON AssociateTeamMember' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("remoteAccessAllowed" Prelude..=)
              Prelude.<$> remoteAccessAllowed,
            ("clientRequestToken" Prelude..=)
              Prelude.<$> clientRequestToken,
            Prelude.Just ("projectId" Prelude..= projectId),
            Prelude.Just ("userArn" Prelude..= userArn),
            Prelude.Just ("projectRole" Prelude..= projectRole)
          ]
      )

instance Prelude.ToPath AssociateTeamMember where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AssociateTeamMember where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateTeamMemberResponse' smart constructor.
data AssociateTeamMemberResponse = AssociateTeamMemberResponse'
  { -- | The user- or system-generated token from the initial request that can be
    -- used to repeat the request.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  AssociateTeamMemberResponse
newAssociateTeamMemberResponse pHttpStatus_ =
  AssociateTeamMemberResponse'
    { clientRequestToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The user- or system-generated token from the initial request that can be
-- used to repeat the request.
associateTeamMemberResponse_clientRequestToken :: Lens.Lens' AssociateTeamMemberResponse (Prelude.Maybe Prelude.Text)
associateTeamMemberResponse_clientRequestToken = Lens.lens (\AssociateTeamMemberResponse' {clientRequestToken} -> clientRequestToken) (\s@AssociateTeamMemberResponse' {} a -> s {clientRequestToken = a} :: AssociateTeamMemberResponse)

-- | The response's http status code.
associateTeamMemberResponse_httpStatus :: Lens.Lens' AssociateTeamMemberResponse Prelude.Int
associateTeamMemberResponse_httpStatus = Lens.lens (\AssociateTeamMemberResponse' {httpStatus} -> httpStatus) (\s@AssociateTeamMemberResponse' {} a -> s {httpStatus = a} :: AssociateTeamMemberResponse)

instance Prelude.NFData AssociateTeamMemberResponse
