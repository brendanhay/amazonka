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
-- Module      : Amazonka.CodeStar.AssociateTeamMember
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an IAM user to the team for an AWS CodeStar project.
module Amazonka.CodeStar.AssociateTeamMember
  ( -- * Creating a Request
    AssociateTeamMember (..),
    newAssociateTeamMember,

    -- * Request Lenses
    associateTeamMember_clientRequestToken,
    associateTeamMember_remoteAccessAllowed,
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

import Amazonka.CodeStar.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateTeamMember' smart constructor.
data AssociateTeamMember = AssociateTeamMember'
  { -- | A user- or system-generated token that identifies the entity that
    -- requested the team member association to the project. This token can be
    -- used to repeat the request.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | Whether the team member is allowed to use an SSH public\/private key
    -- pair to remotely access project resources, for example Amazon EC2
    -- instances.
    remoteAccessAllowed :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the project to which you will add the IAM user.
    projectId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the IAM user you want to add to the
    -- AWS CodeStar project.
    userArn :: Prelude.Text,
    -- | The AWS CodeStar project role that will apply to this user. This role
    -- determines what actions a user can take in an AWS CodeStar project.
    projectRole :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateTeamMember' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'associateTeamMember_clientRequestToken' - A user- or system-generated token that identifies the entity that
-- requested the team member association to the project. This token can be
-- used to repeat the request.
--
-- 'remoteAccessAllowed', 'associateTeamMember_remoteAccessAllowed' - Whether the team member is allowed to use an SSH public\/private key
-- pair to remotely access project resources, for example Amazon EC2
-- instances.
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
      { clientRequestToken =
          Prelude.Nothing,
        remoteAccessAllowed = Prelude.Nothing,
        projectId = pProjectId_,
        userArn = pUserArn_,
        projectRole = pProjectRole_
      }

-- | A user- or system-generated token that identifies the entity that
-- requested the team member association to the project. This token can be
-- used to repeat the request.
associateTeamMember_clientRequestToken :: Lens.Lens' AssociateTeamMember (Prelude.Maybe Prelude.Text)
associateTeamMember_clientRequestToken = Lens.lens (\AssociateTeamMember' {clientRequestToken} -> clientRequestToken) (\s@AssociateTeamMember' {} a -> s {clientRequestToken = a} :: AssociateTeamMember)

-- | Whether the team member is allowed to use an SSH public\/private key
-- pair to remotely access project resources, for example Amazon EC2
-- instances.
associateTeamMember_remoteAccessAllowed :: Lens.Lens' AssociateTeamMember (Prelude.Maybe Prelude.Bool)
associateTeamMember_remoteAccessAllowed = Lens.lens (\AssociateTeamMember' {remoteAccessAllowed} -> remoteAccessAllowed) (\s@AssociateTeamMember' {} a -> s {remoteAccessAllowed = a} :: AssociateTeamMember)

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

instance Core.AWSRequest AssociateTeamMember where
  type
    AWSResponse AssociateTeamMember =
      AssociateTeamMemberResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateTeamMemberResponse'
            Prelude.<$> (x Data..?> "clientRequestToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateTeamMember where
  hashWithSalt _salt AssociateTeamMember' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` remoteAccessAllowed
      `Prelude.hashWithSalt` projectId
      `Prelude.hashWithSalt` userArn
      `Prelude.hashWithSalt` projectRole

instance Prelude.NFData AssociateTeamMember where
  rnf AssociateTeamMember' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf remoteAccessAllowed
      `Prelude.seq` Prelude.rnf projectId
      `Prelude.seq` Prelude.rnf userArn
      `Prelude.seq` Prelude.rnf projectRole

instance Data.ToHeaders AssociateTeamMember where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeStar_20170419.AssociateTeamMember" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateTeamMember where
  toJSON AssociateTeamMember' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("remoteAccessAllowed" Data..=)
              Prelude.<$> remoteAccessAllowed,
            Prelude.Just ("projectId" Data..= projectId),
            Prelude.Just ("userArn" Data..= userArn),
            Prelude.Just ("projectRole" Data..= projectRole)
          ]
      )

instance Data.ToPath AssociateTeamMember where
  toPath = Prelude.const "/"

instance Data.ToQuery AssociateTeamMember where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateTeamMemberResponse' smart constructor.
data AssociateTeamMemberResponse = AssociateTeamMemberResponse'
  { -- | The user- or system-generated token from the initial request that can be
    -- used to repeat the request.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData AssociateTeamMemberResponse where
  rnf AssociateTeamMemberResponse' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf httpStatus
