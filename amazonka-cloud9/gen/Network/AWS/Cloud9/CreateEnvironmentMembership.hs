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
-- Module      : Network.AWS.Cloud9.CreateEnvironmentMembership
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an environment member to an AWS Cloud9 development environment.
module Network.AWS.Cloud9.CreateEnvironmentMembership
  ( -- * Creating a Request
    CreateEnvironmentMembership (..),
    newCreateEnvironmentMembership,

    -- * Request Lenses
    createEnvironmentMembership_environmentId,
    createEnvironmentMembership_userArn,
    createEnvironmentMembership_permissions,

    -- * Destructuring the Response
    CreateEnvironmentMembershipResponse (..),
    newCreateEnvironmentMembershipResponse,

    -- * Response Lenses
    createEnvironmentMembershipResponse_membership,
    createEnvironmentMembershipResponse_httpStatus,
  )
where

import Network.AWS.Cloud9.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateEnvironmentMembership' smart constructor.
data CreateEnvironmentMembership = CreateEnvironmentMembership'
  { -- | The ID of the environment that contains the environment member you want
    -- to add.
    environmentId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the environment member you want to
    -- add.
    userArn :: Prelude.Text,
    -- | The type of environment member permissions you want to associate with
    -- this environment member. Available values include:
    --
    -- -   @read-only@: Has read-only access to the environment.
    --
    -- -   @read-write@: Has read-write access to the environment.
    permissions :: MemberPermissions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEnvironmentMembership' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentId', 'createEnvironmentMembership_environmentId' - The ID of the environment that contains the environment member you want
-- to add.
--
-- 'userArn', 'createEnvironmentMembership_userArn' - The Amazon Resource Name (ARN) of the environment member you want to
-- add.
--
-- 'permissions', 'createEnvironmentMembership_permissions' - The type of environment member permissions you want to associate with
-- this environment member. Available values include:
--
-- -   @read-only@: Has read-only access to the environment.
--
-- -   @read-write@: Has read-write access to the environment.
newCreateEnvironmentMembership ::
  -- | 'environmentId'
  Prelude.Text ->
  -- | 'userArn'
  Prelude.Text ->
  -- | 'permissions'
  MemberPermissions ->
  CreateEnvironmentMembership
newCreateEnvironmentMembership
  pEnvironmentId_
  pUserArn_
  pPermissions_ =
    CreateEnvironmentMembership'
      { environmentId =
          pEnvironmentId_,
        userArn = pUserArn_,
        permissions = pPermissions_
      }

-- | The ID of the environment that contains the environment member you want
-- to add.
createEnvironmentMembership_environmentId :: Lens.Lens' CreateEnvironmentMembership Prelude.Text
createEnvironmentMembership_environmentId = Lens.lens (\CreateEnvironmentMembership' {environmentId} -> environmentId) (\s@CreateEnvironmentMembership' {} a -> s {environmentId = a} :: CreateEnvironmentMembership)

-- | The Amazon Resource Name (ARN) of the environment member you want to
-- add.
createEnvironmentMembership_userArn :: Lens.Lens' CreateEnvironmentMembership Prelude.Text
createEnvironmentMembership_userArn = Lens.lens (\CreateEnvironmentMembership' {userArn} -> userArn) (\s@CreateEnvironmentMembership' {} a -> s {userArn = a} :: CreateEnvironmentMembership)

-- | The type of environment member permissions you want to associate with
-- this environment member. Available values include:
--
-- -   @read-only@: Has read-only access to the environment.
--
-- -   @read-write@: Has read-write access to the environment.
createEnvironmentMembership_permissions :: Lens.Lens' CreateEnvironmentMembership MemberPermissions
createEnvironmentMembership_permissions = Lens.lens (\CreateEnvironmentMembership' {permissions} -> permissions) (\s@CreateEnvironmentMembership' {} a -> s {permissions = a} :: CreateEnvironmentMembership)

instance Core.AWSRequest CreateEnvironmentMembership where
  type
    AWSResponse CreateEnvironmentMembership =
      CreateEnvironmentMembershipResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEnvironmentMembershipResponse'
            Prelude.<$> (x Core..?> "membership")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateEnvironmentMembership

instance Prelude.NFData CreateEnvironmentMembership

instance Core.ToHeaders CreateEnvironmentMembership where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCloud9WorkspaceManagementService.CreateEnvironmentMembership" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateEnvironmentMembership where
  toJSON CreateEnvironmentMembership' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("environmentId" Core..= environmentId),
            Prelude.Just ("userArn" Core..= userArn),
            Prelude.Just ("permissions" Core..= permissions)
          ]
      )

instance Core.ToPath CreateEnvironmentMembership where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateEnvironmentMembership where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateEnvironmentMembershipResponse' smart constructor.
data CreateEnvironmentMembershipResponse = CreateEnvironmentMembershipResponse'
  { -- | Information about the environment member that was added.
    membership :: Prelude.Maybe EnvironmentMember,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEnvironmentMembershipResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'membership', 'createEnvironmentMembershipResponse_membership' - Information about the environment member that was added.
--
-- 'httpStatus', 'createEnvironmentMembershipResponse_httpStatus' - The response's http status code.
newCreateEnvironmentMembershipResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateEnvironmentMembershipResponse
newCreateEnvironmentMembershipResponse pHttpStatus_ =
  CreateEnvironmentMembershipResponse'
    { membership =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the environment member that was added.
createEnvironmentMembershipResponse_membership :: Lens.Lens' CreateEnvironmentMembershipResponse (Prelude.Maybe EnvironmentMember)
createEnvironmentMembershipResponse_membership = Lens.lens (\CreateEnvironmentMembershipResponse' {membership} -> membership) (\s@CreateEnvironmentMembershipResponse' {} a -> s {membership = a} :: CreateEnvironmentMembershipResponse)

-- | The response's http status code.
createEnvironmentMembershipResponse_httpStatus :: Lens.Lens' CreateEnvironmentMembershipResponse Prelude.Int
createEnvironmentMembershipResponse_httpStatus = Lens.lens (\CreateEnvironmentMembershipResponse' {httpStatus} -> httpStatus) (\s@CreateEnvironmentMembershipResponse' {} a -> s {httpStatus = a} :: CreateEnvironmentMembershipResponse)

instance
  Prelude.NFData
    CreateEnvironmentMembershipResponse
