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
-- Module      : Amazonka.Cloud9.CreateEnvironmentMembership
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an environment member to an Cloud9 development environment.
module Amazonka.Cloud9.CreateEnvironmentMembership
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
    createEnvironmentMembershipResponse_httpStatus,
    createEnvironmentMembershipResponse_membership,
  )
where

import Amazonka.Cloud9.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEnvironmentMembershipResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "membership")
      )

instance Prelude.Hashable CreateEnvironmentMembership where
  hashWithSalt _salt CreateEnvironmentMembership' {..} =
    _salt `Prelude.hashWithSalt` environmentId
      `Prelude.hashWithSalt` userArn
      `Prelude.hashWithSalt` permissions

instance Prelude.NFData CreateEnvironmentMembership where
  rnf CreateEnvironmentMembership' {..} =
    Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf userArn
      `Prelude.seq` Prelude.rnf permissions

instance Data.ToHeaders CreateEnvironmentMembership where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCloud9WorkspaceManagementService.CreateEnvironmentMembership" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateEnvironmentMembership where
  toJSON CreateEnvironmentMembership' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("environmentId" Data..= environmentId),
            Prelude.Just ("userArn" Data..= userArn),
            Prelude.Just ("permissions" Data..= permissions)
          ]
      )

instance Data.ToPath CreateEnvironmentMembership where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateEnvironmentMembership where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateEnvironmentMembershipResponse' smart constructor.
data CreateEnvironmentMembershipResponse = CreateEnvironmentMembershipResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about the environment member that was added.
    membership :: EnvironmentMember
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
-- 'httpStatus', 'createEnvironmentMembershipResponse_httpStatus' - The response's http status code.
--
-- 'membership', 'createEnvironmentMembershipResponse_membership' - Information about the environment member that was added.
newCreateEnvironmentMembershipResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'membership'
  EnvironmentMember ->
  CreateEnvironmentMembershipResponse
newCreateEnvironmentMembershipResponse
  pHttpStatus_
  pMembership_ =
    CreateEnvironmentMembershipResponse'
      { httpStatus =
          pHttpStatus_,
        membership = pMembership_
      }

-- | The response's http status code.
createEnvironmentMembershipResponse_httpStatus :: Lens.Lens' CreateEnvironmentMembershipResponse Prelude.Int
createEnvironmentMembershipResponse_httpStatus = Lens.lens (\CreateEnvironmentMembershipResponse' {httpStatus} -> httpStatus) (\s@CreateEnvironmentMembershipResponse' {} a -> s {httpStatus = a} :: CreateEnvironmentMembershipResponse)

-- | Information about the environment member that was added.
createEnvironmentMembershipResponse_membership :: Lens.Lens' CreateEnvironmentMembershipResponse EnvironmentMember
createEnvironmentMembershipResponse_membership = Lens.lens (\CreateEnvironmentMembershipResponse' {membership} -> membership) (\s@CreateEnvironmentMembershipResponse' {} a -> s {membership = a} :: CreateEnvironmentMembershipResponse)

instance
  Prelude.NFData
    CreateEnvironmentMembershipResponse
  where
  rnf CreateEnvironmentMembershipResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf membership
