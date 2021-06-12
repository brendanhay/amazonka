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
-- Module      : Network.AWS.Cloud9.UpdateEnvironmentMembership
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the settings of an existing environment member for an AWS Cloud9
-- development environment.
module Network.AWS.Cloud9.UpdateEnvironmentMembership
  ( -- * Creating a Request
    UpdateEnvironmentMembership (..),
    newUpdateEnvironmentMembership,

    -- * Request Lenses
    updateEnvironmentMembership_environmentId,
    updateEnvironmentMembership_userArn,
    updateEnvironmentMembership_permissions,

    -- * Destructuring the Response
    UpdateEnvironmentMembershipResponse (..),
    newUpdateEnvironmentMembershipResponse,

    -- * Response Lenses
    updateEnvironmentMembershipResponse_membership,
    updateEnvironmentMembershipResponse_httpStatus,
  )
where

import Network.AWS.Cloud9.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateEnvironmentMembership' smart constructor.
data UpdateEnvironmentMembership = UpdateEnvironmentMembership'
  { -- | The ID of the environment for the environment member whose settings you
    -- want to change.
    environmentId :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the environment member whose settings
    -- you want to change.
    userArn :: Core.Text,
    -- | The replacement type of environment member permissions you want to
    -- associate with this environment member. Available values include:
    --
    -- -   @read-only@: Has read-only access to the environment.
    --
    -- -   @read-write@: Has read-write access to the environment.
    permissions :: MemberPermissions
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateEnvironmentMembership' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentId', 'updateEnvironmentMembership_environmentId' - The ID of the environment for the environment member whose settings you
-- want to change.
--
-- 'userArn', 'updateEnvironmentMembership_userArn' - The Amazon Resource Name (ARN) of the environment member whose settings
-- you want to change.
--
-- 'permissions', 'updateEnvironmentMembership_permissions' - The replacement type of environment member permissions you want to
-- associate with this environment member. Available values include:
--
-- -   @read-only@: Has read-only access to the environment.
--
-- -   @read-write@: Has read-write access to the environment.
newUpdateEnvironmentMembership ::
  -- | 'environmentId'
  Core.Text ->
  -- | 'userArn'
  Core.Text ->
  -- | 'permissions'
  MemberPermissions ->
  UpdateEnvironmentMembership
newUpdateEnvironmentMembership
  pEnvironmentId_
  pUserArn_
  pPermissions_ =
    UpdateEnvironmentMembership'
      { environmentId =
          pEnvironmentId_,
        userArn = pUserArn_,
        permissions = pPermissions_
      }

-- | The ID of the environment for the environment member whose settings you
-- want to change.
updateEnvironmentMembership_environmentId :: Lens.Lens' UpdateEnvironmentMembership Core.Text
updateEnvironmentMembership_environmentId = Lens.lens (\UpdateEnvironmentMembership' {environmentId} -> environmentId) (\s@UpdateEnvironmentMembership' {} a -> s {environmentId = a} :: UpdateEnvironmentMembership)

-- | The Amazon Resource Name (ARN) of the environment member whose settings
-- you want to change.
updateEnvironmentMembership_userArn :: Lens.Lens' UpdateEnvironmentMembership Core.Text
updateEnvironmentMembership_userArn = Lens.lens (\UpdateEnvironmentMembership' {userArn} -> userArn) (\s@UpdateEnvironmentMembership' {} a -> s {userArn = a} :: UpdateEnvironmentMembership)

-- | The replacement type of environment member permissions you want to
-- associate with this environment member. Available values include:
--
-- -   @read-only@: Has read-only access to the environment.
--
-- -   @read-write@: Has read-write access to the environment.
updateEnvironmentMembership_permissions :: Lens.Lens' UpdateEnvironmentMembership MemberPermissions
updateEnvironmentMembership_permissions = Lens.lens (\UpdateEnvironmentMembership' {permissions} -> permissions) (\s@UpdateEnvironmentMembership' {} a -> s {permissions = a} :: UpdateEnvironmentMembership)

instance Core.AWSRequest UpdateEnvironmentMembership where
  type
    AWSResponse UpdateEnvironmentMembership =
      UpdateEnvironmentMembershipResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateEnvironmentMembershipResponse'
            Core.<$> (x Core..?> "membership")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateEnvironmentMembership

instance Core.NFData UpdateEnvironmentMembership

instance Core.ToHeaders UpdateEnvironmentMembership where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCloud9WorkspaceManagementService.UpdateEnvironmentMembership" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateEnvironmentMembership where
  toJSON UpdateEnvironmentMembership' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("environmentId" Core..= environmentId),
            Core.Just ("userArn" Core..= userArn),
            Core.Just ("permissions" Core..= permissions)
          ]
      )

instance Core.ToPath UpdateEnvironmentMembership where
  toPath = Core.const "/"

instance Core.ToQuery UpdateEnvironmentMembership where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateEnvironmentMembershipResponse' smart constructor.
data UpdateEnvironmentMembershipResponse = UpdateEnvironmentMembershipResponse'
  { -- | Information about the environment member whose settings were changed.
    membership :: Core.Maybe EnvironmentMember,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateEnvironmentMembershipResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'membership', 'updateEnvironmentMembershipResponse_membership' - Information about the environment member whose settings were changed.
--
-- 'httpStatus', 'updateEnvironmentMembershipResponse_httpStatus' - The response's http status code.
newUpdateEnvironmentMembershipResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateEnvironmentMembershipResponse
newUpdateEnvironmentMembershipResponse pHttpStatus_ =
  UpdateEnvironmentMembershipResponse'
    { membership =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the environment member whose settings were changed.
updateEnvironmentMembershipResponse_membership :: Lens.Lens' UpdateEnvironmentMembershipResponse (Core.Maybe EnvironmentMember)
updateEnvironmentMembershipResponse_membership = Lens.lens (\UpdateEnvironmentMembershipResponse' {membership} -> membership) (\s@UpdateEnvironmentMembershipResponse' {} a -> s {membership = a} :: UpdateEnvironmentMembershipResponse)

-- | The response's http status code.
updateEnvironmentMembershipResponse_httpStatus :: Lens.Lens' UpdateEnvironmentMembershipResponse Core.Int
updateEnvironmentMembershipResponse_httpStatus = Lens.lens (\UpdateEnvironmentMembershipResponse' {httpStatus} -> httpStatus) (\s@UpdateEnvironmentMembershipResponse' {} a -> s {httpStatus = a} :: UpdateEnvironmentMembershipResponse)

instance
  Core.NFData
    UpdateEnvironmentMembershipResponse
