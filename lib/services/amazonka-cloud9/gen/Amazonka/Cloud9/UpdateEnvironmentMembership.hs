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
-- Module      : Amazonka.Cloud9.UpdateEnvironmentMembership
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the settings of an existing environment member for an Cloud9
-- development environment.
module Amazonka.Cloud9.UpdateEnvironmentMembership
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

import Amazonka.Cloud9.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateEnvironmentMembership' smart constructor.
data UpdateEnvironmentMembership = UpdateEnvironmentMembership'
  { -- | The ID of the environment for the environment member whose settings you
    -- want to change.
    environmentId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the environment member whose settings
    -- you want to change.
    userArn :: Prelude.Text,
    -- | The replacement type of environment member permissions you want to
    -- associate with this environment member. Available values include:
    --
    -- -   @read-only@: Has read-only access to the environment.
    --
    -- -   @read-write@: Has read-write access to the environment.
    permissions :: MemberPermissions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'userArn'
  Prelude.Text ->
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
updateEnvironmentMembership_environmentId :: Lens.Lens' UpdateEnvironmentMembership Prelude.Text
updateEnvironmentMembership_environmentId = Lens.lens (\UpdateEnvironmentMembership' {environmentId} -> environmentId) (\s@UpdateEnvironmentMembership' {} a -> s {environmentId = a} :: UpdateEnvironmentMembership)

-- | The Amazon Resource Name (ARN) of the environment member whose settings
-- you want to change.
updateEnvironmentMembership_userArn :: Lens.Lens' UpdateEnvironmentMembership Prelude.Text
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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateEnvironmentMembershipResponse'
            Prelude.<$> (x Data..?> "membership")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateEnvironmentMembership where
  hashWithSalt _salt UpdateEnvironmentMembership' {..} =
    _salt `Prelude.hashWithSalt` environmentId
      `Prelude.hashWithSalt` userArn
      `Prelude.hashWithSalt` permissions

instance Prelude.NFData UpdateEnvironmentMembership where
  rnf UpdateEnvironmentMembership' {..} =
    Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf userArn
      `Prelude.seq` Prelude.rnf permissions

instance Data.ToHeaders UpdateEnvironmentMembership where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCloud9WorkspaceManagementService.UpdateEnvironmentMembership" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateEnvironmentMembership where
  toJSON UpdateEnvironmentMembership' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("environmentId" Data..= environmentId),
            Prelude.Just ("userArn" Data..= userArn),
            Prelude.Just ("permissions" Data..= permissions)
          ]
      )

instance Data.ToPath UpdateEnvironmentMembership where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateEnvironmentMembership where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateEnvironmentMembershipResponse' smart constructor.
data UpdateEnvironmentMembershipResponse = UpdateEnvironmentMembershipResponse'
  { -- | Information about the environment member whose settings were changed.
    membership :: Prelude.Maybe EnvironmentMember,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UpdateEnvironmentMembershipResponse
newUpdateEnvironmentMembershipResponse pHttpStatus_ =
  UpdateEnvironmentMembershipResponse'
    { membership =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the environment member whose settings were changed.
updateEnvironmentMembershipResponse_membership :: Lens.Lens' UpdateEnvironmentMembershipResponse (Prelude.Maybe EnvironmentMember)
updateEnvironmentMembershipResponse_membership = Lens.lens (\UpdateEnvironmentMembershipResponse' {membership} -> membership) (\s@UpdateEnvironmentMembershipResponse' {} a -> s {membership = a} :: UpdateEnvironmentMembershipResponse)

-- | The response's http status code.
updateEnvironmentMembershipResponse_httpStatus :: Lens.Lens' UpdateEnvironmentMembershipResponse Prelude.Int
updateEnvironmentMembershipResponse_httpStatus = Lens.lens (\UpdateEnvironmentMembershipResponse' {httpStatus} -> httpStatus) (\s@UpdateEnvironmentMembershipResponse' {} a -> s {httpStatus = a} :: UpdateEnvironmentMembershipResponse)

instance
  Prelude.NFData
    UpdateEnvironmentMembershipResponse
  where
  rnf UpdateEnvironmentMembershipResponse' {..} =
    Prelude.rnf membership
      `Prelude.seq` Prelude.rnf httpStatus
