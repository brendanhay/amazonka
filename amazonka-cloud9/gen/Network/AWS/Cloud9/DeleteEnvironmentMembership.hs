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
-- Module      : Network.AWS.Cloud9.DeleteEnvironmentMembership
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an environment member from an AWS Cloud9 development
-- environment.
module Network.AWS.Cloud9.DeleteEnvironmentMembership
  ( -- * Creating a Request
    DeleteEnvironmentMembership (..),
    newDeleteEnvironmentMembership,

    -- * Request Lenses
    deleteEnvironmentMembership_environmentId,
    deleteEnvironmentMembership_userArn,

    -- * Destructuring the Response
    DeleteEnvironmentMembershipResponse (..),
    newDeleteEnvironmentMembershipResponse,

    -- * Response Lenses
    deleteEnvironmentMembershipResponse_httpStatus,
  )
where

import Network.AWS.Cloud9.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteEnvironmentMembership' smart constructor.
data DeleteEnvironmentMembership = DeleteEnvironmentMembership'
  { -- | The ID of the environment to delete the environment member from.
    environmentId :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the environment member to delete from
    -- the environment.
    userArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteEnvironmentMembership' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentId', 'deleteEnvironmentMembership_environmentId' - The ID of the environment to delete the environment member from.
--
-- 'userArn', 'deleteEnvironmentMembership_userArn' - The Amazon Resource Name (ARN) of the environment member to delete from
-- the environment.
newDeleteEnvironmentMembership ::
  -- | 'environmentId'
  Core.Text ->
  -- | 'userArn'
  Core.Text ->
  DeleteEnvironmentMembership
newDeleteEnvironmentMembership
  pEnvironmentId_
  pUserArn_ =
    DeleteEnvironmentMembership'
      { environmentId =
          pEnvironmentId_,
        userArn = pUserArn_
      }

-- | The ID of the environment to delete the environment member from.
deleteEnvironmentMembership_environmentId :: Lens.Lens' DeleteEnvironmentMembership Core.Text
deleteEnvironmentMembership_environmentId = Lens.lens (\DeleteEnvironmentMembership' {environmentId} -> environmentId) (\s@DeleteEnvironmentMembership' {} a -> s {environmentId = a} :: DeleteEnvironmentMembership)

-- | The Amazon Resource Name (ARN) of the environment member to delete from
-- the environment.
deleteEnvironmentMembership_userArn :: Lens.Lens' DeleteEnvironmentMembership Core.Text
deleteEnvironmentMembership_userArn = Lens.lens (\DeleteEnvironmentMembership' {userArn} -> userArn) (\s@DeleteEnvironmentMembership' {} a -> s {userArn = a} :: DeleteEnvironmentMembership)

instance Core.AWSRequest DeleteEnvironmentMembership where
  type
    AWSResponse DeleteEnvironmentMembership =
      DeleteEnvironmentMembershipResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteEnvironmentMembershipResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteEnvironmentMembership

instance Core.NFData DeleteEnvironmentMembership

instance Core.ToHeaders DeleteEnvironmentMembership where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCloud9WorkspaceManagementService.DeleteEnvironmentMembership" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteEnvironmentMembership where
  toJSON DeleteEnvironmentMembership' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("environmentId" Core..= environmentId),
            Core.Just ("userArn" Core..= userArn)
          ]
      )

instance Core.ToPath DeleteEnvironmentMembership where
  toPath = Core.const "/"

instance Core.ToQuery DeleteEnvironmentMembership where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteEnvironmentMembershipResponse' smart constructor.
data DeleteEnvironmentMembershipResponse = DeleteEnvironmentMembershipResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteEnvironmentMembershipResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteEnvironmentMembershipResponse_httpStatus' - The response's http status code.
newDeleteEnvironmentMembershipResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteEnvironmentMembershipResponse
newDeleteEnvironmentMembershipResponse pHttpStatus_ =
  DeleteEnvironmentMembershipResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteEnvironmentMembershipResponse_httpStatus :: Lens.Lens' DeleteEnvironmentMembershipResponse Core.Int
deleteEnvironmentMembershipResponse_httpStatus = Lens.lens (\DeleteEnvironmentMembershipResponse' {httpStatus} -> httpStatus) (\s@DeleteEnvironmentMembershipResponse' {} a -> s {httpStatus = a} :: DeleteEnvironmentMembershipResponse)

instance
  Core.NFData
    DeleteEnvironmentMembershipResponse
