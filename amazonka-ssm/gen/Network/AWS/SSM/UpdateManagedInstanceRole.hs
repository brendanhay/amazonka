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
-- Module      : Network.AWS.SSM.UpdateManagedInstanceRole
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the Amazon Identity and Access Management (IAM) role that is
-- assigned to the on-premises instance or virtual machines (VM). IAM roles
-- are first assigned to these hybrid instances during the activation
-- process. For more information, see CreateActivation.
module Network.AWS.SSM.UpdateManagedInstanceRole
  ( -- * Creating a Request
    UpdateManagedInstanceRole (..),
    newUpdateManagedInstanceRole,

    -- * Request Lenses
    updateManagedInstanceRole_instanceId,
    updateManagedInstanceRole_iamRole,

    -- * Destructuring the Response
    UpdateManagedInstanceRoleResponse (..),
    newUpdateManagedInstanceRoleResponse,

    -- * Response Lenses
    updateManagedInstanceRoleResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newUpdateManagedInstanceRole' smart constructor.
data UpdateManagedInstanceRole = UpdateManagedInstanceRole'
  { -- | The ID of the managed instance where you want to update the role.
    instanceId :: Core.Text,
    -- | The IAM role you want to assign or change.
    iamRole :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateManagedInstanceRole' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'updateManagedInstanceRole_instanceId' - The ID of the managed instance where you want to update the role.
--
-- 'iamRole', 'updateManagedInstanceRole_iamRole' - The IAM role you want to assign or change.
newUpdateManagedInstanceRole ::
  -- | 'instanceId'
  Core.Text ->
  -- | 'iamRole'
  Core.Text ->
  UpdateManagedInstanceRole
newUpdateManagedInstanceRole pInstanceId_ pIamRole_ =
  UpdateManagedInstanceRole'
    { instanceId =
        pInstanceId_,
      iamRole = pIamRole_
    }

-- | The ID of the managed instance where you want to update the role.
updateManagedInstanceRole_instanceId :: Lens.Lens' UpdateManagedInstanceRole Core.Text
updateManagedInstanceRole_instanceId = Lens.lens (\UpdateManagedInstanceRole' {instanceId} -> instanceId) (\s@UpdateManagedInstanceRole' {} a -> s {instanceId = a} :: UpdateManagedInstanceRole)

-- | The IAM role you want to assign or change.
updateManagedInstanceRole_iamRole :: Lens.Lens' UpdateManagedInstanceRole Core.Text
updateManagedInstanceRole_iamRole = Lens.lens (\UpdateManagedInstanceRole' {iamRole} -> iamRole) (\s@UpdateManagedInstanceRole' {} a -> s {iamRole = a} :: UpdateManagedInstanceRole)

instance Core.AWSRequest UpdateManagedInstanceRole where
  type
    AWSResponse UpdateManagedInstanceRole =
      UpdateManagedInstanceRoleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateManagedInstanceRoleResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateManagedInstanceRole

instance Core.NFData UpdateManagedInstanceRole

instance Core.ToHeaders UpdateManagedInstanceRole where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.UpdateManagedInstanceRole" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateManagedInstanceRole where
  toJSON UpdateManagedInstanceRole' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("InstanceId" Core..= instanceId),
            Core.Just ("IamRole" Core..= iamRole)
          ]
      )

instance Core.ToPath UpdateManagedInstanceRole where
  toPath = Core.const "/"

instance Core.ToQuery UpdateManagedInstanceRole where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateManagedInstanceRoleResponse' smart constructor.
data UpdateManagedInstanceRoleResponse = UpdateManagedInstanceRoleResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateManagedInstanceRoleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateManagedInstanceRoleResponse_httpStatus' - The response's http status code.
newUpdateManagedInstanceRoleResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateManagedInstanceRoleResponse
newUpdateManagedInstanceRoleResponse pHttpStatus_ =
  UpdateManagedInstanceRoleResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateManagedInstanceRoleResponse_httpStatus :: Lens.Lens' UpdateManagedInstanceRoleResponse Core.Int
updateManagedInstanceRoleResponse_httpStatus = Lens.lens (\UpdateManagedInstanceRoleResponse' {httpStatus} -> httpStatus) (\s@UpdateManagedInstanceRoleResponse' {} a -> s {httpStatus = a} :: UpdateManagedInstanceRoleResponse)

instance
  Core.NFData
    UpdateManagedInstanceRoleResponse
