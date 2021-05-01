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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newUpdateManagedInstanceRole' smart constructor.
data UpdateManagedInstanceRole = UpdateManagedInstanceRole'
  { -- | The ID of the managed instance where you want to update the role.
    instanceId :: Prelude.Text,
    -- | The IAM role you want to assign or change.
    iamRole :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'iamRole'
  Prelude.Text ->
  UpdateManagedInstanceRole
newUpdateManagedInstanceRole pInstanceId_ pIamRole_ =
  UpdateManagedInstanceRole'
    { instanceId =
        pInstanceId_,
      iamRole = pIamRole_
    }

-- | The ID of the managed instance where you want to update the role.
updateManagedInstanceRole_instanceId :: Lens.Lens' UpdateManagedInstanceRole Prelude.Text
updateManagedInstanceRole_instanceId = Lens.lens (\UpdateManagedInstanceRole' {instanceId} -> instanceId) (\s@UpdateManagedInstanceRole' {} a -> s {instanceId = a} :: UpdateManagedInstanceRole)

-- | The IAM role you want to assign or change.
updateManagedInstanceRole_iamRole :: Lens.Lens' UpdateManagedInstanceRole Prelude.Text
updateManagedInstanceRole_iamRole = Lens.lens (\UpdateManagedInstanceRole' {iamRole} -> iamRole) (\s@UpdateManagedInstanceRole' {} a -> s {iamRole = a} :: UpdateManagedInstanceRole)

instance Prelude.AWSRequest UpdateManagedInstanceRole where
  type
    Rs UpdateManagedInstanceRole =
      UpdateManagedInstanceRoleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateManagedInstanceRoleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateManagedInstanceRole

instance Prelude.NFData UpdateManagedInstanceRole

instance Prelude.ToHeaders UpdateManagedInstanceRole where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonSSM.UpdateManagedInstanceRole" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateManagedInstanceRole where
  toJSON UpdateManagedInstanceRole' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("InstanceId" Prelude..= instanceId),
            Prelude.Just ("IamRole" Prelude..= iamRole)
          ]
      )

instance Prelude.ToPath UpdateManagedInstanceRole where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateManagedInstanceRole where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateManagedInstanceRoleResponse' smart constructor.
data UpdateManagedInstanceRoleResponse = UpdateManagedInstanceRoleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  UpdateManagedInstanceRoleResponse
newUpdateManagedInstanceRoleResponse pHttpStatus_ =
  UpdateManagedInstanceRoleResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateManagedInstanceRoleResponse_httpStatus :: Lens.Lens' UpdateManagedInstanceRoleResponse Prelude.Int
updateManagedInstanceRoleResponse_httpStatus = Lens.lens (\UpdateManagedInstanceRoleResponse' {httpStatus} -> httpStatus) (\s@UpdateManagedInstanceRoleResponse' {} a -> s {httpStatus = a} :: UpdateManagedInstanceRoleResponse)

instance
  Prelude.NFData
    UpdateManagedInstanceRoleResponse
