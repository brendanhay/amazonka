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
-- Module      : Amazonka.SSM.UpdateManagedInstanceRole
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the Identity and Access Management (IAM) role that is assigned
-- to the on-premises server, edge device, or virtual machines (VM). IAM
-- roles are first assigned to these hybrid nodes during the activation
-- process. For more information, see CreateActivation.
module Amazonka.SSM.UpdateManagedInstanceRole
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newUpdateManagedInstanceRole' smart constructor.
data UpdateManagedInstanceRole = UpdateManagedInstanceRole'
  { -- | The ID of the managed node where you want to update the role.
    instanceId :: Prelude.Text,
    -- | The name of the Identity and Access Management (IAM) role that you want
    -- to assign to the managed node. This IAM role must provide AssumeRole
    -- permissions for the Amazon Web Services Systems Manager service
    -- principal @ssm.amazonaws.com@. For more information, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-service-role.html Create an IAM service role for a hybrid environment>
    -- in the /Amazon Web Services Systems Manager User Guide/.
    --
    -- You can\'t specify an IAM service-linked role for this parameter. You
    -- must create a unique role.
    iamRole :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateManagedInstanceRole' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'updateManagedInstanceRole_instanceId' - The ID of the managed node where you want to update the role.
--
-- 'iamRole', 'updateManagedInstanceRole_iamRole' - The name of the Identity and Access Management (IAM) role that you want
-- to assign to the managed node. This IAM role must provide AssumeRole
-- permissions for the Amazon Web Services Systems Manager service
-- principal @ssm.amazonaws.com@. For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-service-role.html Create an IAM service role for a hybrid environment>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- You can\'t specify an IAM service-linked role for this parameter. You
-- must create a unique role.
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

-- | The ID of the managed node where you want to update the role.
updateManagedInstanceRole_instanceId :: Lens.Lens' UpdateManagedInstanceRole Prelude.Text
updateManagedInstanceRole_instanceId = Lens.lens (\UpdateManagedInstanceRole' {instanceId} -> instanceId) (\s@UpdateManagedInstanceRole' {} a -> s {instanceId = a} :: UpdateManagedInstanceRole)

-- | The name of the Identity and Access Management (IAM) role that you want
-- to assign to the managed node. This IAM role must provide AssumeRole
-- permissions for the Amazon Web Services Systems Manager service
-- principal @ssm.amazonaws.com@. For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-service-role.html Create an IAM service role for a hybrid environment>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- You can\'t specify an IAM service-linked role for this parameter. You
-- must create a unique role.
updateManagedInstanceRole_iamRole :: Lens.Lens' UpdateManagedInstanceRole Prelude.Text
updateManagedInstanceRole_iamRole = Lens.lens (\UpdateManagedInstanceRole' {iamRole} -> iamRole) (\s@UpdateManagedInstanceRole' {} a -> s {iamRole = a} :: UpdateManagedInstanceRole)

instance Core.AWSRequest UpdateManagedInstanceRole where
  type
    AWSResponse UpdateManagedInstanceRole =
      UpdateManagedInstanceRoleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateManagedInstanceRoleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateManagedInstanceRole where
  hashWithSalt _salt UpdateManagedInstanceRole' {..} =
    _salt
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` iamRole

instance Prelude.NFData UpdateManagedInstanceRole where
  rnf UpdateManagedInstanceRole' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf iamRole

instance Data.ToHeaders UpdateManagedInstanceRole where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.UpdateManagedInstanceRole" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateManagedInstanceRole where
  toJSON UpdateManagedInstanceRole' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("InstanceId" Data..= instanceId),
            Prelude.Just ("IamRole" Data..= iamRole)
          ]
      )

instance Data.ToPath UpdateManagedInstanceRole where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateManagedInstanceRole where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateManagedInstanceRoleResponse' smart constructor.
data UpdateManagedInstanceRoleResponse = UpdateManagedInstanceRoleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf UpdateManagedInstanceRoleResponse' {..} =
    Prelude.rnf httpStatus
