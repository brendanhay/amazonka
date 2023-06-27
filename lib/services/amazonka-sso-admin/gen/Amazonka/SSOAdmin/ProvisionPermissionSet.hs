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
-- Module      : Amazonka.SSOAdmin.ProvisionPermissionSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The process by which a specified permission set is provisioned to the
-- specified target.
module Amazonka.SSOAdmin.ProvisionPermissionSet
  ( -- * Creating a Request
    ProvisionPermissionSet (..),
    newProvisionPermissionSet,

    -- * Request Lenses
    provisionPermissionSet_targetId,
    provisionPermissionSet_instanceArn,
    provisionPermissionSet_permissionSetArn,
    provisionPermissionSet_targetType,

    -- * Destructuring the Response
    ProvisionPermissionSetResponse (..),
    newProvisionPermissionSetResponse,

    -- * Response Lenses
    provisionPermissionSetResponse_permissionSetProvisioningStatus,
    provisionPermissionSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSOAdmin.Types

-- | /See:/ 'newProvisionPermissionSet' smart constructor.
data ProvisionPermissionSet = ProvisionPermissionSet'
  { -- | TargetID is an AWS account identifier, typically a 10-12 digit string
    -- (For example, 123456789012).
    targetId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the IAM Identity Center instance under which the operation
    -- will be executed. For more information about ARNs, see
    -- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
    -- in the /AWS General Reference/.
    instanceArn :: Prelude.Text,
    -- | The ARN of the permission set.
    permissionSetArn :: Prelude.Text,
    -- | The entity type for which the assignment will be created.
    targetType :: ProvisionTargetType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProvisionPermissionSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetId', 'provisionPermissionSet_targetId' - TargetID is an AWS account identifier, typically a 10-12 digit string
-- (For example, 123456789012).
--
-- 'instanceArn', 'provisionPermissionSet_instanceArn' - The ARN of the IAM Identity Center instance under which the operation
-- will be executed. For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
-- in the /AWS General Reference/.
--
-- 'permissionSetArn', 'provisionPermissionSet_permissionSetArn' - The ARN of the permission set.
--
-- 'targetType', 'provisionPermissionSet_targetType' - The entity type for which the assignment will be created.
newProvisionPermissionSet ::
  -- | 'instanceArn'
  Prelude.Text ->
  -- | 'permissionSetArn'
  Prelude.Text ->
  -- | 'targetType'
  ProvisionTargetType ->
  ProvisionPermissionSet
newProvisionPermissionSet
  pInstanceArn_
  pPermissionSetArn_
  pTargetType_ =
    ProvisionPermissionSet'
      { targetId = Prelude.Nothing,
        instanceArn = pInstanceArn_,
        permissionSetArn = pPermissionSetArn_,
        targetType = pTargetType_
      }

-- | TargetID is an AWS account identifier, typically a 10-12 digit string
-- (For example, 123456789012).
provisionPermissionSet_targetId :: Lens.Lens' ProvisionPermissionSet (Prelude.Maybe Prelude.Text)
provisionPermissionSet_targetId = Lens.lens (\ProvisionPermissionSet' {targetId} -> targetId) (\s@ProvisionPermissionSet' {} a -> s {targetId = a} :: ProvisionPermissionSet)

-- | The ARN of the IAM Identity Center instance under which the operation
-- will be executed. For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
-- in the /AWS General Reference/.
provisionPermissionSet_instanceArn :: Lens.Lens' ProvisionPermissionSet Prelude.Text
provisionPermissionSet_instanceArn = Lens.lens (\ProvisionPermissionSet' {instanceArn} -> instanceArn) (\s@ProvisionPermissionSet' {} a -> s {instanceArn = a} :: ProvisionPermissionSet)

-- | The ARN of the permission set.
provisionPermissionSet_permissionSetArn :: Lens.Lens' ProvisionPermissionSet Prelude.Text
provisionPermissionSet_permissionSetArn = Lens.lens (\ProvisionPermissionSet' {permissionSetArn} -> permissionSetArn) (\s@ProvisionPermissionSet' {} a -> s {permissionSetArn = a} :: ProvisionPermissionSet)

-- | The entity type for which the assignment will be created.
provisionPermissionSet_targetType :: Lens.Lens' ProvisionPermissionSet ProvisionTargetType
provisionPermissionSet_targetType = Lens.lens (\ProvisionPermissionSet' {targetType} -> targetType) (\s@ProvisionPermissionSet' {} a -> s {targetType = a} :: ProvisionPermissionSet)

instance Core.AWSRequest ProvisionPermissionSet where
  type
    AWSResponse ProvisionPermissionSet =
      ProvisionPermissionSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ProvisionPermissionSetResponse'
            Prelude.<$> (x Data..?> "PermissionSetProvisioningStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ProvisionPermissionSet where
  hashWithSalt _salt ProvisionPermissionSet' {..} =
    _salt
      `Prelude.hashWithSalt` targetId
      `Prelude.hashWithSalt` instanceArn
      `Prelude.hashWithSalt` permissionSetArn
      `Prelude.hashWithSalt` targetType

instance Prelude.NFData ProvisionPermissionSet where
  rnf ProvisionPermissionSet' {..} =
    Prelude.rnf targetId
      `Prelude.seq` Prelude.rnf instanceArn
      `Prelude.seq` Prelude.rnf permissionSetArn
      `Prelude.seq` Prelude.rnf targetType

instance Data.ToHeaders ProvisionPermissionSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SWBExternalService.ProvisionPermissionSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ProvisionPermissionSet where
  toJSON ProvisionPermissionSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TargetId" Data..=) Prelude.<$> targetId,
            Prelude.Just ("InstanceArn" Data..= instanceArn),
            Prelude.Just
              ("PermissionSetArn" Data..= permissionSetArn),
            Prelude.Just ("TargetType" Data..= targetType)
          ]
      )

instance Data.ToPath ProvisionPermissionSet where
  toPath = Prelude.const "/"

instance Data.ToQuery ProvisionPermissionSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newProvisionPermissionSetResponse' smart constructor.
data ProvisionPermissionSetResponse = ProvisionPermissionSetResponse'
  { -- | The status object for the permission set provisioning operation.
    permissionSetProvisioningStatus :: Prelude.Maybe PermissionSetProvisioningStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProvisionPermissionSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'permissionSetProvisioningStatus', 'provisionPermissionSetResponse_permissionSetProvisioningStatus' - The status object for the permission set provisioning operation.
--
-- 'httpStatus', 'provisionPermissionSetResponse_httpStatus' - The response's http status code.
newProvisionPermissionSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ProvisionPermissionSetResponse
newProvisionPermissionSetResponse pHttpStatus_ =
  ProvisionPermissionSetResponse'
    { permissionSetProvisioningStatus =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status object for the permission set provisioning operation.
provisionPermissionSetResponse_permissionSetProvisioningStatus :: Lens.Lens' ProvisionPermissionSetResponse (Prelude.Maybe PermissionSetProvisioningStatus)
provisionPermissionSetResponse_permissionSetProvisioningStatus = Lens.lens (\ProvisionPermissionSetResponse' {permissionSetProvisioningStatus} -> permissionSetProvisioningStatus) (\s@ProvisionPermissionSetResponse' {} a -> s {permissionSetProvisioningStatus = a} :: ProvisionPermissionSetResponse)

-- | The response's http status code.
provisionPermissionSetResponse_httpStatus :: Lens.Lens' ProvisionPermissionSetResponse Prelude.Int
provisionPermissionSetResponse_httpStatus = Lens.lens (\ProvisionPermissionSetResponse' {httpStatus} -> httpStatus) (\s@ProvisionPermissionSetResponse' {} a -> s {httpStatus = a} :: ProvisionPermissionSetResponse)

instance
  Prelude.NFData
    ProvisionPermissionSetResponse
  where
  rnf ProvisionPermissionSetResponse' {..} =
    Prelude.rnf permissionSetProvisioningStatus
      `Prelude.seq` Prelude.rnf httpStatus
