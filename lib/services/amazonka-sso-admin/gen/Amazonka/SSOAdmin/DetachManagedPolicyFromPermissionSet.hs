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
-- Module      : Amazonka.SSOAdmin.DetachManagedPolicyFromPermissionSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches the attached AWS managed policy ARN from the specified
-- permission set.
module Amazonka.SSOAdmin.DetachManagedPolicyFromPermissionSet
  ( -- * Creating a Request
    DetachManagedPolicyFromPermissionSet (..),
    newDetachManagedPolicyFromPermissionSet,

    -- * Request Lenses
    detachManagedPolicyFromPermissionSet_instanceArn,
    detachManagedPolicyFromPermissionSet_permissionSetArn,
    detachManagedPolicyFromPermissionSet_managedPolicyArn,

    -- * Destructuring the Response
    DetachManagedPolicyFromPermissionSetResponse (..),
    newDetachManagedPolicyFromPermissionSetResponse,

    -- * Response Lenses
    detachManagedPolicyFromPermissionSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSOAdmin.Types

-- | /See:/ 'newDetachManagedPolicyFromPermissionSet' smart constructor.
data DetachManagedPolicyFromPermissionSet = DetachManagedPolicyFromPermissionSet'
  { -- | The ARN of the IAM Identity Center instance under which the operation
    -- will be executed. For more information about ARNs, see
    -- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
    -- in the /AWS General Reference/.
    instanceArn :: Prelude.Text,
    -- | The ARN of the PermissionSet from which the policy should be detached.
    permissionSetArn :: Prelude.Text,
    -- | The AWS managed policy ARN to be detached from a permission set.
    managedPolicyArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetachManagedPolicyFromPermissionSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceArn', 'detachManagedPolicyFromPermissionSet_instanceArn' - The ARN of the IAM Identity Center instance under which the operation
-- will be executed. For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
-- in the /AWS General Reference/.
--
-- 'permissionSetArn', 'detachManagedPolicyFromPermissionSet_permissionSetArn' - The ARN of the PermissionSet from which the policy should be detached.
--
-- 'managedPolicyArn', 'detachManagedPolicyFromPermissionSet_managedPolicyArn' - The AWS managed policy ARN to be detached from a permission set.
newDetachManagedPolicyFromPermissionSet ::
  -- | 'instanceArn'
  Prelude.Text ->
  -- | 'permissionSetArn'
  Prelude.Text ->
  -- | 'managedPolicyArn'
  Prelude.Text ->
  DetachManagedPolicyFromPermissionSet
newDetachManagedPolicyFromPermissionSet
  pInstanceArn_
  pPermissionSetArn_
  pManagedPolicyArn_ =
    DetachManagedPolicyFromPermissionSet'
      { instanceArn =
          pInstanceArn_,
        permissionSetArn = pPermissionSetArn_,
        managedPolicyArn = pManagedPolicyArn_
      }

-- | The ARN of the IAM Identity Center instance under which the operation
-- will be executed. For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
-- in the /AWS General Reference/.
detachManagedPolicyFromPermissionSet_instanceArn :: Lens.Lens' DetachManagedPolicyFromPermissionSet Prelude.Text
detachManagedPolicyFromPermissionSet_instanceArn = Lens.lens (\DetachManagedPolicyFromPermissionSet' {instanceArn} -> instanceArn) (\s@DetachManagedPolicyFromPermissionSet' {} a -> s {instanceArn = a} :: DetachManagedPolicyFromPermissionSet)

-- | The ARN of the PermissionSet from which the policy should be detached.
detachManagedPolicyFromPermissionSet_permissionSetArn :: Lens.Lens' DetachManagedPolicyFromPermissionSet Prelude.Text
detachManagedPolicyFromPermissionSet_permissionSetArn = Lens.lens (\DetachManagedPolicyFromPermissionSet' {permissionSetArn} -> permissionSetArn) (\s@DetachManagedPolicyFromPermissionSet' {} a -> s {permissionSetArn = a} :: DetachManagedPolicyFromPermissionSet)

-- | The AWS managed policy ARN to be detached from a permission set.
detachManagedPolicyFromPermissionSet_managedPolicyArn :: Lens.Lens' DetachManagedPolicyFromPermissionSet Prelude.Text
detachManagedPolicyFromPermissionSet_managedPolicyArn = Lens.lens (\DetachManagedPolicyFromPermissionSet' {managedPolicyArn} -> managedPolicyArn) (\s@DetachManagedPolicyFromPermissionSet' {} a -> s {managedPolicyArn = a} :: DetachManagedPolicyFromPermissionSet)

instance
  Core.AWSRequest
    DetachManagedPolicyFromPermissionSet
  where
  type
    AWSResponse DetachManagedPolicyFromPermissionSet =
      DetachManagedPolicyFromPermissionSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DetachManagedPolicyFromPermissionSetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DetachManagedPolicyFromPermissionSet
  where
  hashWithSalt
    _salt
    DetachManagedPolicyFromPermissionSet' {..} =
      _salt
        `Prelude.hashWithSalt` instanceArn
        `Prelude.hashWithSalt` permissionSetArn
        `Prelude.hashWithSalt` managedPolicyArn

instance
  Prelude.NFData
    DetachManagedPolicyFromPermissionSet
  where
  rnf DetachManagedPolicyFromPermissionSet' {..} =
    Prelude.rnf instanceArn `Prelude.seq`
      Prelude.rnf permissionSetArn `Prelude.seq`
        Prelude.rnf managedPolicyArn

instance
  Data.ToHeaders
    DetachManagedPolicyFromPermissionSet
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SWBExternalService.DetachManagedPolicyFromPermissionSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DetachManagedPolicyFromPermissionSet
  where
  toJSON DetachManagedPolicyFromPermissionSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("InstanceArn" Data..= instanceArn),
            Prelude.Just
              ("PermissionSetArn" Data..= permissionSetArn),
            Prelude.Just
              ("ManagedPolicyArn" Data..= managedPolicyArn)
          ]
      )

instance
  Data.ToPath
    DetachManagedPolicyFromPermissionSet
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DetachManagedPolicyFromPermissionSet
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDetachManagedPolicyFromPermissionSetResponse' smart constructor.
data DetachManagedPolicyFromPermissionSetResponse = DetachManagedPolicyFromPermissionSetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetachManagedPolicyFromPermissionSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'detachManagedPolicyFromPermissionSetResponse_httpStatus' - The response's http status code.
newDetachManagedPolicyFromPermissionSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DetachManagedPolicyFromPermissionSetResponse
newDetachManagedPolicyFromPermissionSetResponse
  pHttpStatus_ =
    DetachManagedPolicyFromPermissionSetResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
detachManagedPolicyFromPermissionSetResponse_httpStatus :: Lens.Lens' DetachManagedPolicyFromPermissionSetResponse Prelude.Int
detachManagedPolicyFromPermissionSetResponse_httpStatus = Lens.lens (\DetachManagedPolicyFromPermissionSetResponse' {httpStatus} -> httpStatus) (\s@DetachManagedPolicyFromPermissionSetResponse' {} a -> s {httpStatus = a} :: DetachManagedPolicyFromPermissionSetResponse)

instance
  Prelude.NFData
    DetachManagedPolicyFromPermissionSetResponse
  where
  rnf DetachManagedPolicyFromPermissionSetResponse' {..} =
    Prelude.rnf httpStatus
