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
-- Module      : Amazonka.SSOAdmin.DetachCustomerManagedPolicyReferenceFromPermissionSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches the specified customer managed policy from the specified
-- PermissionSet.
module Amazonka.SSOAdmin.DetachCustomerManagedPolicyReferenceFromPermissionSet
  ( -- * Creating a Request
    DetachCustomerManagedPolicyReferenceFromPermissionSet (..),
    newDetachCustomerManagedPolicyReferenceFromPermissionSet,

    -- * Request Lenses
    detachCustomerManagedPolicyReferenceFromPermissionSet_instanceArn,
    detachCustomerManagedPolicyReferenceFromPermissionSet_permissionSetArn,
    detachCustomerManagedPolicyReferenceFromPermissionSet_customerManagedPolicyReference,

    -- * Destructuring the Response
    DetachCustomerManagedPolicyReferenceFromPermissionSetResponse (..),
    newDetachCustomerManagedPolicyReferenceFromPermissionSetResponse,

    -- * Response Lenses
    detachCustomerManagedPolicyReferenceFromPermissionSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSOAdmin.Types

-- | /See:/ 'newDetachCustomerManagedPolicyReferenceFromPermissionSet' smart constructor.
data DetachCustomerManagedPolicyReferenceFromPermissionSet = DetachCustomerManagedPolicyReferenceFromPermissionSet'
  { -- | The ARN of the IAM Identity Center instance under which the operation
    -- will be executed.
    instanceArn :: Prelude.Text,
    -- | The ARN of the @PermissionSet@.
    permissionSetArn :: Prelude.Text,
    -- | Specifies the name and path of a customer managed policy. You must have
    -- an IAM policy that matches the name and path in each AWS account where
    -- you want to deploy your permission set.
    customerManagedPolicyReference :: CustomerManagedPolicyReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetachCustomerManagedPolicyReferenceFromPermissionSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceArn', 'detachCustomerManagedPolicyReferenceFromPermissionSet_instanceArn' - The ARN of the IAM Identity Center instance under which the operation
-- will be executed.
--
-- 'permissionSetArn', 'detachCustomerManagedPolicyReferenceFromPermissionSet_permissionSetArn' - The ARN of the @PermissionSet@.
--
-- 'customerManagedPolicyReference', 'detachCustomerManagedPolicyReferenceFromPermissionSet_customerManagedPolicyReference' - Specifies the name and path of a customer managed policy. You must have
-- an IAM policy that matches the name and path in each AWS account where
-- you want to deploy your permission set.
newDetachCustomerManagedPolicyReferenceFromPermissionSet ::
  -- | 'instanceArn'
  Prelude.Text ->
  -- | 'permissionSetArn'
  Prelude.Text ->
  -- | 'customerManagedPolicyReference'
  CustomerManagedPolicyReference ->
  DetachCustomerManagedPolicyReferenceFromPermissionSet
newDetachCustomerManagedPolicyReferenceFromPermissionSet
  pInstanceArn_
  pPermissionSetArn_
  pCustomerManagedPolicyReference_ =
    DetachCustomerManagedPolicyReferenceFromPermissionSet'
      { instanceArn =
          pInstanceArn_,
        permissionSetArn =
          pPermissionSetArn_,
        customerManagedPolicyReference =
          pCustomerManagedPolicyReference_
      }

-- | The ARN of the IAM Identity Center instance under which the operation
-- will be executed.
detachCustomerManagedPolicyReferenceFromPermissionSet_instanceArn :: Lens.Lens' DetachCustomerManagedPolicyReferenceFromPermissionSet Prelude.Text
detachCustomerManagedPolicyReferenceFromPermissionSet_instanceArn = Lens.lens (\DetachCustomerManagedPolicyReferenceFromPermissionSet' {instanceArn} -> instanceArn) (\s@DetachCustomerManagedPolicyReferenceFromPermissionSet' {} a -> s {instanceArn = a} :: DetachCustomerManagedPolicyReferenceFromPermissionSet)

-- | The ARN of the @PermissionSet@.
detachCustomerManagedPolicyReferenceFromPermissionSet_permissionSetArn :: Lens.Lens' DetachCustomerManagedPolicyReferenceFromPermissionSet Prelude.Text
detachCustomerManagedPolicyReferenceFromPermissionSet_permissionSetArn = Lens.lens (\DetachCustomerManagedPolicyReferenceFromPermissionSet' {permissionSetArn} -> permissionSetArn) (\s@DetachCustomerManagedPolicyReferenceFromPermissionSet' {} a -> s {permissionSetArn = a} :: DetachCustomerManagedPolicyReferenceFromPermissionSet)

-- | Specifies the name and path of a customer managed policy. You must have
-- an IAM policy that matches the name and path in each AWS account where
-- you want to deploy your permission set.
detachCustomerManagedPolicyReferenceFromPermissionSet_customerManagedPolicyReference :: Lens.Lens' DetachCustomerManagedPolicyReferenceFromPermissionSet CustomerManagedPolicyReference
detachCustomerManagedPolicyReferenceFromPermissionSet_customerManagedPolicyReference = Lens.lens (\DetachCustomerManagedPolicyReferenceFromPermissionSet' {customerManagedPolicyReference} -> customerManagedPolicyReference) (\s@DetachCustomerManagedPolicyReferenceFromPermissionSet' {} a -> s {customerManagedPolicyReference = a} :: DetachCustomerManagedPolicyReferenceFromPermissionSet)

instance
  Core.AWSRequest
    DetachCustomerManagedPolicyReferenceFromPermissionSet
  where
  type
    AWSResponse
      DetachCustomerManagedPolicyReferenceFromPermissionSet =
      DetachCustomerManagedPolicyReferenceFromPermissionSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DetachCustomerManagedPolicyReferenceFromPermissionSetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DetachCustomerManagedPolicyReferenceFromPermissionSet
  where
  hashWithSalt
    _salt
    DetachCustomerManagedPolicyReferenceFromPermissionSet' {..} =
      _salt
        `Prelude.hashWithSalt` instanceArn
        `Prelude.hashWithSalt` permissionSetArn
        `Prelude.hashWithSalt` customerManagedPolicyReference

instance
  Prelude.NFData
    DetachCustomerManagedPolicyReferenceFromPermissionSet
  where
  rnf
    DetachCustomerManagedPolicyReferenceFromPermissionSet' {..} =
      Prelude.rnf instanceArn
        `Prelude.seq` Prelude.rnf permissionSetArn
        `Prelude.seq` Prelude.rnf customerManagedPolicyReference

instance
  Data.ToHeaders
    DetachCustomerManagedPolicyReferenceFromPermissionSet
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SWBExternalService.DetachCustomerManagedPolicyReferenceFromPermissionSet" ::
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
    DetachCustomerManagedPolicyReferenceFromPermissionSet
  where
  toJSON
    DetachCustomerManagedPolicyReferenceFromPermissionSet' {..} =
      Data.object
        ( Prelude.catMaybes
            [ Prelude.Just ("InstanceArn" Data..= instanceArn),
              Prelude.Just
                ("PermissionSetArn" Data..= permissionSetArn),
              Prelude.Just
                ( "CustomerManagedPolicyReference"
                    Data..= customerManagedPolicyReference
                )
            ]
        )

instance
  Data.ToPath
    DetachCustomerManagedPolicyReferenceFromPermissionSet
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DetachCustomerManagedPolicyReferenceFromPermissionSet
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDetachCustomerManagedPolicyReferenceFromPermissionSetResponse' smart constructor.
data DetachCustomerManagedPolicyReferenceFromPermissionSetResponse = DetachCustomerManagedPolicyReferenceFromPermissionSetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetachCustomerManagedPolicyReferenceFromPermissionSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'detachCustomerManagedPolicyReferenceFromPermissionSetResponse_httpStatus' - The response's http status code.
newDetachCustomerManagedPolicyReferenceFromPermissionSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DetachCustomerManagedPolicyReferenceFromPermissionSetResponse
newDetachCustomerManagedPolicyReferenceFromPermissionSetResponse
  pHttpStatus_ =
    DetachCustomerManagedPolicyReferenceFromPermissionSetResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
detachCustomerManagedPolicyReferenceFromPermissionSetResponse_httpStatus :: Lens.Lens' DetachCustomerManagedPolicyReferenceFromPermissionSetResponse Prelude.Int
detachCustomerManagedPolicyReferenceFromPermissionSetResponse_httpStatus = Lens.lens (\DetachCustomerManagedPolicyReferenceFromPermissionSetResponse' {httpStatus} -> httpStatus) (\s@DetachCustomerManagedPolicyReferenceFromPermissionSetResponse' {} a -> s {httpStatus = a} :: DetachCustomerManagedPolicyReferenceFromPermissionSetResponse)

instance
  Prelude.NFData
    DetachCustomerManagedPolicyReferenceFromPermissionSetResponse
  where
  rnf
    DetachCustomerManagedPolicyReferenceFromPermissionSetResponse' {..} =
      Prelude.rnf httpStatus
