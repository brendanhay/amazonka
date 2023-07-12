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
-- Module      : Amazonka.SSOAdmin.AttachCustomerManagedPolicyReferenceToPermissionSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches the specified customer managed policy to the specified
-- PermissionSet.
module Amazonka.SSOAdmin.AttachCustomerManagedPolicyReferenceToPermissionSet
  ( -- * Creating a Request
    AttachCustomerManagedPolicyReferenceToPermissionSet (..),
    newAttachCustomerManagedPolicyReferenceToPermissionSet,

    -- * Request Lenses
    attachCustomerManagedPolicyReferenceToPermissionSet_instanceArn,
    attachCustomerManagedPolicyReferenceToPermissionSet_permissionSetArn,
    attachCustomerManagedPolicyReferenceToPermissionSet_customerManagedPolicyReference,

    -- * Destructuring the Response
    AttachCustomerManagedPolicyReferenceToPermissionSetResponse (..),
    newAttachCustomerManagedPolicyReferenceToPermissionSetResponse,

    -- * Response Lenses
    attachCustomerManagedPolicyReferenceToPermissionSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSOAdmin.Types

-- | /See:/ 'newAttachCustomerManagedPolicyReferenceToPermissionSet' smart constructor.
data AttachCustomerManagedPolicyReferenceToPermissionSet = AttachCustomerManagedPolicyReferenceToPermissionSet'
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
-- Create a value of 'AttachCustomerManagedPolicyReferenceToPermissionSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceArn', 'attachCustomerManagedPolicyReferenceToPermissionSet_instanceArn' - The ARN of the IAM Identity Center instance under which the operation
-- will be executed.
--
-- 'permissionSetArn', 'attachCustomerManagedPolicyReferenceToPermissionSet_permissionSetArn' - The ARN of the @PermissionSet@.
--
-- 'customerManagedPolicyReference', 'attachCustomerManagedPolicyReferenceToPermissionSet_customerManagedPolicyReference' - Specifies the name and path of a customer managed policy. You must have
-- an IAM policy that matches the name and path in each AWS account where
-- you want to deploy your permission set.
newAttachCustomerManagedPolicyReferenceToPermissionSet ::
  -- | 'instanceArn'
  Prelude.Text ->
  -- | 'permissionSetArn'
  Prelude.Text ->
  -- | 'customerManagedPolicyReference'
  CustomerManagedPolicyReference ->
  AttachCustomerManagedPolicyReferenceToPermissionSet
newAttachCustomerManagedPolicyReferenceToPermissionSet
  pInstanceArn_
  pPermissionSetArn_
  pCustomerManagedPolicyReference_ =
    AttachCustomerManagedPolicyReferenceToPermissionSet'
      { instanceArn =
          pInstanceArn_,
        permissionSetArn =
          pPermissionSetArn_,
        customerManagedPolicyReference =
          pCustomerManagedPolicyReference_
      }

-- | The ARN of the IAM Identity Center instance under which the operation
-- will be executed.
attachCustomerManagedPolicyReferenceToPermissionSet_instanceArn :: Lens.Lens' AttachCustomerManagedPolicyReferenceToPermissionSet Prelude.Text
attachCustomerManagedPolicyReferenceToPermissionSet_instanceArn = Lens.lens (\AttachCustomerManagedPolicyReferenceToPermissionSet' {instanceArn} -> instanceArn) (\s@AttachCustomerManagedPolicyReferenceToPermissionSet' {} a -> s {instanceArn = a} :: AttachCustomerManagedPolicyReferenceToPermissionSet)

-- | The ARN of the @PermissionSet@.
attachCustomerManagedPolicyReferenceToPermissionSet_permissionSetArn :: Lens.Lens' AttachCustomerManagedPolicyReferenceToPermissionSet Prelude.Text
attachCustomerManagedPolicyReferenceToPermissionSet_permissionSetArn = Lens.lens (\AttachCustomerManagedPolicyReferenceToPermissionSet' {permissionSetArn} -> permissionSetArn) (\s@AttachCustomerManagedPolicyReferenceToPermissionSet' {} a -> s {permissionSetArn = a} :: AttachCustomerManagedPolicyReferenceToPermissionSet)

-- | Specifies the name and path of a customer managed policy. You must have
-- an IAM policy that matches the name and path in each AWS account where
-- you want to deploy your permission set.
attachCustomerManagedPolicyReferenceToPermissionSet_customerManagedPolicyReference :: Lens.Lens' AttachCustomerManagedPolicyReferenceToPermissionSet CustomerManagedPolicyReference
attachCustomerManagedPolicyReferenceToPermissionSet_customerManagedPolicyReference = Lens.lens (\AttachCustomerManagedPolicyReferenceToPermissionSet' {customerManagedPolicyReference} -> customerManagedPolicyReference) (\s@AttachCustomerManagedPolicyReferenceToPermissionSet' {} a -> s {customerManagedPolicyReference = a} :: AttachCustomerManagedPolicyReferenceToPermissionSet)

instance
  Core.AWSRequest
    AttachCustomerManagedPolicyReferenceToPermissionSet
  where
  type
    AWSResponse
      AttachCustomerManagedPolicyReferenceToPermissionSet =
      AttachCustomerManagedPolicyReferenceToPermissionSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AttachCustomerManagedPolicyReferenceToPermissionSetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AttachCustomerManagedPolicyReferenceToPermissionSet
  where
  hashWithSalt
    _salt
    AttachCustomerManagedPolicyReferenceToPermissionSet' {..} =
      _salt
        `Prelude.hashWithSalt` instanceArn
        `Prelude.hashWithSalt` permissionSetArn
        `Prelude.hashWithSalt` customerManagedPolicyReference

instance
  Prelude.NFData
    AttachCustomerManagedPolicyReferenceToPermissionSet
  where
  rnf
    AttachCustomerManagedPolicyReferenceToPermissionSet' {..} =
      Prelude.rnf instanceArn
        `Prelude.seq` Prelude.rnf permissionSetArn
        `Prelude.seq` Prelude.rnf customerManagedPolicyReference

instance
  Data.ToHeaders
    AttachCustomerManagedPolicyReferenceToPermissionSet
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SWBExternalService.AttachCustomerManagedPolicyReferenceToPermissionSet" ::
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
    AttachCustomerManagedPolicyReferenceToPermissionSet
  where
  toJSON
    AttachCustomerManagedPolicyReferenceToPermissionSet' {..} =
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
    AttachCustomerManagedPolicyReferenceToPermissionSet
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    AttachCustomerManagedPolicyReferenceToPermissionSet
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAttachCustomerManagedPolicyReferenceToPermissionSetResponse' smart constructor.
data AttachCustomerManagedPolicyReferenceToPermissionSetResponse = AttachCustomerManagedPolicyReferenceToPermissionSetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachCustomerManagedPolicyReferenceToPermissionSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'attachCustomerManagedPolicyReferenceToPermissionSetResponse_httpStatus' - The response's http status code.
newAttachCustomerManagedPolicyReferenceToPermissionSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AttachCustomerManagedPolicyReferenceToPermissionSetResponse
newAttachCustomerManagedPolicyReferenceToPermissionSetResponse
  pHttpStatus_ =
    AttachCustomerManagedPolicyReferenceToPermissionSetResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
attachCustomerManagedPolicyReferenceToPermissionSetResponse_httpStatus :: Lens.Lens' AttachCustomerManagedPolicyReferenceToPermissionSetResponse Prelude.Int
attachCustomerManagedPolicyReferenceToPermissionSetResponse_httpStatus = Lens.lens (\AttachCustomerManagedPolicyReferenceToPermissionSetResponse' {httpStatus} -> httpStatus) (\s@AttachCustomerManagedPolicyReferenceToPermissionSetResponse' {} a -> s {httpStatus = a} :: AttachCustomerManagedPolicyReferenceToPermissionSetResponse)

instance
  Prelude.NFData
    AttachCustomerManagedPolicyReferenceToPermissionSetResponse
  where
  rnf
    AttachCustomerManagedPolicyReferenceToPermissionSetResponse' {..} =
      Prelude.rnf httpStatus
