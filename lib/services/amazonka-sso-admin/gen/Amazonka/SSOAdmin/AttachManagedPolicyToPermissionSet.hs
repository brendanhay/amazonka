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
-- Module      : Amazonka.SSOAdmin.AttachManagedPolicyToPermissionSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches an AWS managed policy ARN to a permission set.
--
-- If the permission set is already referenced by one or more account
-- assignments, you will need to call @ ProvisionPermissionSet @ after this
-- operation. Calling @ProvisionPermissionSet@ applies the corresponding
-- IAM policy updates to all assigned accounts.
module Amazonka.SSOAdmin.AttachManagedPolicyToPermissionSet
  ( -- * Creating a Request
    AttachManagedPolicyToPermissionSet (..),
    newAttachManagedPolicyToPermissionSet,

    -- * Request Lenses
    attachManagedPolicyToPermissionSet_instanceArn,
    attachManagedPolicyToPermissionSet_permissionSetArn,
    attachManagedPolicyToPermissionSet_managedPolicyArn,

    -- * Destructuring the Response
    AttachManagedPolicyToPermissionSetResponse (..),
    newAttachManagedPolicyToPermissionSetResponse,

    -- * Response Lenses
    attachManagedPolicyToPermissionSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSOAdmin.Types

-- | /See:/ 'newAttachManagedPolicyToPermissionSet' smart constructor.
data AttachManagedPolicyToPermissionSet = AttachManagedPolicyToPermissionSet'
  { -- | The ARN of the IAM Identity Center instance under which the operation
    -- will be executed. For more information about ARNs, see
    -- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
    -- in the /AWS General Reference/.
    instanceArn :: Prelude.Text,
    -- | The ARN of the PermissionSet that the managed policy should be attached
    -- to.
    permissionSetArn :: Prelude.Text,
    -- | The AWS managed policy ARN to be attached to a permission set.
    managedPolicyArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachManagedPolicyToPermissionSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceArn', 'attachManagedPolicyToPermissionSet_instanceArn' - The ARN of the IAM Identity Center instance under which the operation
-- will be executed. For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
-- in the /AWS General Reference/.
--
-- 'permissionSetArn', 'attachManagedPolicyToPermissionSet_permissionSetArn' - The ARN of the PermissionSet that the managed policy should be attached
-- to.
--
-- 'managedPolicyArn', 'attachManagedPolicyToPermissionSet_managedPolicyArn' - The AWS managed policy ARN to be attached to a permission set.
newAttachManagedPolicyToPermissionSet ::
  -- | 'instanceArn'
  Prelude.Text ->
  -- | 'permissionSetArn'
  Prelude.Text ->
  -- | 'managedPolicyArn'
  Prelude.Text ->
  AttachManagedPolicyToPermissionSet
newAttachManagedPolicyToPermissionSet
  pInstanceArn_
  pPermissionSetArn_
  pManagedPolicyArn_ =
    AttachManagedPolicyToPermissionSet'
      { instanceArn =
          pInstanceArn_,
        permissionSetArn = pPermissionSetArn_,
        managedPolicyArn = pManagedPolicyArn_
      }

-- | The ARN of the IAM Identity Center instance under which the operation
-- will be executed. For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
-- in the /AWS General Reference/.
attachManagedPolicyToPermissionSet_instanceArn :: Lens.Lens' AttachManagedPolicyToPermissionSet Prelude.Text
attachManagedPolicyToPermissionSet_instanceArn = Lens.lens (\AttachManagedPolicyToPermissionSet' {instanceArn} -> instanceArn) (\s@AttachManagedPolicyToPermissionSet' {} a -> s {instanceArn = a} :: AttachManagedPolicyToPermissionSet)

-- | The ARN of the PermissionSet that the managed policy should be attached
-- to.
attachManagedPolicyToPermissionSet_permissionSetArn :: Lens.Lens' AttachManagedPolicyToPermissionSet Prelude.Text
attachManagedPolicyToPermissionSet_permissionSetArn = Lens.lens (\AttachManagedPolicyToPermissionSet' {permissionSetArn} -> permissionSetArn) (\s@AttachManagedPolicyToPermissionSet' {} a -> s {permissionSetArn = a} :: AttachManagedPolicyToPermissionSet)

-- | The AWS managed policy ARN to be attached to a permission set.
attachManagedPolicyToPermissionSet_managedPolicyArn :: Lens.Lens' AttachManagedPolicyToPermissionSet Prelude.Text
attachManagedPolicyToPermissionSet_managedPolicyArn = Lens.lens (\AttachManagedPolicyToPermissionSet' {managedPolicyArn} -> managedPolicyArn) (\s@AttachManagedPolicyToPermissionSet' {} a -> s {managedPolicyArn = a} :: AttachManagedPolicyToPermissionSet)

instance
  Core.AWSRequest
    AttachManagedPolicyToPermissionSet
  where
  type
    AWSResponse AttachManagedPolicyToPermissionSet =
      AttachManagedPolicyToPermissionSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AttachManagedPolicyToPermissionSetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AttachManagedPolicyToPermissionSet
  where
  hashWithSalt
    _salt
    AttachManagedPolicyToPermissionSet' {..} =
      _salt `Prelude.hashWithSalt` instanceArn
        `Prelude.hashWithSalt` permissionSetArn
        `Prelude.hashWithSalt` managedPolicyArn

instance
  Prelude.NFData
    AttachManagedPolicyToPermissionSet
  where
  rnf AttachManagedPolicyToPermissionSet' {..} =
    Prelude.rnf instanceArn
      `Prelude.seq` Prelude.rnf permissionSetArn
      `Prelude.seq` Prelude.rnf managedPolicyArn

instance
  Data.ToHeaders
    AttachManagedPolicyToPermissionSet
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SWBExternalService.AttachManagedPolicyToPermissionSet" ::
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
    AttachManagedPolicyToPermissionSet
  where
  toJSON AttachManagedPolicyToPermissionSet' {..} =
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
    AttachManagedPolicyToPermissionSet
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    AttachManagedPolicyToPermissionSet
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAttachManagedPolicyToPermissionSetResponse' smart constructor.
data AttachManagedPolicyToPermissionSetResponse = AttachManagedPolicyToPermissionSetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachManagedPolicyToPermissionSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'attachManagedPolicyToPermissionSetResponse_httpStatus' - The response's http status code.
newAttachManagedPolicyToPermissionSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AttachManagedPolicyToPermissionSetResponse
newAttachManagedPolicyToPermissionSetResponse
  pHttpStatus_ =
    AttachManagedPolicyToPermissionSetResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
attachManagedPolicyToPermissionSetResponse_httpStatus :: Lens.Lens' AttachManagedPolicyToPermissionSetResponse Prelude.Int
attachManagedPolicyToPermissionSetResponse_httpStatus = Lens.lens (\AttachManagedPolicyToPermissionSetResponse' {httpStatus} -> httpStatus) (\s@AttachManagedPolicyToPermissionSetResponse' {} a -> s {httpStatus = a} :: AttachManagedPolicyToPermissionSetResponse)

instance
  Prelude.NFData
    AttachManagedPolicyToPermissionSetResponse
  where
  rnf AttachManagedPolicyToPermissionSetResponse' {..} =
    Prelude.rnf httpStatus
