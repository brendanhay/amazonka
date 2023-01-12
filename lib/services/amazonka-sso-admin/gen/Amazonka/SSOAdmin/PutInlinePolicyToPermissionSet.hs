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
-- Module      : Amazonka.SSOAdmin.PutInlinePolicyToPermissionSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches an inline policy to a permission set.
--
-- If the permission set is already referenced by one or more account
-- assignments, you will need to call @ ProvisionPermissionSet @ after this
-- action to apply the corresponding IAM policy updates to all assigned
-- accounts.
module Amazonka.SSOAdmin.PutInlinePolicyToPermissionSet
  ( -- * Creating a Request
    PutInlinePolicyToPermissionSet (..),
    newPutInlinePolicyToPermissionSet,

    -- * Request Lenses
    putInlinePolicyToPermissionSet_instanceArn,
    putInlinePolicyToPermissionSet_permissionSetArn,
    putInlinePolicyToPermissionSet_inlinePolicy,

    -- * Destructuring the Response
    PutInlinePolicyToPermissionSetResponse (..),
    newPutInlinePolicyToPermissionSetResponse,

    -- * Response Lenses
    putInlinePolicyToPermissionSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSOAdmin.Types

-- | /See:/ 'newPutInlinePolicyToPermissionSet' smart constructor.
data PutInlinePolicyToPermissionSet = PutInlinePolicyToPermissionSet'
  { -- | The ARN of the IAM Identity Center instance under which the operation
    -- will be executed. For more information about ARNs, see
    -- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
    -- in the /AWS General Reference/.
    instanceArn :: Prelude.Text,
    -- | The ARN of the permission set.
    permissionSetArn :: Prelude.Text,
    -- | The inline policy to attach to a PermissionSet.
    inlinePolicy :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutInlinePolicyToPermissionSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceArn', 'putInlinePolicyToPermissionSet_instanceArn' - The ARN of the IAM Identity Center instance under which the operation
-- will be executed. For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
-- in the /AWS General Reference/.
--
-- 'permissionSetArn', 'putInlinePolicyToPermissionSet_permissionSetArn' - The ARN of the permission set.
--
-- 'inlinePolicy', 'putInlinePolicyToPermissionSet_inlinePolicy' - The inline policy to attach to a PermissionSet.
newPutInlinePolicyToPermissionSet ::
  -- | 'instanceArn'
  Prelude.Text ->
  -- | 'permissionSetArn'
  Prelude.Text ->
  -- | 'inlinePolicy'
  Prelude.Text ->
  PutInlinePolicyToPermissionSet
newPutInlinePolicyToPermissionSet
  pInstanceArn_
  pPermissionSetArn_
  pInlinePolicy_ =
    PutInlinePolicyToPermissionSet'
      { instanceArn =
          pInstanceArn_,
        permissionSetArn = pPermissionSetArn_,
        inlinePolicy = pInlinePolicy_
      }

-- | The ARN of the IAM Identity Center instance under which the operation
-- will be executed. For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
-- in the /AWS General Reference/.
putInlinePolicyToPermissionSet_instanceArn :: Lens.Lens' PutInlinePolicyToPermissionSet Prelude.Text
putInlinePolicyToPermissionSet_instanceArn = Lens.lens (\PutInlinePolicyToPermissionSet' {instanceArn} -> instanceArn) (\s@PutInlinePolicyToPermissionSet' {} a -> s {instanceArn = a} :: PutInlinePolicyToPermissionSet)

-- | The ARN of the permission set.
putInlinePolicyToPermissionSet_permissionSetArn :: Lens.Lens' PutInlinePolicyToPermissionSet Prelude.Text
putInlinePolicyToPermissionSet_permissionSetArn = Lens.lens (\PutInlinePolicyToPermissionSet' {permissionSetArn} -> permissionSetArn) (\s@PutInlinePolicyToPermissionSet' {} a -> s {permissionSetArn = a} :: PutInlinePolicyToPermissionSet)

-- | The inline policy to attach to a PermissionSet.
putInlinePolicyToPermissionSet_inlinePolicy :: Lens.Lens' PutInlinePolicyToPermissionSet Prelude.Text
putInlinePolicyToPermissionSet_inlinePolicy = Lens.lens (\PutInlinePolicyToPermissionSet' {inlinePolicy} -> inlinePolicy) (\s@PutInlinePolicyToPermissionSet' {} a -> s {inlinePolicy = a} :: PutInlinePolicyToPermissionSet)

instance
  Core.AWSRequest
    PutInlinePolicyToPermissionSet
  where
  type
    AWSResponse PutInlinePolicyToPermissionSet =
      PutInlinePolicyToPermissionSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutInlinePolicyToPermissionSetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutInlinePolicyToPermissionSet
  where
  hashWithSalt
    _salt
    PutInlinePolicyToPermissionSet' {..} =
      _salt `Prelude.hashWithSalt` instanceArn
        `Prelude.hashWithSalt` permissionSetArn
        `Prelude.hashWithSalt` inlinePolicy

instance
  Prelude.NFData
    PutInlinePolicyToPermissionSet
  where
  rnf PutInlinePolicyToPermissionSet' {..} =
    Prelude.rnf instanceArn
      `Prelude.seq` Prelude.rnf permissionSetArn
      `Prelude.seq` Prelude.rnf inlinePolicy

instance
  Data.ToHeaders
    PutInlinePolicyToPermissionSet
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SWBExternalService.PutInlinePolicyToPermissionSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutInlinePolicyToPermissionSet where
  toJSON PutInlinePolicyToPermissionSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("InstanceArn" Data..= instanceArn),
            Prelude.Just
              ("PermissionSetArn" Data..= permissionSetArn),
            Prelude.Just ("InlinePolicy" Data..= inlinePolicy)
          ]
      )

instance Data.ToPath PutInlinePolicyToPermissionSet where
  toPath = Prelude.const "/"

instance Data.ToQuery PutInlinePolicyToPermissionSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutInlinePolicyToPermissionSetResponse' smart constructor.
data PutInlinePolicyToPermissionSetResponse = PutInlinePolicyToPermissionSetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutInlinePolicyToPermissionSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putInlinePolicyToPermissionSetResponse_httpStatus' - The response's http status code.
newPutInlinePolicyToPermissionSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutInlinePolicyToPermissionSetResponse
newPutInlinePolicyToPermissionSetResponse
  pHttpStatus_ =
    PutInlinePolicyToPermissionSetResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
putInlinePolicyToPermissionSetResponse_httpStatus :: Lens.Lens' PutInlinePolicyToPermissionSetResponse Prelude.Int
putInlinePolicyToPermissionSetResponse_httpStatus = Lens.lens (\PutInlinePolicyToPermissionSetResponse' {httpStatus} -> httpStatus) (\s@PutInlinePolicyToPermissionSetResponse' {} a -> s {httpStatus = a} :: PutInlinePolicyToPermissionSetResponse)

instance
  Prelude.NFData
    PutInlinePolicyToPermissionSetResponse
  where
  rnf PutInlinePolicyToPermissionSetResponse' {..} =
    Prelude.rnf httpStatus
