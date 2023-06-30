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
-- Module      : Amazonka.SSOAdmin.GetInlinePolicyForPermissionSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Obtains the inline policy assigned to the permission set.
module Amazonka.SSOAdmin.GetInlinePolicyForPermissionSet
  ( -- * Creating a Request
    GetInlinePolicyForPermissionSet (..),
    newGetInlinePolicyForPermissionSet,

    -- * Request Lenses
    getInlinePolicyForPermissionSet_instanceArn,
    getInlinePolicyForPermissionSet_permissionSetArn,

    -- * Destructuring the Response
    GetInlinePolicyForPermissionSetResponse (..),
    newGetInlinePolicyForPermissionSetResponse,

    -- * Response Lenses
    getInlinePolicyForPermissionSetResponse_inlinePolicy,
    getInlinePolicyForPermissionSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSOAdmin.Types

-- | /See:/ 'newGetInlinePolicyForPermissionSet' smart constructor.
data GetInlinePolicyForPermissionSet = GetInlinePolicyForPermissionSet'
  { -- | The ARN of the IAM Identity Center instance under which the operation
    -- will be executed. For more information about ARNs, see
    -- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
    -- in the /AWS General Reference/.
    instanceArn :: Prelude.Text,
    -- | The ARN of the permission set.
    permissionSetArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInlinePolicyForPermissionSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceArn', 'getInlinePolicyForPermissionSet_instanceArn' - The ARN of the IAM Identity Center instance under which the operation
-- will be executed. For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
-- in the /AWS General Reference/.
--
-- 'permissionSetArn', 'getInlinePolicyForPermissionSet_permissionSetArn' - The ARN of the permission set.
newGetInlinePolicyForPermissionSet ::
  -- | 'instanceArn'
  Prelude.Text ->
  -- | 'permissionSetArn'
  Prelude.Text ->
  GetInlinePolicyForPermissionSet
newGetInlinePolicyForPermissionSet
  pInstanceArn_
  pPermissionSetArn_ =
    GetInlinePolicyForPermissionSet'
      { instanceArn =
          pInstanceArn_,
        permissionSetArn = pPermissionSetArn_
      }

-- | The ARN of the IAM Identity Center instance under which the operation
-- will be executed. For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
-- in the /AWS General Reference/.
getInlinePolicyForPermissionSet_instanceArn :: Lens.Lens' GetInlinePolicyForPermissionSet Prelude.Text
getInlinePolicyForPermissionSet_instanceArn = Lens.lens (\GetInlinePolicyForPermissionSet' {instanceArn} -> instanceArn) (\s@GetInlinePolicyForPermissionSet' {} a -> s {instanceArn = a} :: GetInlinePolicyForPermissionSet)

-- | The ARN of the permission set.
getInlinePolicyForPermissionSet_permissionSetArn :: Lens.Lens' GetInlinePolicyForPermissionSet Prelude.Text
getInlinePolicyForPermissionSet_permissionSetArn = Lens.lens (\GetInlinePolicyForPermissionSet' {permissionSetArn} -> permissionSetArn) (\s@GetInlinePolicyForPermissionSet' {} a -> s {permissionSetArn = a} :: GetInlinePolicyForPermissionSet)

instance
  Core.AWSRequest
    GetInlinePolicyForPermissionSet
  where
  type
    AWSResponse GetInlinePolicyForPermissionSet =
      GetInlinePolicyForPermissionSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInlinePolicyForPermissionSetResponse'
            Prelude.<$> (x Data..?> "InlinePolicy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetInlinePolicyForPermissionSet
  where
  hashWithSalt
    _salt
    GetInlinePolicyForPermissionSet' {..} =
      _salt
        `Prelude.hashWithSalt` instanceArn
        `Prelude.hashWithSalt` permissionSetArn

instance
  Prelude.NFData
    GetInlinePolicyForPermissionSet
  where
  rnf GetInlinePolicyForPermissionSet' {..} =
    Prelude.rnf instanceArn
      `Prelude.seq` Prelude.rnf permissionSetArn

instance
  Data.ToHeaders
    GetInlinePolicyForPermissionSet
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SWBExternalService.GetInlinePolicyForPermissionSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetInlinePolicyForPermissionSet where
  toJSON GetInlinePolicyForPermissionSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("InstanceArn" Data..= instanceArn),
            Prelude.Just
              ("PermissionSetArn" Data..= permissionSetArn)
          ]
      )

instance Data.ToPath GetInlinePolicyForPermissionSet where
  toPath = Prelude.const "/"

instance Data.ToQuery GetInlinePolicyForPermissionSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetInlinePolicyForPermissionSetResponse' smart constructor.
data GetInlinePolicyForPermissionSetResponse = GetInlinePolicyForPermissionSetResponse'
  { -- | The inline policy that is attached to the permission set.
    inlinePolicy :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInlinePolicyForPermissionSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inlinePolicy', 'getInlinePolicyForPermissionSetResponse_inlinePolicy' - The inline policy that is attached to the permission set.
--
-- 'httpStatus', 'getInlinePolicyForPermissionSetResponse_httpStatus' - The response's http status code.
newGetInlinePolicyForPermissionSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetInlinePolicyForPermissionSetResponse
newGetInlinePolicyForPermissionSetResponse
  pHttpStatus_ =
    GetInlinePolicyForPermissionSetResponse'
      { inlinePolicy =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The inline policy that is attached to the permission set.
getInlinePolicyForPermissionSetResponse_inlinePolicy :: Lens.Lens' GetInlinePolicyForPermissionSetResponse (Prelude.Maybe Prelude.Text)
getInlinePolicyForPermissionSetResponse_inlinePolicy = Lens.lens (\GetInlinePolicyForPermissionSetResponse' {inlinePolicy} -> inlinePolicy) (\s@GetInlinePolicyForPermissionSetResponse' {} a -> s {inlinePolicy = a} :: GetInlinePolicyForPermissionSetResponse)

-- | The response's http status code.
getInlinePolicyForPermissionSetResponse_httpStatus :: Lens.Lens' GetInlinePolicyForPermissionSetResponse Prelude.Int
getInlinePolicyForPermissionSetResponse_httpStatus = Lens.lens (\GetInlinePolicyForPermissionSetResponse' {httpStatus} -> httpStatus) (\s@GetInlinePolicyForPermissionSetResponse' {} a -> s {httpStatus = a} :: GetInlinePolicyForPermissionSetResponse)

instance
  Prelude.NFData
    GetInlinePolicyForPermissionSetResponse
  where
  rnf GetInlinePolicyForPermissionSetResponse' {..} =
    Prelude.rnf inlinePolicy
      `Prelude.seq` Prelude.rnf httpStatus
