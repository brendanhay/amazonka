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
-- Module      : Network.AWS.SSOAdmin.GetInlinePolicyForPermissionSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Obtains the inline policy assigned to the permission set.
module Network.AWS.SSOAdmin.GetInlinePolicyForPermissionSet
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSOAdmin.Types

-- | /See:/ 'newGetInlinePolicyForPermissionSet' smart constructor.
data GetInlinePolicyForPermissionSet = GetInlinePolicyForPermissionSet'
  { -- | The ARN of the SSO instance under which the operation will be executed.
    -- For more information about ARNs, see
    -- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>
    -- in the /Amazon Web Services General Reference/.
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
-- 'instanceArn', 'getInlinePolicyForPermissionSet_instanceArn' - The ARN of the SSO instance under which the operation will be executed.
-- For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>
-- in the /Amazon Web Services General Reference/.
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

-- | The ARN of the SSO instance under which the operation will be executed.
-- For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>
-- in the /Amazon Web Services General Reference/.
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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInlinePolicyForPermissionSetResponse'
            Prelude.<$> (x Core..?> "InlinePolicy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetInlinePolicyForPermissionSet

instance
  Prelude.NFData
    GetInlinePolicyForPermissionSet

instance
  Core.ToHeaders
    GetInlinePolicyForPermissionSet
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SWBExternalService.GetInlinePolicyForPermissionSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetInlinePolicyForPermissionSet where
  toJSON GetInlinePolicyForPermissionSet' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("InstanceArn" Core..= instanceArn),
            Prelude.Just
              ("PermissionSetArn" Core..= permissionSetArn)
          ]
      )

instance Core.ToPath GetInlinePolicyForPermissionSet where
  toPath = Prelude.const "/"

instance Core.ToQuery GetInlinePolicyForPermissionSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetInlinePolicyForPermissionSetResponse' smart constructor.
data GetInlinePolicyForPermissionSetResponse = GetInlinePolicyForPermissionSetResponse'
  { -- | The IAM inline policy that is attached to the permission set.
    inlinePolicy :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInlinePolicyForPermissionSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inlinePolicy', 'getInlinePolicyForPermissionSetResponse_inlinePolicy' - The IAM inline policy that is attached to the permission set.
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

-- | The IAM inline policy that is attached to the permission set.
getInlinePolicyForPermissionSetResponse_inlinePolicy :: Lens.Lens' GetInlinePolicyForPermissionSetResponse (Prelude.Maybe Prelude.Text)
getInlinePolicyForPermissionSetResponse_inlinePolicy = Lens.lens (\GetInlinePolicyForPermissionSetResponse' {inlinePolicy} -> inlinePolicy) (\s@GetInlinePolicyForPermissionSetResponse' {} a -> s {inlinePolicy = a} :: GetInlinePolicyForPermissionSetResponse) Prelude.. Lens.mapping Core._Sensitive

-- | The response's http status code.
getInlinePolicyForPermissionSetResponse_httpStatus :: Lens.Lens' GetInlinePolicyForPermissionSetResponse Prelude.Int
getInlinePolicyForPermissionSetResponse_httpStatus = Lens.lens (\GetInlinePolicyForPermissionSetResponse' {httpStatus} -> httpStatus) (\s@GetInlinePolicyForPermissionSetResponse' {} a -> s {httpStatus = a} :: GetInlinePolicyForPermissionSetResponse)

instance
  Prelude.NFData
    GetInlinePolicyForPermissionSetResponse
