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
-- Module      : Amazonka.SSOAdmin.GetPermissionsBoundaryForPermissionSet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Obtains the permissions boundary for a specified PermissionSet.
module Amazonka.SSOAdmin.GetPermissionsBoundaryForPermissionSet
  ( -- * Creating a Request
    GetPermissionsBoundaryForPermissionSet (..),
    newGetPermissionsBoundaryForPermissionSet,

    -- * Request Lenses
    getPermissionsBoundaryForPermissionSet_instanceArn,
    getPermissionsBoundaryForPermissionSet_permissionSetArn,

    -- * Destructuring the Response
    GetPermissionsBoundaryForPermissionSetResponse (..),
    newGetPermissionsBoundaryForPermissionSetResponse,

    -- * Response Lenses
    getPermissionsBoundaryForPermissionSetResponse_permissionsBoundary,
    getPermissionsBoundaryForPermissionSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSOAdmin.Types

-- | /See:/ 'newGetPermissionsBoundaryForPermissionSet' smart constructor.
data GetPermissionsBoundaryForPermissionSet = GetPermissionsBoundaryForPermissionSet'
  { -- | The ARN of the IAM Identity Center instance under which the operation
    -- will be executed.
    instanceArn :: Prelude.Text,
    -- | The ARN of the @PermissionSet@.
    permissionSetArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPermissionsBoundaryForPermissionSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceArn', 'getPermissionsBoundaryForPermissionSet_instanceArn' - The ARN of the IAM Identity Center instance under which the operation
-- will be executed.
--
-- 'permissionSetArn', 'getPermissionsBoundaryForPermissionSet_permissionSetArn' - The ARN of the @PermissionSet@.
newGetPermissionsBoundaryForPermissionSet ::
  -- | 'instanceArn'
  Prelude.Text ->
  -- | 'permissionSetArn'
  Prelude.Text ->
  GetPermissionsBoundaryForPermissionSet
newGetPermissionsBoundaryForPermissionSet
  pInstanceArn_
  pPermissionSetArn_ =
    GetPermissionsBoundaryForPermissionSet'
      { instanceArn =
          pInstanceArn_,
        permissionSetArn =
          pPermissionSetArn_
      }

-- | The ARN of the IAM Identity Center instance under which the operation
-- will be executed.
getPermissionsBoundaryForPermissionSet_instanceArn :: Lens.Lens' GetPermissionsBoundaryForPermissionSet Prelude.Text
getPermissionsBoundaryForPermissionSet_instanceArn = Lens.lens (\GetPermissionsBoundaryForPermissionSet' {instanceArn} -> instanceArn) (\s@GetPermissionsBoundaryForPermissionSet' {} a -> s {instanceArn = a} :: GetPermissionsBoundaryForPermissionSet)

-- | The ARN of the @PermissionSet@.
getPermissionsBoundaryForPermissionSet_permissionSetArn :: Lens.Lens' GetPermissionsBoundaryForPermissionSet Prelude.Text
getPermissionsBoundaryForPermissionSet_permissionSetArn = Lens.lens (\GetPermissionsBoundaryForPermissionSet' {permissionSetArn} -> permissionSetArn) (\s@GetPermissionsBoundaryForPermissionSet' {} a -> s {permissionSetArn = a} :: GetPermissionsBoundaryForPermissionSet)

instance
  Core.AWSRequest
    GetPermissionsBoundaryForPermissionSet
  where
  type
    AWSResponse
      GetPermissionsBoundaryForPermissionSet =
      GetPermissionsBoundaryForPermissionSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPermissionsBoundaryForPermissionSetResponse'
            Prelude.<$> (x Core..?> "PermissionsBoundary")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetPermissionsBoundaryForPermissionSet
  where
  hashWithSalt
    _salt
    GetPermissionsBoundaryForPermissionSet' {..} =
      _salt `Prelude.hashWithSalt` instanceArn
        `Prelude.hashWithSalt` permissionSetArn

instance
  Prelude.NFData
    GetPermissionsBoundaryForPermissionSet
  where
  rnf GetPermissionsBoundaryForPermissionSet' {..} =
    Prelude.rnf instanceArn
      `Prelude.seq` Prelude.rnf permissionSetArn

instance
  Core.ToHeaders
    GetPermissionsBoundaryForPermissionSet
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SWBExternalService.GetPermissionsBoundaryForPermissionSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    GetPermissionsBoundaryForPermissionSet
  where
  toJSON GetPermissionsBoundaryForPermissionSet' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("InstanceArn" Core..= instanceArn),
            Prelude.Just
              ("PermissionSetArn" Core..= permissionSetArn)
          ]
      )

instance
  Core.ToPath
    GetPermissionsBoundaryForPermissionSet
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    GetPermissionsBoundaryForPermissionSet
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPermissionsBoundaryForPermissionSetResponse' smart constructor.
data GetPermissionsBoundaryForPermissionSetResponse = GetPermissionsBoundaryForPermissionSetResponse'
  { -- | The permissions boundary attached to the specified permission set.
    permissionsBoundary :: Prelude.Maybe PermissionsBoundary,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPermissionsBoundaryForPermissionSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'permissionsBoundary', 'getPermissionsBoundaryForPermissionSetResponse_permissionsBoundary' - The permissions boundary attached to the specified permission set.
--
-- 'httpStatus', 'getPermissionsBoundaryForPermissionSetResponse_httpStatus' - The response's http status code.
newGetPermissionsBoundaryForPermissionSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPermissionsBoundaryForPermissionSetResponse
newGetPermissionsBoundaryForPermissionSetResponse
  pHttpStatus_ =
    GetPermissionsBoundaryForPermissionSetResponse'
      { permissionsBoundary =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The permissions boundary attached to the specified permission set.
getPermissionsBoundaryForPermissionSetResponse_permissionsBoundary :: Lens.Lens' GetPermissionsBoundaryForPermissionSetResponse (Prelude.Maybe PermissionsBoundary)
getPermissionsBoundaryForPermissionSetResponse_permissionsBoundary = Lens.lens (\GetPermissionsBoundaryForPermissionSetResponse' {permissionsBoundary} -> permissionsBoundary) (\s@GetPermissionsBoundaryForPermissionSetResponse' {} a -> s {permissionsBoundary = a} :: GetPermissionsBoundaryForPermissionSetResponse)

-- | The response's http status code.
getPermissionsBoundaryForPermissionSetResponse_httpStatus :: Lens.Lens' GetPermissionsBoundaryForPermissionSetResponse Prelude.Int
getPermissionsBoundaryForPermissionSetResponse_httpStatus = Lens.lens (\GetPermissionsBoundaryForPermissionSetResponse' {httpStatus} -> httpStatus) (\s@GetPermissionsBoundaryForPermissionSetResponse' {} a -> s {httpStatus = a} :: GetPermissionsBoundaryForPermissionSetResponse)

instance
  Prelude.NFData
    GetPermissionsBoundaryForPermissionSetResponse
  where
  rnf
    GetPermissionsBoundaryForPermissionSetResponse' {..} =
      Prelude.rnf permissionsBoundary
        `Prelude.seq` Prelude.rnf httpStatus
