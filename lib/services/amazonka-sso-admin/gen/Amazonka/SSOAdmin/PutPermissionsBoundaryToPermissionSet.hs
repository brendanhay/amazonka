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
-- Module      : Amazonka.SSOAdmin.PutPermissionsBoundaryToPermissionSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches an AWS managed or customer managed policy to the specified
-- PermissionSet as a permissions boundary.
module Amazonka.SSOAdmin.PutPermissionsBoundaryToPermissionSet
  ( -- * Creating a Request
    PutPermissionsBoundaryToPermissionSet (..),
    newPutPermissionsBoundaryToPermissionSet,

    -- * Request Lenses
    putPermissionsBoundaryToPermissionSet_instanceArn,
    putPermissionsBoundaryToPermissionSet_permissionSetArn,
    putPermissionsBoundaryToPermissionSet_permissionsBoundary,

    -- * Destructuring the Response
    PutPermissionsBoundaryToPermissionSetResponse (..),
    newPutPermissionsBoundaryToPermissionSetResponse,

    -- * Response Lenses
    putPermissionsBoundaryToPermissionSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSOAdmin.Types

-- | /See:/ 'newPutPermissionsBoundaryToPermissionSet' smart constructor.
data PutPermissionsBoundaryToPermissionSet = PutPermissionsBoundaryToPermissionSet'
  { -- | The ARN of the IAM Identity Center instance under which the operation
    -- will be executed.
    instanceArn :: Prelude.Text,
    -- | The ARN of the @PermissionSet@.
    permissionSetArn :: Prelude.Text,
    -- | The permissions boundary that you want to attach to a @PermissionSet@.
    permissionsBoundary :: PermissionsBoundary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutPermissionsBoundaryToPermissionSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceArn', 'putPermissionsBoundaryToPermissionSet_instanceArn' - The ARN of the IAM Identity Center instance under which the operation
-- will be executed.
--
-- 'permissionSetArn', 'putPermissionsBoundaryToPermissionSet_permissionSetArn' - The ARN of the @PermissionSet@.
--
-- 'permissionsBoundary', 'putPermissionsBoundaryToPermissionSet_permissionsBoundary' - The permissions boundary that you want to attach to a @PermissionSet@.
newPutPermissionsBoundaryToPermissionSet ::
  -- | 'instanceArn'
  Prelude.Text ->
  -- | 'permissionSetArn'
  Prelude.Text ->
  -- | 'permissionsBoundary'
  PermissionsBoundary ->
  PutPermissionsBoundaryToPermissionSet
newPutPermissionsBoundaryToPermissionSet
  pInstanceArn_
  pPermissionSetArn_
  pPermissionsBoundary_ =
    PutPermissionsBoundaryToPermissionSet'
      { instanceArn =
          pInstanceArn_,
        permissionSetArn =
          pPermissionSetArn_,
        permissionsBoundary =
          pPermissionsBoundary_
      }

-- | The ARN of the IAM Identity Center instance under which the operation
-- will be executed.
putPermissionsBoundaryToPermissionSet_instanceArn :: Lens.Lens' PutPermissionsBoundaryToPermissionSet Prelude.Text
putPermissionsBoundaryToPermissionSet_instanceArn = Lens.lens (\PutPermissionsBoundaryToPermissionSet' {instanceArn} -> instanceArn) (\s@PutPermissionsBoundaryToPermissionSet' {} a -> s {instanceArn = a} :: PutPermissionsBoundaryToPermissionSet)

-- | The ARN of the @PermissionSet@.
putPermissionsBoundaryToPermissionSet_permissionSetArn :: Lens.Lens' PutPermissionsBoundaryToPermissionSet Prelude.Text
putPermissionsBoundaryToPermissionSet_permissionSetArn = Lens.lens (\PutPermissionsBoundaryToPermissionSet' {permissionSetArn} -> permissionSetArn) (\s@PutPermissionsBoundaryToPermissionSet' {} a -> s {permissionSetArn = a} :: PutPermissionsBoundaryToPermissionSet)

-- | The permissions boundary that you want to attach to a @PermissionSet@.
putPermissionsBoundaryToPermissionSet_permissionsBoundary :: Lens.Lens' PutPermissionsBoundaryToPermissionSet PermissionsBoundary
putPermissionsBoundaryToPermissionSet_permissionsBoundary = Lens.lens (\PutPermissionsBoundaryToPermissionSet' {permissionsBoundary} -> permissionsBoundary) (\s@PutPermissionsBoundaryToPermissionSet' {} a -> s {permissionsBoundary = a} :: PutPermissionsBoundaryToPermissionSet)

instance
  Core.AWSRequest
    PutPermissionsBoundaryToPermissionSet
  where
  type
    AWSResponse
      PutPermissionsBoundaryToPermissionSet =
      PutPermissionsBoundaryToPermissionSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutPermissionsBoundaryToPermissionSetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutPermissionsBoundaryToPermissionSet
  where
  hashWithSalt
    _salt
    PutPermissionsBoundaryToPermissionSet' {..} =
      _salt
        `Prelude.hashWithSalt` instanceArn
        `Prelude.hashWithSalt` permissionSetArn
        `Prelude.hashWithSalt` permissionsBoundary

instance
  Prelude.NFData
    PutPermissionsBoundaryToPermissionSet
  where
  rnf PutPermissionsBoundaryToPermissionSet' {..} =
    Prelude.rnf instanceArn
      `Prelude.seq` Prelude.rnf permissionSetArn
      `Prelude.seq` Prelude.rnf permissionsBoundary

instance
  Data.ToHeaders
    PutPermissionsBoundaryToPermissionSet
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SWBExternalService.PutPermissionsBoundaryToPermissionSet" ::
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
    PutPermissionsBoundaryToPermissionSet
  where
  toJSON PutPermissionsBoundaryToPermissionSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("InstanceArn" Data..= instanceArn),
            Prelude.Just
              ("PermissionSetArn" Data..= permissionSetArn),
            Prelude.Just
              ("PermissionsBoundary" Data..= permissionsBoundary)
          ]
      )

instance
  Data.ToPath
    PutPermissionsBoundaryToPermissionSet
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    PutPermissionsBoundaryToPermissionSet
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutPermissionsBoundaryToPermissionSetResponse' smart constructor.
data PutPermissionsBoundaryToPermissionSetResponse = PutPermissionsBoundaryToPermissionSetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutPermissionsBoundaryToPermissionSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putPermissionsBoundaryToPermissionSetResponse_httpStatus' - The response's http status code.
newPutPermissionsBoundaryToPermissionSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutPermissionsBoundaryToPermissionSetResponse
newPutPermissionsBoundaryToPermissionSetResponse
  pHttpStatus_ =
    PutPermissionsBoundaryToPermissionSetResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
putPermissionsBoundaryToPermissionSetResponse_httpStatus :: Lens.Lens' PutPermissionsBoundaryToPermissionSetResponse Prelude.Int
putPermissionsBoundaryToPermissionSetResponse_httpStatus = Lens.lens (\PutPermissionsBoundaryToPermissionSetResponse' {httpStatus} -> httpStatus) (\s@PutPermissionsBoundaryToPermissionSetResponse' {} a -> s {httpStatus = a} :: PutPermissionsBoundaryToPermissionSetResponse)

instance
  Prelude.NFData
    PutPermissionsBoundaryToPermissionSetResponse
  where
  rnf
    PutPermissionsBoundaryToPermissionSetResponse' {..} =
      Prelude.rnf httpStatus
