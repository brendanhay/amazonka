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
-- Module      : Amazonka.RAM.DeletePermission
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified customer managed permission in the Amazon Web
-- Services Region in which you call this operation. You can delete a
-- customer managed permission only if it isn\'t attached to any resource
-- share. The operation deletes all versions associated with the customer
-- managed permission.
module Amazonka.RAM.DeletePermission
  ( -- * Creating a Request
    DeletePermission (..),
    newDeletePermission,

    -- * Request Lenses
    deletePermission_clientToken,
    deletePermission_permissionArn,

    -- * Destructuring the Response
    DeletePermissionResponse (..),
    newDeletePermissionResponse,

    -- * Response Lenses
    deletePermissionResponse_clientToken,
    deletePermissionResponse_permissionStatus,
    deletePermissionResponse_returnValue,
    deletePermissionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeletePermission' smart constructor.
data DeletePermission = DeletePermission'
  { -- | Specifies a unique, case-sensitive identifier that you provide to ensure
    -- the idempotency of the request. This lets you safely retry the request
    -- without accidentally performing the same operation a second time.
    -- Passing the same value to a later call to an operation requires that you
    -- also pass the same value for all other parameters. We recommend that you
    -- use a
    -- <https://wikipedia.org/wiki/Universally_unique_identifier UUID type of value.>.
    --
    -- If you don\'t provide this value, then Amazon Web Services generates a
    -- random one for you.
    --
    -- If you retry the operation with the same @ClientToken@, but with
    -- different parameters, the retry fails with an
    -- @IdempotentParameterMismatch@ error.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies the
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
    -- of the customer managed permission that you want to delete.
    permissionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deletePermission_clientToken' - Specifies a unique, case-sensitive identifier that you provide to ensure
-- the idempotency of the request. This lets you safely retry the request
-- without accidentally performing the same operation a second time.
-- Passing the same value to a later call to an operation requires that you
-- also pass the same value for all other parameters. We recommend that you
-- use a
-- <https://wikipedia.org/wiki/Universally_unique_identifier UUID type of value.>.
--
-- If you don\'t provide this value, then Amazon Web Services generates a
-- random one for you.
--
-- If you retry the operation with the same @ClientToken@, but with
-- different parameters, the retry fails with an
-- @IdempotentParameterMismatch@ error.
--
-- 'permissionArn', 'deletePermission_permissionArn' - Specifies the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of the customer managed permission that you want to delete.
newDeletePermission ::
  -- | 'permissionArn'
  Prelude.Text ->
  DeletePermission
newDeletePermission pPermissionArn_ =
  DeletePermission'
    { clientToken = Prelude.Nothing,
      permissionArn = pPermissionArn_
    }

-- | Specifies a unique, case-sensitive identifier that you provide to ensure
-- the idempotency of the request. This lets you safely retry the request
-- without accidentally performing the same operation a second time.
-- Passing the same value to a later call to an operation requires that you
-- also pass the same value for all other parameters. We recommend that you
-- use a
-- <https://wikipedia.org/wiki/Universally_unique_identifier UUID type of value.>.
--
-- If you don\'t provide this value, then Amazon Web Services generates a
-- random one for you.
--
-- If you retry the operation with the same @ClientToken@, but with
-- different parameters, the retry fails with an
-- @IdempotentParameterMismatch@ error.
deletePermission_clientToken :: Lens.Lens' DeletePermission (Prelude.Maybe Prelude.Text)
deletePermission_clientToken = Lens.lens (\DeletePermission' {clientToken} -> clientToken) (\s@DeletePermission' {} a -> s {clientToken = a} :: DeletePermission)

-- | Specifies the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of the customer managed permission that you want to delete.
deletePermission_permissionArn :: Lens.Lens' DeletePermission Prelude.Text
deletePermission_permissionArn = Lens.lens (\DeletePermission' {permissionArn} -> permissionArn) (\s@DeletePermission' {} a -> s {permissionArn = a} :: DeletePermission)

instance Core.AWSRequest DeletePermission where
  type
    AWSResponse DeletePermission =
      DeletePermissionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeletePermissionResponse'
            Prelude.<$> (x Data..?> "clientToken")
            Prelude.<*> (x Data..?> "permissionStatus")
            Prelude.<*> (x Data..?> "returnValue")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeletePermission where
  hashWithSalt _salt DeletePermission' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` permissionArn

instance Prelude.NFData DeletePermission where
  rnf DeletePermission' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf permissionArn

instance Data.ToHeaders DeletePermission where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeletePermission where
  toPath = Prelude.const "/deletepermission"

instance Data.ToQuery DeletePermission where
  toQuery DeletePermission' {..} =
    Prelude.mconcat
      [ "clientToken" Data.=: clientToken,
        "permissionArn" Data.=: permissionArn
      ]

-- | /See:/ 'newDeletePermissionResponse' smart constructor.
data DeletePermissionResponse = DeletePermissionResponse'
  { -- | The idempotency identifier associated with this request. If you want to
    -- repeat the same operation in an idempotent manner then you must include
    -- this value in the @clientToken@ request parameter of that later call.
    -- All other parameters must also have the same values that you used in the
    -- first call.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | This operation is performed asynchronously, and this response parameter
    -- indicates the current status.
    permissionStatus :: Prelude.Maybe PermissionStatus,
    -- | A boolean that indicates whether the delete operations succeeded.
    returnValue :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePermissionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deletePermissionResponse_clientToken' - The idempotency identifier associated with this request. If you want to
-- repeat the same operation in an idempotent manner then you must include
-- this value in the @clientToken@ request parameter of that later call.
-- All other parameters must also have the same values that you used in the
-- first call.
--
-- 'permissionStatus', 'deletePermissionResponse_permissionStatus' - This operation is performed asynchronously, and this response parameter
-- indicates the current status.
--
-- 'returnValue', 'deletePermissionResponse_returnValue' - A boolean that indicates whether the delete operations succeeded.
--
-- 'httpStatus', 'deletePermissionResponse_httpStatus' - The response's http status code.
newDeletePermissionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeletePermissionResponse
newDeletePermissionResponse pHttpStatus_ =
  DeletePermissionResponse'
    { clientToken =
        Prelude.Nothing,
      permissionStatus = Prelude.Nothing,
      returnValue = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The idempotency identifier associated with this request. If you want to
-- repeat the same operation in an idempotent manner then you must include
-- this value in the @clientToken@ request parameter of that later call.
-- All other parameters must also have the same values that you used in the
-- first call.
deletePermissionResponse_clientToken :: Lens.Lens' DeletePermissionResponse (Prelude.Maybe Prelude.Text)
deletePermissionResponse_clientToken = Lens.lens (\DeletePermissionResponse' {clientToken} -> clientToken) (\s@DeletePermissionResponse' {} a -> s {clientToken = a} :: DeletePermissionResponse)

-- | This operation is performed asynchronously, and this response parameter
-- indicates the current status.
deletePermissionResponse_permissionStatus :: Lens.Lens' DeletePermissionResponse (Prelude.Maybe PermissionStatus)
deletePermissionResponse_permissionStatus = Lens.lens (\DeletePermissionResponse' {permissionStatus} -> permissionStatus) (\s@DeletePermissionResponse' {} a -> s {permissionStatus = a} :: DeletePermissionResponse)

-- | A boolean that indicates whether the delete operations succeeded.
deletePermissionResponse_returnValue :: Lens.Lens' DeletePermissionResponse (Prelude.Maybe Prelude.Bool)
deletePermissionResponse_returnValue = Lens.lens (\DeletePermissionResponse' {returnValue} -> returnValue) (\s@DeletePermissionResponse' {} a -> s {returnValue = a} :: DeletePermissionResponse)

-- | The response's http status code.
deletePermissionResponse_httpStatus :: Lens.Lens' DeletePermissionResponse Prelude.Int
deletePermissionResponse_httpStatus = Lens.lens (\DeletePermissionResponse' {httpStatus} -> httpStatus) (\s@DeletePermissionResponse' {} a -> s {httpStatus = a} :: DeletePermissionResponse)

instance Prelude.NFData DeletePermissionResponse where
  rnf DeletePermissionResponse' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf permissionStatus
      `Prelude.seq` Prelude.rnf returnValue
      `Prelude.seq` Prelude.rnf httpStatus
