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
-- Module      : Amazonka.RAM.DeletePermissionVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one version of a customer managed permission. The version you
-- specify must not be attached to any resource share and must not be the
-- default version for the permission.
--
-- If a customer managed permission has the maximum of 5 versions, then you
-- must delete at least one version before you can create another.
module Amazonka.RAM.DeletePermissionVersion
  ( -- * Creating a Request
    DeletePermissionVersion (..),
    newDeletePermissionVersion,

    -- * Request Lenses
    deletePermissionVersion_clientToken,
    deletePermissionVersion_permissionArn,
    deletePermissionVersion_permissionVersion,

    -- * Destructuring the Response
    DeletePermissionVersionResponse (..),
    newDeletePermissionVersionResponse,

    -- * Response Lenses
    deletePermissionVersionResponse_clientToken,
    deletePermissionVersionResponse_permissionStatus,
    deletePermissionVersionResponse_returnValue,
    deletePermissionVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeletePermissionVersion' smart constructor.
data DeletePermissionVersion = DeletePermissionVersion'
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
    -- of the permission with the version you want to delete.
    permissionArn :: Prelude.Text,
    -- | Specifies the version number to delete.
    --
    -- You can\'t delete the default version for a customer managed permission.
    --
    -- You can\'t delete a version if it\'s the only version of the permission.
    -- You must either first create another version, or delete the permission
    -- completely.
    --
    -- You can\'t delete a version if it is attached to any resource shares. If
    -- the version is the default, you must first use
    -- SetDefaultPermissionVersion to set a different version as the default
    -- for the customer managed permission, and then use
    -- AssociateResourceSharePermission to update your resource shares to use
    -- the new default version.
    permissionVersion :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePermissionVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deletePermissionVersion_clientToken' - Specifies a unique, case-sensitive identifier that you provide to ensure
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
-- 'permissionArn', 'deletePermissionVersion_permissionArn' - Specifies the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of the permission with the version you want to delete.
--
-- 'permissionVersion', 'deletePermissionVersion_permissionVersion' - Specifies the version number to delete.
--
-- You can\'t delete the default version for a customer managed permission.
--
-- You can\'t delete a version if it\'s the only version of the permission.
-- You must either first create another version, or delete the permission
-- completely.
--
-- You can\'t delete a version if it is attached to any resource shares. If
-- the version is the default, you must first use
-- SetDefaultPermissionVersion to set a different version as the default
-- for the customer managed permission, and then use
-- AssociateResourceSharePermission to update your resource shares to use
-- the new default version.
newDeletePermissionVersion ::
  -- | 'permissionArn'
  Prelude.Text ->
  -- | 'permissionVersion'
  Prelude.Int ->
  DeletePermissionVersion
newDeletePermissionVersion
  pPermissionArn_
  pPermissionVersion_ =
    DeletePermissionVersion'
      { clientToken =
          Prelude.Nothing,
        permissionArn = pPermissionArn_,
        permissionVersion = pPermissionVersion_
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
deletePermissionVersion_clientToken :: Lens.Lens' DeletePermissionVersion (Prelude.Maybe Prelude.Text)
deletePermissionVersion_clientToken = Lens.lens (\DeletePermissionVersion' {clientToken} -> clientToken) (\s@DeletePermissionVersion' {} a -> s {clientToken = a} :: DeletePermissionVersion)

-- | Specifies the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of the permission with the version you want to delete.
deletePermissionVersion_permissionArn :: Lens.Lens' DeletePermissionVersion Prelude.Text
deletePermissionVersion_permissionArn = Lens.lens (\DeletePermissionVersion' {permissionArn} -> permissionArn) (\s@DeletePermissionVersion' {} a -> s {permissionArn = a} :: DeletePermissionVersion)

-- | Specifies the version number to delete.
--
-- You can\'t delete the default version for a customer managed permission.
--
-- You can\'t delete a version if it\'s the only version of the permission.
-- You must either first create another version, or delete the permission
-- completely.
--
-- You can\'t delete a version if it is attached to any resource shares. If
-- the version is the default, you must first use
-- SetDefaultPermissionVersion to set a different version as the default
-- for the customer managed permission, and then use
-- AssociateResourceSharePermission to update your resource shares to use
-- the new default version.
deletePermissionVersion_permissionVersion :: Lens.Lens' DeletePermissionVersion Prelude.Int
deletePermissionVersion_permissionVersion = Lens.lens (\DeletePermissionVersion' {permissionVersion} -> permissionVersion) (\s@DeletePermissionVersion' {} a -> s {permissionVersion = a} :: DeletePermissionVersion)

instance Core.AWSRequest DeletePermissionVersion where
  type
    AWSResponse DeletePermissionVersion =
      DeletePermissionVersionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeletePermissionVersionResponse'
            Prelude.<$> (x Data..?> "clientToken")
            Prelude.<*> (x Data..?> "permissionStatus")
            Prelude.<*> (x Data..?> "returnValue")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeletePermissionVersion where
  hashWithSalt _salt DeletePermissionVersion' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` permissionArn
      `Prelude.hashWithSalt` permissionVersion

instance Prelude.NFData DeletePermissionVersion where
  rnf DeletePermissionVersion' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf permissionArn
      `Prelude.seq` Prelude.rnf permissionVersion

instance Data.ToHeaders DeletePermissionVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeletePermissionVersion where
  toPath = Prelude.const "/deletepermissionversion"

instance Data.ToQuery DeletePermissionVersion where
  toQuery DeletePermissionVersion' {..} =
    Prelude.mconcat
      [ "clientToken" Data.=: clientToken,
        "permissionArn" Data.=: permissionArn,
        "permissionVersion" Data.=: permissionVersion
      ]

-- | /See:/ 'newDeletePermissionVersionResponse' smart constructor.
data DeletePermissionVersionResponse = DeletePermissionVersionResponse'
  { -- | The idempotency identifier associated with this request. If you want to
    -- repeat the same operation in an idempotent manner then you must include
    -- this value in the @clientToken@ request parameter of that later call.
    -- All other parameters must also have the same values that you used in the
    -- first call.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | This operation is performed asynchronously, and this response parameter
    -- indicates the current status.
    permissionStatus :: Prelude.Maybe PermissionStatus,
    -- | A boolean value that indicates whether the operation is successful.
    returnValue :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePermissionVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deletePermissionVersionResponse_clientToken' - The idempotency identifier associated with this request. If you want to
-- repeat the same operation in an idempotent manner then you must include
-- this value in the @clientToken@ request parameter of that later call.
-- All other parameters must also have the same values that you used in the
-- first call.
--
-- 'permissionStatus', 'deletePermissionVersionResponse_permissionStatus' - This operation is performed asynchronously, and this response parameter
-- indicates the current status.
--
-- 'returnValue', 'deletePermissionVersionResponse_returnValue' - A boolean value that indicates whether the operation is successful.
--
-- 'httpStatus', 'deletePermissionVersionResponse_httpStatus' - The response's http status code.
newDeletePermissionVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeletePermissionVersionResponse
newDeletePermissionVersionResponse pHttpStatus_ =
  DeletePermissionVersionResponse'
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
deletePermissionVersionResponse_clientToken :: Lens.Lens' DeletePermissionVersionResponse (Prelude.Maybe Prelude.Text)
deletePermissionVersionResponse_clientToken = Lens.lens (\DeletePermissionVersionResponse' {clientToken} -> clientToken) (\s@DeletePermissionVersionResponse' {} a -> s {clientToken = a} :: DeletePermissionVersionResponse)

-- | This operation is performed asynchronously, and this response parameter
-- indicates the current status.
deletePermissionVersionResponse_permissionStatus :: Lens.Lens' DeletePermissionVersionResponse (Prelude.Maybe PermissionStatus)
deletePermissionVersionResponse_permissionStatus = Lens.lens (\DeletePermissionVersionResponse' {permissionStatus} -> permissionStatus) (\s@DeletePermissionVersionResponse' {} a -> s {permissionStatus = a} :: DeletePermissionVersionResponse)

-- | A boolean value that indicates whether the operation is successful.
deletePermissionVersionResponse_returnValue :: Lens.Lens' DeletePermissionVersionResponse (Prelude.Maybe Prelude.Bool)
deletePermissionVersionResponse_returnValue = Lens.lens (\DeletePermissionVersionResponse' {returnValue} -> returnValue) (\s@DeletePermissionVersionResponse' {} a -> s {returnValue = a} :: DeletePermissionVersionResponse)

-- | The response's http status code.
deletePermissionVersionResponse_httpStatus :: Lens.Lens' DeletePermissionVersionResponse Prelude.Int
deletePermissionVersionResponse_httpStatus = Lens.lens (\DeletePermissionVersionResponse' {httpStatus} -> httpStatus) (\s@DeletePermissionVersionResponse' {} a -> s {httpStatus = a} :: DeletePermissionVersionResponse)

instance
  Prelude.NFData
    DeletePermissionVersionResponse
  where
  rnf DeletePermissionVersionResponse' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf permissionStatus
      `Prelude.seq` Prelude.rnf returnValue
      `Prelude.seq` Prelude.rnf httpStatus
