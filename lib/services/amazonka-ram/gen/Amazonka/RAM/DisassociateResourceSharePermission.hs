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
-- Module      : Amazonka.RAM.DisassociateResourceSharePermission
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates an RAM permission from a resource share. Permission
-- changes take effect immediately. You can remove a RAM permission from a
-- resource share only if there are currently no resources of the relevant
-- resource type currently attached to the resource share.
module Amazonka.RAM.DisassociateResourceSharePermission
  ( -- * Creating a Request
    DisassociateResourceSharePermission (..),
    newDisassociateResourceSharePermission,

    -- * Request Lenses
    disassociateResourceSharePermission_clientToken,
    disassociateResourceSharePermission_resourceShareArn,
    disassociateResourceSharePermission_permissionArn,

    -- * Destructuring the Response
    DisassociateResourceSharePermissionResponse (..),
    newDisassociateResourceSharePermissionResponse,

    -- * Response Lenses
    disassociateResourceSharePermissionResponse_clientToken,
    disassociateResourceSharePermissionResponse_returnValue,
    disassociateResourceSharePermissionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateResourceSharePermission' smart constructor.
data DisassociateResourceSharePermission = DisassociateResourceSharePermission'
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
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
    -- of the resource share from which you want to disassociate a permission.
    resourceShareArn :: Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
    -- of the permission to disassociate from the resource share. Changes to
    -- permissions take effect immediately.
    permissionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateResourceSharePermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'disassociateResourceSharePermission_clientToken' - Specifies a unique, case-sensitive identifier that you provide to ensure
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
-- 'resourceShareArn', 'disassociateResourceSharePermission_resourceShareArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of the resource share from which you want to disassociate a permission.
--
-- 'permissionArn', 'disassociateResourceSharePermission_permissionArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of the permission to disassociate from the resource share. Changes to
-- permissions take effect immediately.
newDisassociateResourceSharePermission ::
  -- | 'resourceShareArn'
  Prelude.Text ->
  -- | 'permissionArn'
  Prelude.Text ->
  DisassociateResourceSharePermission
newDisassociateResourceSharePermission
  pResourceShareArn_
  pPermissionArn_ =
    DisassociateResourceSharePermission'
      { clientToken =
          Prelude.Nothing,
        resourceShareArn = pResourceShareArn_,
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
disassociateResourceSharePermission_clientToken :: Lens.Lens' DisassociateResourceSharePermission (Prelude.Maybe Prelude.Text)
disassociateResourceSharePermission_clientToken = Lens.lens (\DisassociateResourceSharePermission' {clientToken} -> clientToken) (\s@DisassociateResourceSharePermission' {} a -> s {clientToken = a} :: DisassociateResourceSharePermission)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of the resource share from which you want to disassociate a permission.
disassociateResourceSharePermission_resourceShareArn :: Lens.Lens' DisassociateResourceSharePermission Prelude.Text
disassociateResourceSharePermission_resourceShareArn = Lens.lens (\DisassociateResourceSharePermission' {resourceShareArn} -> resourceShareArn) (\s@DisassociateResourceSharePermission' {} a -> s {resourceShareArn = a} :: DisassociateResourceSharePermission)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of the permission to disassociate from the resource share. Changes to
-- permissions take effect immediately.
disassociateResourceSharePermission_permissionArn :: Lens.Lens' DisassociateResourceSharePermission Prelude.Text
disassociateResourceSharePermission_permissionArn = Lens.lens (\DisassociateResourceSharePermission' {permissionArn} -> permissionArn) (\s@DisassociateResourceSharePermission' {} a -> s {permissionArn = a} :: DisassociateResourceSharePermission)

instance
  Core.AWSRequest
    DisassociateResourceSharePermission
  where
  type
    AWSResponse DisassociateResourceSharePermission =
      DisassociateResourceSharePermissionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociateResourceSharePermissionResponse'
            Prelude.<$> (x Data..?> "clientToken")
              Prelude.<*> (x Data..?> "returnValue")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateResourceSharePermission
  where
  hashWithSalt
    _salt
    DisassociateResourceSharePermission' {..} =
      _salt `Prelude.hashWithSalt` clientToken
        `Prelude.hashWithSalt` resourceShareArn
        `Prelude.hashWithSalt` permissionArn

instance
  Prelude.NFData
    DisassociateResourceSharePermission
  where
  rnf DisassociateResourceSharePermission' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf resourceShareArn
      `Prelude.seq` Prelude.rnf permissionArn

instance
  Data.ToHeaders
    DisassociateResourceSharePermission
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DisassociateResourceSharePermission
  where
  toJSON DisassociateResourceSharePermission' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just
              ("resourceShareArn" Data..= resourceShareArn),
            Prelude.Just
              ("permissionArn" Data..= permissionArn)
          ]
      )

instance
  Data.ToPath
    DisassociateResourceSharePermission
  where
  toPath =
    Prelude.const
      "/disassociateresourcesharepermission"

instance
  Data.ToQuery
    DisassociateResourceSharePermission
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateResourceSharePermissionResponse' smart constructor.
data DisassociateResourceSharePermissionResponse = DisassociateResourceSharePermissionResponse'
  { -- | The idempotency identifier associated with this request. If you want to
    -- repeat the same operation in an idempotent manner then you must include
    -- this value in the @clientToken@ request parameter of that later call.
    -- All other parameters must also have the same values that you used in the
    -- first call.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A return value of @true@ indicates that the request succeeded. A value
    -- of @false@ indicates that the request failed.
    returnValue :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateResourceSharePermissionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'disassociateResourceSharePermissionResponse_clientToken' - The idempotency identifier associated with this request. If you want to
-- repeat the same operation in an idempotent manner then you must include
-- this value in the @clientToken@ request parameter of that later call.
-- All other parameters must also have the same values that you used in the
-- first call.
--
-- 'returnValue', 'disassociateResourceSharePermissionResponse_returnValue' - A return value of @true@ indicates that the request succeeded. A value
-- of @false@ indicates that the request failed.
--
-- 'httpStatus', 'disassociateResourceSharePermissionResponse_httpStatus' - The response's http status code.
newDisassociateResourceSharePermissionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateResourceSharePermissionResponse
newDisassociateResourceSharePermissionResponse
  pHttpStatus_ =
    DisassociateResourceSharePermissionResponse'
      { clientToken =
          Prelude.Nothing,
        returnValue = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The idempotency identifier associated with this request. If you want to
-- repeat the same operation in an idempotent manner then you must include
-- this value in the @clientToken@ request parameter of that later call.
-- All other parameters must also have the same values that you used in the
-- first call.
disassociateResourceSharePermissionResponse_clientToken :: Lens.Lens' DisassociateResourceSharePermissionResponse (Prelude.Maybe Prelude.Text)
disassociateResourceSharePermissionResponse_clientToken = Lens.lens (\DisassociateResourceSharePermissionResponse' {clientToken} -> clientToken) (\s@DisassociateResourceSharePermissionResponse' {} a -> s {clientToken = a} :: DisassociateResourceSharePermissionResponse)

-- | A return value of @true@ indicates that the request succeeded. A value
-- of @false@ indicates that the request failed.
disassociateResourceSharePermissionResponse_returnValue :: Lens.Lens' DisassociateResourceSharePermissionResponse (Prelude.Maybe Prelude.Bool)
disassociateResourceSharePermissionResponse_returnValue = Lens.lens (\DisassociateResourceSharePermissionResponse' {returnValue} -> returnValue) (\s@DisassociateResourceSharePermissionResponse' {} a -> s {returnValue = a} :: DisassociateResourceSharePermissionResponse)

-- | The response's http status code.
disassociateResourceSharePermissionResponse_httpStatus :: Lens.Lens' DisassociateResourceSharePermissionResponse Prelude.Int
disassociateResourceSharePermissionResponse_httpStatus = Lens.lens (\DisassociateResourceSharePermissionResponse' {httpStatus} -> httpStatus) (\s@DisassociateResourceSharePermissionResponse' {} a -> s {httpStatus = a} :: DisassociateResourceSharePermissionResponse)

instance
  Prelude.NFData
    DisassociateResourceSharePermissionResponse
  where
  rnf DisassociateResourceSharePermissionResponse' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf returnValue
      `Prelude.seq` Prelude.rnf httpStatus
