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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates an RAM permission from a resource share.
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
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateResourceSharePermission' smart constructor.
data DisassociateResourceSharePermission = DisassociateResourceSharePermission'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the resource share.
    resourceShareArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the permission to disassociate from
    -- the resource share.
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
-- 'clientToken', 'disassociateResourceSharePermission_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'resourceShareArn', 'disassociateResourceSharePermission_resourceShareArn' - The Amazon Resource Name (ARN) of the resource share.
--
-- 'permissionArn', 'disassociateResourceSharePermission_permissionArn' - The Amazon Resource Name (ARN) of the permission to disassociate from
-- the resource share.
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

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
disassociateResourceSharePermission_clientToken :: Lens.Lens' DisassociateResourceSharePermission (Prelude.Maybe Prelude.Text)
disassociateResourceSharePermission_clientToken = Lens.lens (\DisassociateResourceSharePermission' {clientToken} -> clientToken) (\s@DisassociateResourceSharePermission' {} a -> s {clientToken = a} :: DisassociateResourceSharePermission)

-- | The Amazon Resource Name (ARN) of the resource share.
disassociateResourceSharePermission_resourceShareArn :: Lens.Lens' DisassociateResourceSharePermission Prelude.Text
disassociateResourceSharePermission_resourceShareArn = Lens.lens (\DisassociateResourceSharePermission' {resourceShareArn} -> resourceShareArn) (\s@DisassociateResourceSharePermission' {} a -> s {resourceShareArn = a} :: DisassociateResourceSharePermission)

-- | The Amazon Resource Name (ARN) of the permission to disassociate from
-- the resource share.
disassociateResourceSharePermission_permissionArn :: Lens.Lens' DisassociateResourceSharePermission Prelude.Text
disassociateResourceSharePermission_permissionArn = Lens.lens (\DisassociateResourceSharePermission' {permissionArn} -> permissionArn) (\s@DisassociateResourceSharePermission' {} a -> s {permissionArn = a} :: DisassociateResourceSharePermission)

instance
  Core.AWSRequest
    DisassociateResourceSharePermission
  where
  type
    AWSResponse DisassociateResourceSharePermission =
      DisassociateResourceSharePermissionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociateResourceSharePermissionResponse'
            Prelude.<$> (x Core..?> "clientToken")
              Prelude.<*> (x Core..?> "returnValue")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateResourceSharePermission
  where
  hashWithSalt
    salt'
    DisassociateResourceSharePermission' {..} =
      salt' `Prelude.hashWithSalt` permissionArn
        `Prelude.hashWithSalt` resourceShareArn
        `Prelude.hashWithSalt` clientToken

instance
  Prelude.NFData
    DisassociateResourceSharePermission
  where
  rnf DisassociateResourceSharePermission' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf permissionArn
      `Prelude.seq` Prelude.rnf resourceShareArn

instance
  Core.ToHeaders
    DisassociateResourceSharePermission
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    DisassociateResourceSharePermission
  where
  toJSON DisassociateResourceSharePermission' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("clientToken" Core..=) Prelude.<$> clientToken,
            Prelude.Just
              ("resourceShareArn" Core..= resourceShareArn),
            Prelude.Just
              ("permissionArn" Core..= permissionArn)
          ]
      )

instance
  Core.ToPath
    DisassociateResourceSharePermission
  where
  toPath =
    Prelude.const
      "/disassociateresourcesharepermission"

instance
  Core.ToQuery
    DisassociateResourceSharePermission
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateResourceSharePermissionResponse' smart constructor.
data DisassociateResourceSharePermissionResponse = DisassociateResourceSharePermissionResponse'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the request succeeded.
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
-- 'clientToken', 'disassociateResourceSharePermissionResponse_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'returnValue', 'disassociateResourceSharePermissionResponse_returnValue' - Indicates whether the request succeeded.
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

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
disassociateResourceSharePermissionResponse_clientToken :: Lens.Lens' DisassociateResourceSharePermissionResponse (Prelude.Maybe Prelude.Text)
disassociateResourceSharePermissionResponse_clientToken = Lens.lens (\DisassociateResourceSharePermissionResponse' {clientToken} -> clientToken) (\s@DisassociateResourceSharePermissionResponse' {} a -> s {clientToken = a} :: DisassociateResourceSharePermissionResponse)

-- | Indicates whether the request succeeded.
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
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf returnValue
