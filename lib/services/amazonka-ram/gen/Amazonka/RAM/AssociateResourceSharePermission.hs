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
-- Module      : Amazonka.RAM.AssociateResourceSharePermission
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a permission with a resource share.
module Amazonka.RAM.AssociateResourceSharePermission
  ( -- * Creating a Request
    AssociateResourceSharePermission (..),
    newAssociateResourceSharePermission,

    -- * Request Lenses
    associateResourceSharePermission_replace,
    associateResourceSharePermission_clientToken,
    associateResourceSharePermission_permissionVersion,
    associateResourceSharePermission_resourceShareArn,
    associateResourceSharePermission_permissionArn,

    -- * Destructuring the Response
    AssociateResourceSharePermissionResponse (..),
    newAssociateResourceSharePermissionResponse,

    -- * Response Lenses
    associateResourceSharePermissionResponse_clientToken,
    associateResourceSharePermissionResponse_returnValue,
    associateResourceSharePermissionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateResourceSharePermission' smart constructor.
data AssociateResourceSharePermission = AssociateResourceSharePermission'
  { -- | Indicates whether the permission should replace the permissions that are
    -- currently associated with the resource share. Use @true@ to replace the
    -- current permissions. Use @false@ to add the permission to the current
    -- permission.
    replace :: Prelude.Maybe Prelude.Bool,
    -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The version of the RAM permissions to associate with the resource share.
    permissionVersion :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the resource share.
    resourceShareArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the RAM permission to associate with
    -- the resource share.
    permissionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateResourceSharePermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replace', 'associateResourceSharePermission_replace' - Indicates whether the permission should replace the permissions that are
-- currently associated with the resource share. Use @true@ to replace the
-- current permissions. Use @false@ to add the permission to the current
-- permission.
--
-- 'clientToken', 'associateResourceSharePermission_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'permissionVersion', 'associateResourceSharePermission_permissionVersion' - The version of the RAM permissions to associate with the resource share.
--
-- 'resourceShareArn', 'associateResourceSharePermission_resourceShareArn' - The Amazon Resource Name (ARN) of the resource share.
--
-- 'permissionArn', 'associateResourceSharePermission_permissionArn' - The Amazon Resource Name (ARN) of the RAM permission to associate with
-- the resource share.
newAssociateResourceSharePermission ::
  -- | 'resourceShareArn'
  Prelude.Text ->
  -- | 'permissionArn'
  Prelude.Text ->
  AssociateResourceSharePermission
newAssociateResourceSharePermission
  pResourceShareArn_
  pPermissionArn_ =
    AssociateResourceSharePermission'
      { replace =
          Prelude.Nothing,
        clientToken = Prelude.Nothing,
        permissionVersion = Prelude.Nothing,
        resourceShareArn = pResourceShareArn_,
        permissionArn = pPermissionArn_
      }

-- | Indicates whether the permission should replace the permissions that are
-- currently associated with the resource share. Use @true@ to replace the
-- current permissions. Use @false@ to add the permission to the current
-- permission.
associateResourceSharePermission_replace :: Lens.Lens' AssociateResourceSharePermission (Prelude.Maybe Prelude.Bool)
associateResourceSharePermission_replace = Lens.lens (\AssociateResourceSharePermission' {replace} -> replace) (\s@AssociateResourceSharePermission' {} a -> s {replace = a} :: AssociateResourceSharePermission)

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
associateResourceSharePermission_clientToken :: Lens.Lens' AssociateResourceSharePermission (Prelude.Maybe Prelude.Text)
associateResourceSharePermission_clientToken = Lens.lens (\AssociateResourceSharePermission' {clientToken} -> clientToken) (\s@AssociateResourceSharePermission' {} a -> s {clientToken = a} :: AssociateResourceSharePermission)

-- | The version of the RAM permissions to associate with the resource share.
associateResourceSharePermission_permissionVersion :: Lens.Lens' AssociateResourceSharePermission (Prelude.Maybe Prelude.Int)
associateResourceSharePermission_permissionVersion = Lens.lens (\AssociateResourceSharePermission' {permissionVersion} -> permissionVersion) (\s@AssociateResourceSharePermission' {} a -> s {permissionVersion = a} :: AssociateResourceSharePermission)

-- | The Amazon Resource Name (ARN) of the resource share.
associateResourceSharePermission_resourceShareArn :: Lens.Lens' AssociateResourceSharePermission Prelude.Text
associateResourceSharePermission_resourceShareArn = Lens.lens (\AssociateResourceSharePermission' {resourceShareArn} -> resourceShareArn) (\s@AssociateResourceSharePermission' {} a -> s {resourceShareArn = a} :: AssociateResourceSharePermission)

-- | The Amazon Resource Name (ARN) of the RAM permission to associate with
-- the resource share.
associateResourceSharePermission_permissionArn :: Lens.Lens' AssociateResourceSharePermission Prelude.Text
associateResourceSharePermission_permissionArn = Lens.lens (\AssociateResourceSharePermission' {permissionArn} -> permissionArn) (\s@AssociateResourceSharePermission' {} a -> s {permissionArn = a} :: AssociateResourceSharePermission)

instance
  Core.AWSRequest
    AssociateResourceSharePermission
  where
  type
    AWSResponse AssociateResourceSharePermission =
      AssociateResourceSharePermissionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateResourceSharePermissionResponse'
            Prelude.<$> (x Core..?> "clientToken")
            Prelude.<*> (x Core..?> "returnValue")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociateResourceSharePermission

instance
  Prelude.NFData
    AssociateResourceSharePermission

instance
  Core.ToHeaders
    AssociateResourceSharePermission
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

instance Core.ToJSON AssociateResourceSharePermission where
  toJSON AssociateResourceSharePermission' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("replace" Core..=) Prelude.<$> replace,
            ("clientToken" Core..=) Prelude.<$> clientToken,
            ("permissionVersion" Core..=)
              Prelude.<$> permissionVersion,
            Prelude.Just
              ("resourceShareArn" Core..= resourceShareArn),
            Prelude.Just
              ("permissionArn" Core..= permissionArn)
          ]
      )

instance Core.ToPath AssociateResourceSharePermission where
  toPath =
    Prelude.const "/associateresourcesharepermission"

instance
  Core.ToQuery
    AssociateResourceSharePermission
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateResourceSharePermissionResponse' smart constructor.
data AssociateResourceSharePermissionResponse = AssociateResourceSharePermissionResponse'
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
-- Create a value of 'AssociateResourceSharePermissionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'associateResourceSharePermissionResponse_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'returnValue', 'associateResourceSharePermissionResponse_returnValue' - Indicates whether the request succeeded.
--
-- 'httpStatus', 'associateResourceSharePermissionResponse_httpStatus' - The response's http status code.
newAssociateResourceSharePermissionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateResourceSharePermissionResponse
newAssociateResourceSharePermissionResponse
  pHttpStatus_ =
    AssociateResourceSharePermissionResponse'
      { clientToken =
          Prelude.Nothing,
        returnValue = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
associateResourceSharePermissionResponse_clientToken :: Lens.Lens' AssociateResourceSharePermissionResponse (Prelude.Maybe Prelude.Text)
associateResourceSharePermissionResponse_clientToken = Lens.lens (\AssociateResourceSharePermissionResponse' {clientToken} -> clientToken) (\s@AssociateResourceSharePermissionResponse' {} a -> s {clientToken = a} :: AssociateResourceSharePermissionResponse)

-- | Indicates whether the request succeeded.
associateResourceSharePermissionResponse_returnValue :: Lens.Lens' AssociateResourceSharePermissionResponse (Prelude.Maybe Prelude.Bool)
associateResourceSharePermissionResponse_returnValue = Lens.lens (\AssociateResourceSharePermissionResponse' {returnValue} -> returnValue) (\s@AssociateResourceSharePermissionResponse' {} a -> s {returnValue = a} :: AssociateResourceSharePermissionResponse)

-- | The response's http status code.
associateResourceSharePermissionResponse_httpStatus :: Lens.Lens' AssociateResourceSharePermissionResponse Prelude.Int
associateResourceSharePermissionResponse_httpStatus = Lens.lens (\AssociateResourceSharePermissionResponse' {httpStatus} -> httpStatus) (\s@AssociateResourceSharePermissionResponse' {} a -> s {httpStatus = a} :: AssociateResourceSharePermissionResponse)

instance
  Prelude.NFData
    AssociateResourceSharePermissionResponse
