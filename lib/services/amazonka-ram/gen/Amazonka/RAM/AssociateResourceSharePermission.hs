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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or replaces the RAM permission for a resource type included in a
-- resource share. You can have exactly one permission associated with each
-- resource type in the resource share. You can add a new RAM permission
-- only if there are currently no resources of that resource type currently
-- in the resource share.
module Amazonka.RAM.AssociateResourceSharePermission
  ( -- * Creating a Request
    AssociateResourceSharePermission (..),
    newAssociateResourceSharePermission,

    -- * Request Lenses
    associateResourceSharePermission_clientToken,
    associateResourceSharePermission_permissionVersion,
    associateResourceSharePermission_replace,
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
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateResourceSharePermission' smart constructor.
data AssociateResourceSharePermission = AssociateResourceSharePermission'
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
    -- | Specifies the version of the RAM permission to associate with the
    -- resource share. If you don\'t specify this parameter, the operation uses
    -- the version designated as the default. You can use the
    -- ListPermissionVersions operation to discover the available versions of a
    -- permission.
    permissionVersion :: Prelude.Maybe Prelude.Int,
    -- | Specifies whether the specified permission should replace or add to the
    -- existing permission associated with the resource share. Use @true@ to
    -- replace the current permissions. Use @false@ to add the permission to
    -- the current permission. The default value is @false@.
    --
    -- A resource share can have only one permission per resource type. If a
    -- resource share already has a permission for the specified resource type
    -- and you don\'t set @replace@ to @true@ then the operation returns an
    -- error. This helps prevent accidental overwriting of a permission.
    replace :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
    -- of the resource share to which you want to add or replace permissions.
    resourceShareArn :: Prelude.Text,
    -- | Specifies the
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
    -- of the RAM permission to associate with the resource share. To find the
    -- ARN for a permission, use either the ListPermissions operation or go to
    -- the
    -- <https://console.aws.amazon.com/ram/home#Permissions: Permissions library>
    -- page in the RAM console and then choose the name of the permission. The
    -- ARN is displayed on the detail page.
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
-- 'clientToken', 'associateResourceSharePermission_clientToken' - Specifies a unique, case-sensitive identifier that you provide to ensure
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
-- 'permissionVersion', 'associateResourceSharePermission_permissionVersion' - Specifies the version of the RAM permission to associate with the
-- resource share. If you don\'t specify this parameter, the operation uses
-- the version designated as the default. You can use the
-- ListPermissionVersions operation to discover the available versions of a
-- permission.
--
-- 'replace', 'associateResourceSharePermission_replace' - Specifies whether the specified permission should replace or add to the
-- existing permission associated with the resource share. Use @true@ to
-- replace the current permissions. Use @false@ to add the permission to
-- the current permission. The default value is @false@.
--
-- A resource share can have only one permission per resource type. If a
-- resource share already has a permission for the specified resource type
-- and you don\'t set @replace@ to @true@ then the operation returns an
-- error. This helps prevent accidental overwriting of a permission.
--
-- 'resourceShareArn', 'associateResourceSharePermission_resourceShareArn' - Specifies the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of the resource share to which you want to add or replace permissions.
--
-- 'permissionArn', 'associateResourceSharePermission_permissionArn' - Specifies the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of the RAM permission to associate with the resource share. To find the
-- ARN for a permission, use either the ListPermissions operation or go to
-- the
-- <https://console.aws.amazon.com/ram/home#Permissions: Permissions library>
-- page in the RAM console and then choose the name of the permission. The
-- ARN is displayed on the detail page.
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
      { clientToken =
          Prelude.Nothing,
        permissionVersion = Prelude.Nothing,
        replace = Prelude.Nothing,
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
associateResourceSharePermission_clientToken :: Lens.Lens' AssociateResourceSharePermission (Prelude.Maybe Prelude.Text)
associateResourceSharePermission_clientToken = Lens.lens (\AssociateResourceSharePermission' {clientToken} -> clientToken) (\s@AssociateResourceSharePermission' {} a -> s {clientToken = a} :: AssociateResourceSharePermission)

-- | Specifies the version of the RAM permission to associate with the
-- resource share. If you don\'t specify this parameter, the operation uses
-- the version designated as the default. You can use the
-- ListPermissionVersions operation to discover the available versions of a
-- permission.
associateResourceSharePermission_permissionVersion :: Lens.Lens' AssociateResourceSharePermission (Prelude.Maybe Prelude.Int)
associateResourceSharePermission_permissionVersion = Lens.lens (\AssociateResourceSharePermission' {permissionVersion} -> permissionVersion) (\s@AssociateResourceSharePermission' {} a -> s {permissionVersion = a} :: AssociateResourceSharePermission)

-- | Specifies whether the specified permission should replace or add to the
-- existing permission associated with the resource share. Use @true@ to
-- replace the current permissions. Use @false@ to add the permission to
-- the current permission. The default value is @false@.
--
-- A resource share can have only one permission per resource type. If a
-- resource share already has a permission for the specified resource type
-- and you don\'t set @replace@ to @true@ then the operation returns an
-- error. This helps prevent accidental overwriting of a permission.
associateResourceSharePermission_replace :: Lens.Lens' AssociateResourceSharePermission (Prelude.Maybe Prelude.Bool)
associateResourceSharePermission_replace = Lens.lens (\AssociateResourceSharePermission' {replace} -> replace) (\s@AssociateResourceSharePermission' {} a -> s {replace = a} :: AssociateResourceSharePermission)

-- | Specifies the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of the resource share to which you want to add or replace permissions.
associateResourceSharePermission_resourceShareArn :: Lens.Lens' AssociateResourceSharePermission Prelude.Text
associateResourceSharePermission_resourceShareArn = Lens.lens (\AssociateResourceSharePermission' {resourceShareArn} -> resourceShareArn) (\s@AssociateResourceSharePermission' {} a -> s {resourceShareArn = a} :: AssociateResourceSharePermission)

-- | Specifies the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of the RAM permission to associate with the resource share. To find the
-- ARN for a permission, use either the ListPermissions operation or go to
-- the
-- <https://console.aws.amazon.com/ram/home#Permissions: Permissions library>
-- page in the RAM console and then choose the name of the permission. The
-- ARN is displayed on the detail page.
associateResourceSharePermission_permissionArn :: Lens.Lens' AssociateResourceSharePermission Prelude.Text
associateResourceSharePermission_permissionArn = Lens.lens (\AssociateResourceSharePermission' {permissionArn} -> permissionArn) (\s@AssociateResourceSharePermission' {} a -> s {permissionArn = a} :: AssociateResourceSharePermission)

instance
  Core.AWSRequest
    AssociateResourceSharePermission
  where
  type
    AWSResponse AssociateResourceSharePermission =
      AssociateResourceSharePermissionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
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
  where
  hashWithSalt
    _salt
    AssociateResourceSharePermission' {..} =
      _salt `Prelude.hashWithSalt` clientToken
        `Prelude.hashWithSalt` permissionVersion
        `Prelude.hashWithSalt` replace
        `Prelude.hashWithSalt` resourceShareArn
        `Prelude.hashWithSalt` permissionArn

instance
  Prelude.NFData
    AssociateResourceSharePermission
  where
  rnf AssociateResourceSharePermission' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf permissionVersion
      `Prelude.seq` Prelude.rnf replace
      `Prelude.seq` Prelude.rnf resourceShareArn
      `Prelude.seq` Prelude.rnf permissionArn

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
          [ ("clientToken" Core..=) Prelude.<$> clientToken,
            ("permissionVersion" Core..=)
              Prelude.<$> permissionVersion,
            ("replace" Core..=) Prelude.<$> replace,
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
-- Create a value of 'AssociateResourceSharePermissionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'associateResourceSharePermissionResponse_clientToken' - The idempotency identifier associated with this request. If you want to
-- repeat the same operation in an idempotent manner then you must include
-- this value in the @clientToken@ request parameter of that later call.
-- All other parameters must also have the same values that you used in the
-- first call.
--
-- 'returnValue', 'associateResourceSharePermissionResponse_returnValue' - A return value of @true@ indicates that the request succeeded. A value
-- of @false@ indicates that the request failed.
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

-- | The idempotency identifier associated with this request. If you want to
-- repeat the same operation in an idempotent manner then you must include
-- this value in the @clientToken@ request parameter of that later call.
-- All other parameters must also have the same values that you used in the
-- first call.
associateResourceSharePermissionResponse_clientToken :: Lens.Lens' AssociateResourceSharePermissionResponse (Prelude.Maybe Prelude.Text)
associateResourceSharePermissionResponse_clientToken = Lens.lens (\AssociateResourceSharePermissionResponse' {clientToken} -> clientToken) (\s@AssociateResourceSharePermissionResponse' {} a -> s {clientToken = a} :: AssociateResourceSharePermissionResponse)

-- | A return value of @true@ indicates that the request succeeded. A value
-- of @false@ indicates that the request failed.
associateResourceSharePermissionResponse_returnValue :: Lens.Lens' AssociateResourceSharePermissionResponse (Prelude.Maybe Prelude.Bool)
associateResourceSharePermissionResponse_returnValue = Lens.lens (\AssociateResourceSharePermissionResponse' {returnValue} -> returnValue) (\s@AssociateResourceSharePermissionResponse' {} a -> s {returnValue = a} :: AssociateResourceSharePermissionResponse)

-- | The response's http status code.
associateResourceSharePermissionResponse_httpStatus :: Lens.Lens' AssociateResourceSharePermissionResponse Prelude.Int
associateResourceSharePermissionResponse_httpStatus = Lens.lens (\AssociateResourceSharePermissionResponse' {httpStatus} -> httpStatus) (\s@AssociateResourceSharePermissionResponse' {} a -> s {httpStatus = a} :: AssociateResourceSharePermissionResponse)

instance
  Prelude.NFData
    AssociateResourceSharePermissionResponse
  where
  rnf AssociateResourceSharePermissionResponse' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf returnValue
      `Prelude.seq` Prelude.rnf httpStatus
