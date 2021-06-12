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
-- Module      : Network.AWS.EC2.ModifyFpgaImageAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified attribute of the specified Amazon FPGA Image
-- (AFI).
module Network.AWS.EC2.ModifyFpgaImageAttribute
  ( -- * Creating a Request
    ModifyFpgaImageAttribute (..),
    newModifyFpgaImageAttribute,

    -- * Request Lenses
    modifyFpgaImageAttribute_dryRun,
    modifyFpgaImageAttribute_productCodes,
    modifyFpgaImageAttribute_userIds,
    modifyFpgaImageAttribute_name,
    modifyFpgaImageAttribute_attribute,
    modifyFpgaImageAttribute_description,
    modifyFpgaImageAttribute_userGroups,
    modifyFpgaImageAttribute_loadPermission,
    modifyFpgaImageAttribute_operationType,
    modifyFpgaImageAttribute_fpgaImageId,

    -- * Destructuring the Response
    ModifyFpgaImageAttributeResponse (..),
    newModifyFpgaImageAttributeResponse,

    -- * Response Lenses
    modifyFpgaImageAttributeResponse_fpgaImageAttribute,
    modifyFpgaImageAttributeResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyFpgaImageAttribute' smart constructor.
data ModifyFpgaImageAttribute = ModifyFpgaImageAttribute'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The product codes. After you add a product code to an AFI, it can\'t be
    -- removed. This parameter is valid only when modifying the @productCodes@
    -- attribute.
    productCodes :: Core.Maybe [Core.Text],
    -- | The AWS account IDs. This parameter is valid only when modifying the
    -- @loadPermission@ attribute.
    userIds :: Core.Maybe [Core.Text],
    -- | A name for the AFI.
    name :: Core.Maybe Core.Text,
    -- | The name of the attribute.
    attribute :: Core.Maybe FpgaImageAttributeName,
    -- | A description for the AFI.
    description :: Core.Maybe Core.Text,
    -- | The user groups. This parameter is valid only when modifying the
    -- @loadPermission@ attribute.
    userGroups :: Core.Maybe [Core.Text],
    -- | The load permission for the AFI.
    loadPermission :: Core.Maybe LoadPermissionModifications,
    -- | The operation type.
    operationType :: Core.Maybe OperationType,
    -- | The ID of the AFI.
    fpgaImageId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyFpgaImageAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'modifyFpgaImageAttribute_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'productCodes', 'modifyFpgaImageAttribute_productCodes' - The product codes. After you add a product code to an AFI, it can\'t be
-- removed. This parameter is valid only when modifying the @productCodes@
-- attribute.
--
-- 'userIds', 'modifyFpgaImageAttribute_userIds' - The AWS account IDs. This parameter is valid only when modifying the
-- @loadPermission@ attribute.
--
-- 'name', 'modifyFpgaImageAttribute_name' - A name for the AFI.
--
-- 'attribute', 'modifyFpgaImageAttribute_attribute' - The name of the attribute.
--
-- 'description', 'modifyFpgaImageAttribute_description' - A description for the AFI.
--
-- 'userGroups', 'modifyFpgaImageAttribute_userGroups' - The user groups. This parameter is valid only when modifying the
-- @loadPermission@ attribute.
--
-- 'loadPermission', 'modifyFpgaImageAttribute_loadPermission' - The load permission for the AFI.
--
-- 'operationType', 'modifyFpgaImageAttribute_operationType' - The operation type.
--
-- 'fpgaImageId', 'modifyFpgaImageAttribute_fpgaImageId' - The ID of the AFI.
newModifyFpgaImageAttribute ::
  -- | 'fpgaImageId'
  Core.Text ->
  ModifyFpgaImageAttribute
newModifyFpgaImageAttribute pFpgaImageId_ =
  ModifyFpgaImageAttribute'
    { dryRun = Core.Nothing,
      productCodes = Core.Nothing,
      userIds = Core.Nothing,
      name = Core.Nothing,
      attribute = Core.Nothing,
      description = Core.Nothing,
      userGroups = Core.Nothing,
      loadPermission = Core.Nothing,
      operationType = Core.Nothing,
      fpgaImageId = pFpgaImageId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyFpgaImageAttribute_dryRun :: Lens.Lens' ModifyFpgaImageAttribute (Core.Maybe Core.Bool)
modifyFpgaImageAttribute_dryRun = Lens.lens (\ModifyFpgaImageAttribute' {dryRun} -> dryRun) (\s@ModifyFpgaImageAttribute' {} a -> s {dryRun = a} :: ModifyFpgaImageAttribute)

-- | The product codes. After you add a product code to an AFI, it can\'t be
-- removed. This parameter is valid only when modifying the @productCodes@
-- attribute.
modifyFpgaImageAttribute_productCodes :: Lens.Lens' ModifyFpgaImageAttribute (Core.Maybe [Core.Text])
modifyFpgaImageAttribute_productCodes = Lens.lens (\ModifyFpgaImageAttribute' {productCodes} -> productCodes) (\s@ModifyFpgaImageAttribute' {} a -> s {productCodes = a} :: ModifyFpgaImageAttribute) Core.. Lens.mapping Lens._Coerce

-- | The AWS account IDs. This parameter is valid only when modifying the
-- @loadPermission@ attribute.
modifyFpgaImageAttribute_userIds :: Lens.Lens' ModifyFpgaImageAttribute (Core.Maybe [Core.Text])
modifyFpgaImageAttribute_userIds = Lens.lens (\ModifyFpgaImageAttribute' {userIds} -> userIds) (\s@ModifyFpgaImageAttribute' {} a -> s {userIds = a} :: ModifyFpgaImageAttribute) Core.. Lens.mapping Lens._Coerce

-- | A name for the AFI.
modifyFpgaImageAttribute_name :: Lens.Lens' ModifyFpgaImageAttribute (Core.Maybe Core.Text)
modifyFpgaImageAttribute_name = Lens.lens (\ModifyFpgaImageAttribute' {name} -> name) (\s@ModifyFpgaImageAttribute' {} a -> s {name = a} :: ModifyFpgaImageAttribute)

-- | The name of the attribute.
modifyFpgaImageAttribute_attribute :: Lens.Lens' ModifyFpgaImageAttribute (Core.Maybe FpgaImageAttributeName)
modifyFpgaImageAttribute_attribute = Lens.lens (\ModifyFpgaImageAttribute' {attribute} -> attribute) (\s@ModifyFpgaImageAttribute' {} a -> s {attribute = a} :: ModifyFpgaImageAttribute)

-- | A description for the AFI.
modifyFpgaImageAttribute_description :: Lens.Lens' ModifyFpgaImageAttribute (Core.Maybe Core.Text)
modifyFpgaImageAttribute_description = Lens.lens (\ModifyFpgaImageAttribute' {description} -> description) (\s@ModifyFpgaImageAttribute' {} a -> s {description = a} :: ModifyFpgaImageAttribute)

-- | The user groups. This parameter is valid only when modifying the
-- @loadPermission@ attribute.
modifyFpgaImageAttribute_userGroups :: Lens.Lens' ModifyFpgaImageAttribute (Core.Maybe [Core.Text])
modifyFpgaImageAttribute_userGroups = Lens.lens (\ModifyFpgaImageAttribute' {userGroups} -> userGroups) (\s@ModifyFpgaImageAttribute' {} a -> s {userGroups = a} :: ModifyFpgaImageAttribute) Core.. Lens.mapping Lens._Coerce

-- | The load permission for the AFI.
modifyFpgaImageAttribute_loadPermission :: Lens.Lens' ModifyFpgaImageAttribute (Core.Maybe LoadPermissionModifications)
modifyFpgaImageAttribute_loadPermission = Lens.lens (\ModifyFpgaImageAttribute' {loadPermission} -> loadPermission) (\s@ModifyFpgaImageAttribute' {} a -> s {loadPermission = a} :: ModifyFpgaImageAttribute)

-- | The operation type.
modifyFpgaImageAttribute_operationType :: Lens.Lens' ModifyFpgaImageAttribute (Core.Maybe OperationType)
modifyFpgaImageAttribute_operationType = Lens.lens (\ModifyFpgaImageAttribute' {operationType} -> operationType) (\s@ModifyFpgaImageAttribute' {} a -> s {operationType = a} :: ModifyFpgaImageAttribute)

-- | The ID of the AFI.
modifyFpgaImageAttribute_fpgaImageId :: Lens.Lens' ModifyFpgaImageAttribute Core.Text
modifyFpgaImageAttribute_fpgaImageId = Lens.lens (\ModifyFpgaImageAttribute' {fpgaImageId} -> fpgaImageId) (\s@ModifyFpgaImageAttribute' {} a -> s {fpgaImageId = a} :: ModifyFpgaImageAttribute)

instance Core.AWSRequest ModifyFpgaImageAttribute where
  type
    AWSResponse ModifyFpgaImageAttribute =
      ModifyFpgaImageAttributeResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyFpgaImageAttributeResponse'
            Core.<$> (x Core..@? "fpgaImageAttribute")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ModifyFpgaImageAttribute

instance Core.NFData ModifyFpgaImageAttribute

instance Core.ToHeaders ModifyFpgaImageAttribute where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ModifyFpgaImageAttribute where
  toPath = Core.const "/"

instance Core.ToQuery ModifyFpgaImageAttribute where
  toQuery ModifyFpgaImageAttribute' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ModifyFpgaImageAttribute" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        Core.toQuery
          ( Core.toQueryList "ProductCode"
              Core.<$> productCodes
          ),
        Core.toQuery
          (Core.toQueryList "UserId" Core.<$> userIds),
        "Name" Core.=: name,
        "Attribute" Core.=: attribute,
        "Description" Core.=: description,
        Core.toQuery
          (Core.toQueryList "UserGroup" Core.<$> userGroups),
        "LoadPermission" Core.=: loadPermission,
        "OperationType" Core.=: operationType,
        "FpgaImageId" Core.=: fpgaImageId
      ]

-- | /See:/ 'newModifyFpgaImageAttributeResponse' smart constructor.
data ModifyFpgaImageAttributeResponse = ModifyFpgaImageAttributeResponse'
  { -- | Information about the attribute.
    fpgaImageAttribute :: Core.Maybe FpgaImageAttribute,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyFpgaImageAttributeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fpgaImageAttribute', 'modifyFpgaImageAttributeResponse_fpgaImageAttribute' - Information about the attribute.
--
-- 'httpStatus', 'modifyFpgaImageAttributeResponse_httpStatus' - The response's http status code.
newModifyFpgaImageAttributeResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ModifyFpgaImageAttributeResponse
newModifyFpgaImageAttributeResponse pHttpStatus_ =
  ModifyFpgaImageAttributeResponse'
    { fpgaImageAttribute =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the attribute.
modifyFpgaImageAttributeResponse_fpgaImageAttribute :: Lens.Lens' ModifyFpgaImageAttributeResponse (Core.Maybe FpgaImageAttribute)
modifyFpgaImageAttributeResponse_fpgaImageAttribute = Lens.lens (\ModifyFpgaImageAttributeResponse' {fpgaImageAttribute} -> fpgaImageAttribute) (\s@ModifyFpgaImageAttributeResponse' {} a -> s {fpgaImageAttribute = a} :: ModifyFpgaImageAttributeResponse)

-- | The response's http status code.
modifyFpgaImageAttributeResponse_httpStatus :: Lens.Lens' ModifyFpgaImageAttributeResponse Core.Int
modifyFpgaImageAttributeResponse_httpStatus = Lens.lens (\ModifyFpgaImageAttributeResponse' {httpStatus} -> httpStatus) (\s@ModifyFpgaImageAttributeResponse' {} a -> s {httpStatus = a} :: ModifyFpgaImageAttributeResponse)

instance Core.NFData ModifyFpgaImageAttributeResponse
