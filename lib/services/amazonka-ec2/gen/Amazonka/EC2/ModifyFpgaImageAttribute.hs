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
-- Module      : Amazonka.EC2.ModifyFpgaImageAttribute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified attribute of the specified Amazon FPGA Image
-- (AFI).
module Amazonka.EC2.ModifyFpgaImageAttribute
  ( -- * Creating a Request
    ModifyFpgaImageAttribute (..),
    newModifyFpgaImageAttribute,

    -- * Request Lenses
    modifyFpgaImageAttribute_name,
    modifyFpgaImageAttribute_operationType,
    modifyFpgaImageAttribute_attribute,
    modifyFpgaImageAttribute_productCodes,
    modifyFpgaImageAttribute_userGroups,
    modifyFpgaImageAttribute_description,
    modifyFpgaImageAttribute_dryRun,
    modifyFpgaImageAttribute_loadPermission,
    modifyFpgaImageAttribute_userIds,
    modifyFpgaImageAttribute_fpgaImageId,

    -- * Destructuring the Response
    ModifyFpgaImageAttributeResponse (..),
    newModifyFpgaImageAttributeResponse,

    -- * Response Lenses
    modifyFpgaImageAttributeResponse_fpgaImageAttribute,
    modifyFpgaImageAttributeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyFpgaImageAttribute' smart constructor.
data ModifyFpgaImageAttribute = ModifyFpgaImageAttribute'
  { -- | A name for the AFI.
    name :: Prelude.Maybe Prelude.Text,
    -- | The operation type.
    operationType :: Prelude.Maybe OperationType,
    -- | The name of the attribute.
    attribute :: Prelude.Maybe FpgaImageAttributeName,
    -- | The product codes. After you add a product code to an AFI, it can\'t be
    -- removed. This parameter is valid only when modifying the @productCodes@
    -- attribute.
    productCodes :: Prelude.Maybe [Prelude.Text],
    -- | The user groups. This parameter is valid only when modifying the
    -- @loadPermission@ attribute.
    userGroups :: Prelude.Maybe [Prelude.Text],
    -- | A description for the AFI.
    description :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The load permission for the AFI.
    loadPermission :: Prelude.Maybe LoadPermissionModifications,
    -- | The Amazon Web Services account IDs. This parameter is valid only when
    -- modifying the @loadPermission@ attribute.
    userIds :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the AFI.
    fpgaImageId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyFpgaImageAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'modifyFpgaImageAttribute_name' - A name for the AFI.
--
-- 'operationType', 'modifyFpgaImageAttribute_operationType' - The operation type.
--
-- 'attribute', 'modifyFpgaImageAttribute_attribute' - The name of the attribute.
--
-- 'productCodes', 'modifyFpgaImageAttribute_productCodes' - The product codes. After you add a product code to an AFI, it can\'t be
-- removed. This parameter is valid only when modifying the @productCodes@
-- attribute.
--
-- 'userGroups', 'modifyFpgaImageAttribute_userGroups' - The user groups. This parameter is valid only when modifying the
-- @loadPermission@ attribute.
--
-- 'description', 'modifyFpgaImageAttribute_description' - A description for the AFI.
--
-- 'dryRun', 'modifyFpgaImageAttribute_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'loadPermission', 'modifyFpgaImageAttribute_loadPermission' - The load permission for the AFI.
--
-- 'userIds', 'modifyFpgaImageAttribute_userIds' - The Amazon Web Services account IDs. This parameter is valid only when
-- modifying the @loadPermission@ attribute.
--
-- 'fpgaImageId', 'modifyFpgaImageAttribute_fpgaImageId' - The ID of the AFI.
newModifyFpgaImageAttribute ::
  -- | 'fpgaImageId'
  Prelude.Text ->
  ModifyFpgaImageAttribute
newModifyFpgaImageAttribute pFpgaImageId_ =
  ModifyFpgaImageAttribute'
    { name = Prelude.Nothing,
      operationType = Prelude.Nothing,
      attribute = Prelude.Nothing,
      productCodes = Prelude.Nothing,
      userGroups = Prelude.Nothing,
      description = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      loadPermission = Prelude.Nothing,
      userIds = Prelude.Nothing,
      fpgaImageId = pFpgaImageId_
    }

-- | A name for the AFI.
modifyFpgaImageAttribute_name :: Lens.Lens' ModifyFpgaImageAttribute (Prelude.Maybe Prelude.Text)
modifyFpgaImageAttribute_name = Lens.lens (\ModifyFpgaImageAttribute' {name} -> name) (\s@ModifyFpgaImageAttribute' {} a -> s {name = a} :: ModifyFpgaImageAttribute)

-- | The operation type.
modifyFpgaImageAttribute_operationType :: Lens.Lens' ModifyFpgaImageAttribute (Prelude.Maybe OperationType)
modifyFpgaImageAttribute_operationType = Lens.lens (\ModifyFpgaImageAttribute' {operationType} -> operationType) (\s@ModifyFpgaImageAttribute' {} a -> s {operationType = a} :: ModifyFpgaImageAttribute)

-- | The name of the attribute.
modifyFpgaImageAttribute_attribute :: Lens.Lens' ModifyFpgaImageAttribute (Prelude.Maybe FpgaImageAttributeName)
modifyFpgaImageAttribute_attribute = Lens.lens (\ModifyFpgaImageAttribute' {attribute} -> attribute) (\s@ModifyFpgaImageAttribute' {} a -> s {attribute = a} :: ModifyFpgaImageAttribute)

-- | The product codes. After you add a product code to an AFI, it can\'t be
-- removed. This parameter is valid only when modifying the @productCodes@
-- attribute.
modifyFpgaImageAttribute_productCodes :: Lens.Lens' ModifyFpgaImageAttribute (Prelude.Maybe [Prelude.Text])
modifyFpgaImageAttribute_productCodes = Lens.lens (\ModifyFpgaImageAttribute' {productCodes} -> productCodes) (\s@ModifyFpgaImageAttribute' {} a -> s {productCodes = a} :: ModifyFpgaImageAttribute) Prelude.. Lens.mapping Lens.coerced

-- | The user groups. This parameter is valid only when modifying the
-- @loadPermission@ attribute.
modifyFpgaImageAttribute_userGroups :: Lens.Lens' ModifyFpgaImageAttribute (Prelude.Maybe [Prelude.Text])
modifyFpgaImageAttribute_userGroups = Lens.lens (\ModifyFpgaImageAttribute' {userGroups} -> userGroups) (\s@ModifyFpgaImageAttribute' {} a -> s {userGroups = a} :: ModifyFpgaImageAttribute) Prelude.. Lens.mapping Lens.coerced

-- | A description for the AFI.
modifyFpgaImageAttribute_description :: Lens.Lens' ModifyFpgaImageAttribute (Prelude.Maybe Prelude.Text)
modifyFpgaImageAttribute_description = Lens.lens (\ModifyFpgaImageAttribute' {description} -> description) (\s@ModifyFpgaImageAttribute' {} a -> s {description = a} :: ModifyFpgaImageAttribute)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyFpgaImageAttribute_dryRun :: Lens.Lens' ModifyFpgaImageAttribute (Prelude.Maybe Prelude.Bool)
modifyFpgaImageAttribute_dryRun = Lens.lens (\ModifyFpgaImageAttribute' {dryRun} -> dryRun) (\s@ModifyFpgaImageAttribute' {} a -> s {dryRun = a} :: ModifyFpgaImageAttribute)

-- | The load permission for the AFI.
modifyFpgaImageAttribute_loadPermission :: Lens.Lens' ModifyFpgaImageAttribute (Prelude.Maybe LoadPermissionModifications)
modifyFpgaImageAttribute_loadPermission = Lens.lens (\ModifyFpgaImageAttribute' {loadPermission} -> loadPermission) (\s@ModifyFpgaImageAttribute' {} a -> s {loadPermission = a} :: ModifyFpgaImageAttribute)

-- | The Amazon Web Services account IDs. This parameter is valid only when
-- modifying the @loadPermission@ attribute.
modifyFpgaImageAttribute_userIds :: Lens.Lens' ModifyFpgaImageAttribute (Prelude.Maybe [Prelude.Text])
modifyFpgaImageAttribute_userIds = Lens.lens (\ModifyFpgaImageAttribute' {userIds} -> userIds) (\s@ModifyFpgaImageAttribute' {} a -> s {userIds = a} :: ModifyFpgaImageAttribute) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the AFI.
modifyFpgaImageAttribute_fpgaImageId :: Lens.Lens' ModifyFpgaImageAttribute Prelude.Text
modifyFpgaImageAttribute_fpgaImageId = Lens.lens (\ModifyFpgaImageAttribute' {fpgaImageId} -> fpgaImageId) (\s@ModifyFpgaImageAttribute' {} a -> s {fpgaImageId = a} :: ModifyFpgaImageAttribute)

instance Core.AWSRequest ModifyFpgaImageAttribute where
  type
    AWSResponse ModifyFpgaImageAttribute =
      ModifyFpgaImageAttributeResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyFpgaImageAttributeResponse'
            Prelude.<$> (x Data..@? "fpgaImageAttribute")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyFpgaImageAttribute where
  hashWithSalt _salt ModifyFpgaImageAttribute' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` operationType
      `Prelude.hashWithSalt` attribute
      `Prelude.hashWithSalt` productCodes
      `Prelude.hashWithSalt` userGroups
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` loadPermission
      `Prelude.hashWithSalt` userIds
      `Prelude.hashWithSalt` fpgaImageId

instance Prelude.NFData ModifyFpgaImageAttribute where
  rnf ModifyFpgaImageAttribute' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf operationType
      `Prelude.seq` Prelude.rnf attribute
      `Prelude.seq` Prelude.rnf productCodes
      `Prelude.seq` Prelude.rnf userGroups
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf loadPermission
      `Prelude.seq` Prelude.rnf userIds
      `Prelude.seq` Prelude.rnf fpgaImageId

instance Data.ToHeaders ModifyFpgaImageAttribute where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyFpgaImageAttribute where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyFpgaImageAttribute where
  toQuery ModifyFpgaImageAttribute' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifyFpgaImageAttribute" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "Name" Data.=: name,
        "OperationType" Data.=: operationType,
        "Attribute" Data.=: attribute,
        Data.toQuery
          ( Data.toQueryList "ProductCode"
              Prelude.<$> productCodes
          ),
        Data.toQuery
          ( Data.toQueryList "UserGroup"
              Prelude.<$> userGroups
          ),
        "Description" Data.=: description,
        "DryRun" Data.=: dryRun,
        "LoadPermission" Data.=: loadPermission,
        Data.toQuery
          (Data.toQueryList "UserId" Prelude.<$> userIds),
        "FpgaImageId" Data.=: fpgaImageId
      ]

-- | /See:/ 'newModifyFpgaImageAttributeResponse' smart constructor.
data ModifyFpgaImageAttributeResponse = ModifyFpgaImageAttributeResponse'
  { -- | Information about the attribute.
    fpgaImageAttribute :: Prelude.Maybe FpgaImageAttribute,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ModifyFpgaImageAttributeResponse
newModifyFpgaImageAttributeResponse pHttpStatus_ =
  ModifyFpgaImageAttributeResponse'
    { fpgaImageAttribute =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the attribute.
modifyFpgaImageAttributeResponse_fpgaImageAttribute :: Lens.Lens' ModifyFpgaImageAttributeResponse (Prelude.Maybe FpgaImageAttribute)
modifyFpgaImageAttributeResponse_fpgaImageAttribute = Lens.lens (\ModifyFpgaImageAttributeResponse' {fpgaImageAttribute} -> fpgaImageAttribute) (\s@ModifyFpgaImageAttributeResponse' {} a -> s {fpgaImageAttribute = a} :: ModifyFpgaImageAttributeResponse)

-- | The response's http status code.
modifyFpgaImageAttributeResponse_httpStatus :: Lens.Lens' ModifyFpgaImageAttributeResponse Prelude.Int
modifyFpgaImageAttributeResponse_httpStatus = Lens.lens (\ModifyFpgaImageAttributeResponse' {httpStatus} -> httpStatus) (\s@ModifyFpgaImageAttributeResponse' {} a -> s {httpStatus = a} :: ModifyFpgaImageAttributeResponse)

instance
  Prelude.NFData
    ModifyFpgaImageAttributeResponse
  where
  rnf ModifyFpgaImageAttributeResponse' {..} =
    Prelude.rnf fpgaImageAttribute
      `Prelude.seq` Prelude.rnf httpStatus
