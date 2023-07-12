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
-- Module      : Amazonka.EC2.ModifyImageAttribute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified attribute of the specified AMI. You can specify
-- only one attribute at a time. You can use the @Attribute@ parameter to
-- specify the attribute or one of the following parameters: @Description@
-- or @LaunchPermission@.
--
-- Images with an Amazon Web Services Marketplace product code cannot be
-- made public.
--
-- To enable the SriovNetSupport enhanced networking attribute of an image,
-- enable SriovNetSupport on an instance and create an AMI from the
-- instance.
module Amazonka.EC2.ModifyImageAttribute
  ( -- * Creating a Request
    ModifyImageAttribute (..),
    newModifyImageAttribute,

    -- * Request Lenses
    modifyImageAttribute_attribute,
    modifyImageAttribute_description,
    modifyImageAttribute_dryRun,
    modifyImageAttribute_launchPermission,
    modifyImageAttribute_operationType,
    modifyImageAttribute_organizationArns,
    modifyImageAttribute_organizationalUnitArns,
    modifyImageAttribute_productCodes,
    modifyImageAttribute_userGroups,
    modifyImageAttribute_userIds,
    modifyImageAttribute_value,
    modifyImageAttribute_imageId,

    -- * Destructuring the Response
    ModifyImageAttributeResponse (..),
    newModifyImageAttributeResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for ModifyImageAttribute.
--
-- /See:/ 'newModifyImageAttribute' smart constructor.
data ModifyImageAttribute = ModifyImageAttribute'
  { -- | The name of the attribute to modify.
    --
    -- Valid values: @description@ | @launchPermission@
    attribute :: Prelude.Maybe Prelude.Text,
    -- | A new description for the AMI.
    description :: Prelude.Maybe AttributeValue,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | A new launch permission for the AMI.
    launchPermission :: Prelude.Maybe LaunchPermissionModifications,
    -- | The operation type. This parameter can be used only when the @Attribute@
    -- parameter is @launchPermission@.
    operationType :: Prelude.Maybe OperationType,
    -- | The Amazon Resource Name (ARN) of an organization. This parameter can be
    -- used only when the @Attribute@ parameter is @launchPermission@.
    organizationArns :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Name (ARN) of an organizational unit (OU). This
    -- parameter can be used only when the @Attribute@ parameter is
    -- @launchPermission@.
    organizationalUnitArns :: Prelude.Maybe [Prelude.Text],
    -- | Not supported.
    productCodes :: Prelude.Maybe [Prelude.Text],
    -- | The user groups. This parameter can be used only when the @Attribute@
    -- parameter is @launchPermission@.
    userGroups :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Web Services account IDs. This parameter can be used only
    -- when the @Attribute@ parameter is @launchPermission@.
    userIds :: Prelude.Maybe [Prelude.Text],
    -- | The value of the attribute being modified. This parameter can be used
    -- only when the @Attribute@ parameter is @description@.
    value :: Prelude.Maybe Prelude.Text,
    -- | The ID of the AMI.
    imageId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyImageAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attribute', 'modifyImageAttribute_attribute' - The name of the attribute to modify.
--
-- Valid values: @description@ | @launchPermission@
--
-- 'description', 'modifyImageAttribute_description' - A new description for the AMI.
--
-- 'dryRun', 'modifyImageAttribute_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'launchPermission', 'modifyImageAttribute_launchPermission' - A new launch permission for the AMI.
--
-- 'operationType', 'modifyImageAttribute_operationType' - The operation type. This parameter can be used only when the @Attribute@
-- parameter is @launchPermission@.
--
-- 'organizationArns', 'modifyImageAttribute_organizationArns' - The Amazon Resource Name (ARN) of an organization. This parameter can be
-- used only when the @Attribute@ parameter is @launchPermission@.
--
-- 'organizationalUnitArns', 'modifyImageAttribute_organizationalUnitArns' - The Amazon Resource Name (ARN) of an organizational unit (OU). This
-- parameter can be used only when the @Attribute@ parameter is
-- @launchPermission@.
--
-- 'productCodes', 'modifyImageAttribute_productCodes' - Not supported.
--
-- 'userGroups', 'modifyImageAttribute_userGroups' - The user groups. This parameter can be used only when the @Attribute@
-- parameter is @launchPermission@.
--
-- 'userIds', 'modifyImageAttribute_userIds' - The Amazon Web Services account IDs. This parameter can be used only
-- when the @Attribute@ parameter is @launchPermission@.
--
-- 'value', 'modifyImageAttribute_value' - The value of the attribute being modified. This parameter can be used
-- only when the @Attribute@ parameter is @description@.
--
-- 'imageId', 'modifyImageAttribute_imageId' - The ID of the AMI.
newModifyImageAttribute ::
  -- | 'imageId'
  Prelude.Text ->
  ModifyImageAttribute
newModifyImageAttribute pImageId_ =
  ModifyImageAttribute'
    { attribute = Prelude.Nothing,
      description = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      launchPermission = Prelude.Nothing,
      operationType = Prelude.Nothing,
      organizationArns = Prelude.Nothing,
      organizationalUnitArns = Prelude.Nothing,
      productCodes = Prelude.Nothing,
      userGroups = Prelude.Nothing,
      userIds = Prelude.Nothing,
      value = Prelude.Nothing,
      imageId = pImageId_
    }

-- | The name of the attribute to modify.
--
-- Valid values: @description@ | @launchPermission@
modifyImageAttribute_attribute :: Lens.Lens' ModifyImageAttribute (Prelude.Maybe Prelude.Text)
modifyImageAttribute_attribute = Lens.lens (\ModifyImageAttribute' {attribute} -> attribute) (\s@ModifyImageAttribute' {} a -> s {attribute = a} :: ModifyImageAttribute)

-- | A new description for the AMI.
modifyImageAttribute_description :: Lens.Lens' ModifyImageAttribute (Prelude.Maybe AttributeValue)
modifyImageAttribute_description = Lens.lens (\ModifyImageAttribute' {description} -> description) (\s@ModifyImageAttribute' {} a -> s {description = a} :: ModifyImageAttribute)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyImageAttribute_dryRun :: Lens.Lens' ModifyImageAttribute (Prelude.Maybe Prelude.Bool)
modifyImageAttribute_dryRun = Lens.lens (\ModifyImageAttribute' {dryRun} -> dryRun) (\s@ModifyImageAttribute' {} a -> s {dryRun = a} :: ModifyImageAttribute)

-- | A new launch permission for the AMI.
modifyImageAttribute_launchPermission :: Lens.Lens' ModifyImageAttribute (Prelude.Maybe LaunchPermissionModifications)
modifyImageAttribute_launchPermission = Lens.lens (\ModifyImageAttribute' {launchPermission} -> launchPermission) (\s@ModifyImageAttribute' {} a -> s {launchPermission = a} :: ModifyImageAttribute)

-- | The operation type. This parameter can be used only when the @Attribute@
-- parameter is @launchPermission@.
modifyImageAttribute_operationType :: Lens.Lens' ModifyImageAttribute (Prelude.Maybe OperationType)
modifyImageAttribute_operationType = Lens.lens (\ModifyImageAttribute' {operationType} -> operationType) (\s@ModifyImageAttribute' {} a -> s {operationType = a} :: ModifyImageAttribute)

-- | The Amazon Resource Name (ARN) of an organization. This parameter can be
-- used only when the @Attribute@ parameter is @launchPermission@.
modifyImageAttribute_organizationArns :: Lens.Lens' ModifyImageAttribute (Prelude.Maybe [Prelude.Text])
modifyImageAttribute_organizationArns = Lens.lens (\ModifyImageAttribute' {organizationArns} -> organizationArns) (\s@ModifyImageAttribute' {} a -> s {organizationArns = a} :: ModifyImageAttribute) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of an organizational unit (OU). This
-- parameter can be used only when the @Attribute@ parameter is
-- @launchPermission@.
modifyImageAttribute_organizationalUnitArns :: Lens.Lens' ModifyImageAttribute (Prelude.Maybe [Prelude.Text])
modifyImageAttribute_organizationalUnitArns = Lens.lens (\ModifyImageAttribute' {organizationalUnitArns} -> organizationalUnitArns) (\s@ModifyImageAttribute' {} a -> s {organizationalUnitArns = a} :: ModifyImageAttribute) Prelude.. Lens.mapping Lens.coerced

-- | Not supported.
modifyImageAttribute_productCodes :: Lens.Lens' ModifyImageAttribute (Prelude.Maybe [Prelude.Text])
modifyImageAttribute_productCodes = Lens.lens (\ModifyImageAttribute' {productCodes} -> productCodes) (\s@ModifyImageAttribute' {} a -> s {productCodes = a} :: ModifyImageAttribute) Prelude.. Lens.mapping Lens.coerced

-- | The user groups. This parameter can be used only when the @Attribute@
-- parameter is @launchPermission@.
modifyImageAttribute_userGroups :: Lens.Lens' ModifyImageAttribute (Prelude.Maybe [Prelude.Text])
modifyImageAttribute_userGroups = Lens.lens (\ModifyImageAttribute' {userGroups} -> userGroups) (\s@ModifyImageAttribute' {} a -> s {userGroups = a} :: ModifyImageAttribute) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services account IDs. This parameter can be used only
-- when the @Attribute@ parameter is @launchPermission@.
modifyImageAttribute_userIds :: Lens.Lens' ModifyImageAttribute (Prelude.Maybe [Prelude.Text])
modifyImageAttribute_userIds = Lens.lens (\ModifyImageAttribute' {userIds} -> userIds) (\s@ModifyImageAttribute' {} a -> s {userIds = a} :: ModifyImageAttribute) Prelude.. Lens.mapping Lens.coerced

-- | The value of the attribute being modified. This parameter can be used
-- only when the @Attribute@ parameter is @description@.
modifyImageAttribute_value :: Lens.Lens' ModifyImageAttribute (Prelude.Maybe Prelude.Text)
modifyImageAttribute_value = Lens.lens (\ModifyImageAttribute' {value} -> value) (\s@ModifyImageAttribute' {} a -> s {value = a} :: ModifyImageAttribute)

-- | The ID of the AMI.
modifyImageAttribute_imageId :: Lens.Lens' ModifyImageAttribute Prelude.Text
modifyImageAttribute_imageId = Lens.lens (\ModifyImageAttribute' {imageId} -> imageId) (\s@ModifyImageAttribute' {} a -> s {imageId = a} :: ModifyImageAttribute)

instance Core.AWSRequest ModifyImageAttribute where
  type
    AWSResponse ModifyImageAttribute =
      ModifyImageAttributeResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull ModifyImageAttributeResponse'

instance Prelude.Hashable ModifyImageAttribute where
  hashWithSalt _salt ModifyImageAttribute' {..} =
    _salt
      `Prelude.hashWithSalt` attribute
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` launchPermission
      `Prelude.hashWithSalt` operationType
      `Prelude.hashWithSalt` organizationArns
      `Prelude.hashWithSalt` organizationalUnitArns
      `Prelude.hashWithSalt` productCodes
      `Prelude.hashWithSalt` userGroups
      `Prelude.hashWithSalt` userIds
      `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` imageId

instance Prelude.NFData ModifyImageAttribute where
  rnf ModifyImageAttribute' {..} =
    Prelude.rnf attribute
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf launchPermission
      `Prelude.seq` Prelude.rnf operationType
      `Prelude.seq` Prelude.rnf organizationArns
      `Prelude.seq` Prelude.rnf organizationalUnitArns
      `Prelude.seq` Prelude.rnf productCodes
      `Prelude.seq` Prelude.rnf userGroups
      `Prelude.seq` Prelude.rnf userIds
      `Prelude.seq` Prelude.rnf value
      `Prelude.seq` Prelude.rnf imageId

instance Data.ToHeaders ModifyImageAttribute where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyImageAttribute where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyImageAttribute where
  toQuery ModifyImageAttribute' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifyImageAttribute" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "Attribute" Data.=: attribute,
        "Description" Data.=: description,
        "DryRun" Data.=: dryRun,
        "LaunchPermission" Data.=: launchPermission,
        "OperationType" Data.=: operationType,
        Data.toQuery
          ( Data.toQueryList "OrganizationArn"
              Prelude.<$> organizationArns
          ),
        Data.toQuery
          ( Data.toQueryList "OrganizationalUnitArn"
              Prelude.<$> organizationalUnitArns
          ),
        Data.toQuery
          ( Data.toQueryList "ProductCode"
              Prelude.<$> productCodes
          ),
        Data.toQuery
          ( Data.toQueryList "UserGroup"
              Prelude.<$> userGroups
          ),
        Data.toQuery
          (Data.toQueryList "UserId" Prelude.<$> userIds),
        "Value" Data.=: value,
        "ImageId" Data.=: imageId
      ]

-- | /See:/ 'newModifyImageAttributeResponse' smart constructor.
data ModifyImageAttributeResponse = ModifyImageAttributeResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyImageAttributeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newModifyImageAttributeResponse ::
  ModifyImageAttributeResponse
newModifyImageAttributeResponse =
  ModifyImageAttributeResponse'

instance Prelude.NFData ModifyImageAttributeResponse where
  rnf _ = ()
