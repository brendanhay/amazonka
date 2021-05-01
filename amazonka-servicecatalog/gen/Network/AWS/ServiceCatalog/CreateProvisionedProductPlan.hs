{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ServiceCatalog.CreateProvisionedProductPlan
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a plan. A plan includes the list of resources to be created
-- (when provisioning a new product) or modified (when updating a
-- provisioned product) when the plan is executed.
--
-- You can create one plan per provisioned product. To create a plan for an
-- existing provisioned product, the product status must be AVAILBLE or
-- TAINTED.
--
-- To view the resource changes in the change set, use
-- DescribeProvisionedProductPlan. To create or modify the provisioned
-- product, use ExecuteProvisionedProductPlan.
module Network.AWS.ServiceCatalog.CreateProvisionedProductPlan
  ( -- * Creating a Request
    CreateProvisionedProductPlan (..),
    newCreateProvisionedProductPlan,

    -- * Request Lenses
    createProvisionedProductPlan_notificationArns,
    createProvisionedProductPlan_tags,
    createProvisionedProductPlan_provisioningParameters,
    createProvisionedProductPlan_pathId,
    createProvisionedProductPlan_acceptLanguage,
    createProvisionedProductPlan_planName,
    createProvisionedProductPlan_planType,
    createProvisionedProductPlan_productId,
    createProvisionedProductPlan_provisionedProductName,
    createProvisionedProductPlan_provisioningArtifactId,
    createProvisionedProductPlan_idempotencyToken,

    -- * Destructuring the Response
    CreateProvisionedProductPlanResponse (..),
    newCreateProvisionedProductPlanResponse,

    -- * Response Lenses
    createProvisionedProductPlanResponse_provisionProductId,
    createProvisionedProductPlanResponse_provisionedProductName,
    createProvisionedProductPlanResponse_provisioningArtifactId,
    createProvisionedProductPlanResponse_planName,
    createProvisionedProductPlanResponse_planId,
    createProvisionedProductPlanResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newCreateProvisionedProductPlan' smart constructor.
data CreateProvisionedProductPlan = CreateProvisionedProductPlan'
  { -- | Passed to CloudFormation. The SNS topic ARNs to which to publish
    -- stack-related events.
    notificationArns :: Prelude.Maybe [Prelude.Text],
    -- | One or more tags.
    --
    -- If the plan is for an existing provisioned product, the product must
    -- have a @RESOURCE_UPDATE@ constraint with
    -- @TagUpdatesOnProvisionedProduct@ set to @ALLOWED@ to allow tag updates.
    tags :: Prelude.Maybe [Tag],
    -- | Parameters specified by the administrator that are required for
    -- provisioning the product.
    provisioningParameters :: Prelude.Maybe [UpdateProvisioningParameter],
    -- | The path identifier of the product. This value is optional if the
    -- product has a default path, and required if the product has more than
    -- one path. To list the paths for a product, use ListLaunchPaths.
    pathId :: Prelude.Maybe Prelude.Text,
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The name of the plan.
    planName :: Prelude.Text,
    -- | The plan type.
    planType :: ProvisionedProductPlanType,
    -- | The product identifier.
    productId :: Prelude.Text,
    -- | A user-friendly name for the provisioned product. This value must be
    -- unique for the AWS account and cannot be updated after the product is
    -- provisioned.
    provisionedProductName :: Prelude.Text,
    -- | The identifier of the provisioning artifact.
    provisioningArtifactId :: Prelude.Text,
    -- | A unique identifier that you provide to ensure idempotency. If multiple
    -- requests differ only by the idempotency token, the same response is
    -- returned for each repeated request.
    idempotencyToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateProvisionedProductPlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notificationArns', 'createProvisionedProductPlan_notificationArns' - Passed to CloudFormation. The SNS topic ARNs to which to publish
-- stack-related events.
--
-- 'tags', 'createProvisionedProductPlan_tags' - One or more tags.
--
-- If the plan is for an existing provisioned product, the product must
-- have a @RESOURCE_UPDATE@ constraint with
-- @TagUpdatesOnProvisionedProduct@ set to @ALLOWED@ to allow tag updates.
--
-- 'provisioningParameters', 'createProvisionedProductPlan_provisioningParameters' - Parameters specified by the administrator that are required for
-- provisioning the product.
--
-- 'pathId', 'createProvisionedProductPlan_pathId' - The path identifier of the product. This value is optional if the
-- product has a default path, and required if the product has more than
-- one path. To list the paths for a product, use ListLaunchPaths.
--
-- 'acceptLanguage', 'createProvisionedProductPlan_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'planName', 'createProvisionedProductPlan_planName' - The name of the plan.
--
-- 'planType', 'createProvisionedProductPlan_planType' - The plan type.
--
-- 'productId', 'createProvisionedProductPlan_productId' - The product identifier.
--
-- 'provisionedProductName', 'createProvisionedProductPlan_provisionedProductName' - A user-friendly name for the provisioned product. This value must be
-- unique for the AWS account and cannot be updated after the product is
-- provisioned.
--
-- 'provisioningArtifactId', 'createProvisionedProductPlan_provisioningArtifactId' - The identifier of the provisioning artifact.
--
-- 'idempotencyToken', 'createProvisionedProductPlan_idempotencyToken' - A unique identifier that you provide to ensure idempotency. If multiple
-- requests differ only by the idempotency token, the same response is
-- returned for each repeated request.
newCreateProvisionedProductPlan ::
  -- | 'planName'
  Prelude.Text ->
  -- | 'planType'
  ProvisionedProductPlanType ->
  -- | 'productId'
  Prelude.Text ->
  -- | 'provisionedProductName'
  Prelude.Text ->
  -- | 'provisioningArtifactId'
  Prelude.Text ->
  -- | 'idempotencyToken'
  Prelude.Text ->
  CreateProvisionedProductPlan
newCreateProvisionedProductPlan
  pPlanName_
  pPlanType_
  pProductId_
  pProvisionedProductName_
  pProvisioningArtifactId_
  pIdempotencyToken_ =
    CreateProvisionedProductPlan'
      { notificationArns =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        provisioningParameters = Prelude.Nothing,
        pathId = Prelude.Nothing,
        acceptLanguage = Prelude.Nothing,
        planName = pPlanName_,
        planType = pPlanType_,
        productId = pProductId_,
        provisionedProductName =
          pProvisionedProductName_,
        provisioningArtifactId =
          pProvisioningArtifactId_,
        idempotencyToken = pIdempotencyToken_
      }

-- | Passed to CloudFormation. The SNS topic ARNs to which to publish
-- stack-related events.
createProvisionedProductPlan_notificationArns :: Lens.Lens' CreateProvisionedProductPlan (Prelude.Maybe [Prelude.Text])
createProvisionedProductPlan_notificationArns = Lens.lens (\CreateProvisionedProductPlan' {notificationArns} -> notificationArns) (\s@CreateProvisionedProductPlan' {} a -> s {notificationArns = a} :: CreateProvisionedProductPlan) Prelude.. Lens.mapping Prelude._Coerce

-- | One or more tags.
--
-- If the plan is for an existing provisioned product, the product must
-- have a @RESOURCE_UPDATE@ constraint with
-- @TagUpdatesOnProvisionedProduct@ set to @ALLOWED@ to allow tag updates.
createProvisionedProductPlan_tags :: Lens.Lens' CreateProvisionedProductPlan (Prelude.Maybe [Tag])
createProvisionedProductPlan_tags = Lens.lens (\CreateProvisionedProductPlan' {tags} -> tags) (\s@CreateProvisionedProductPlan' {} a -> s {tags = a} :: CreateProvisionedProductPlan) Prelude.. Lens.mapping Prelude._Coerce

-- | Parameters specified by the administrator that are required for
-- provisioning the product.
createProvisionedProductPlan_provisioningParameters :: Lens.Lens' CreateProvisionedProductPlan (Prelude.Maybe [UpdateProvisioningParameter])
createProvisionedProductPlan_provisioningParameters = Lens.lens (\CreateProvisionedProductPlan' {provisioningParameters} -> provisioningParameters) (\s@CreateProvisionedProductPlan' {} a -> s {provisioningParameters = a} :: CreateProvisionedProductPlan) Prelude.. Lens.mapping Prelude._Coerce

-- | The path identifier of the product. This value is optional if the
-- product has a default path, and required if the product has more than
-- one path. To list the paths for a product, use ListLaunchPaths.
createProvisionedProductPlan_pathId :: Lens.Lens' CreateProvisionedProductPlan (Prelude.Maybe Prelude.Text)
createProvisionedProductPlan_pathId = Lens.lens (\CreateProvisionedProductPlan' {pathId} -> pathId) (\s@CreateProvisionedProductPlan' {} a -> s {pathId = a} :: CreateProvisionedProductPlan)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
createProvisionedProductPlan_acceptLanguage :: Lens.Lens' CreateProvisionedProductPlan (Prelude.Maybe Prelude.Text)
createProvisionedProductPlan_acceptLanguage = Lens.lens (\CreateProvisionedProductPlan' {acceptLanguage} -> acceptLanguage) (\s@CreateProvisionedProductPlan' {} a -> s {acceptLanguage = a} :: CreateProvisionedProductPlan)

-- | The name of the plan.
createProvisionedProductPlan_planName :: Lens.Lens' CreateProvisionedProductPlan Prelude.Text
createProvisionedProductPlan_planName = Lens.lens (\CreateProvisionedProductPlan' {planName} -> planName) (\s@CreateProvisionedProductPlan' {} a -> s {planName = a} :: CreateProvisionedProductPlan)

-- | The plan type.
createProvisionedProductPlan_planType :: Lens.Lens' CreateProvisionedProductPlan ProvisionedProductPlanType
createProvisionedProductPlan_planType = Lens.lens (\CreateProvisionedProductPlan' {planType} -> planType) (\s@CreateProvisionedProductPlan' {} a -> s {planType = a} :: CreateProvisionedProductPlan)

-- | The product identifier.
createProvisionedProductPlan_productId :: Lens.Lens' CreateProvisionedProductPlan Prelude.Text
createProvisionedProductPlan_productId = Lens.lens (\CreateProvisionedProductPlan' {productId} -> productId) (\s@CreateProvisionedProductPlan' {} a -> s {productId = a} :: CreateProvisionedProductPlan)

-- | A user-friendly name for the provisioned product. This value must be
-- unique for the AWS account and cannot be updated after the product is
-- provisioned.
createProvisionedProductPlan_provisionedProductName :: Lens.Lens' CreateProvisionedProductPlan Prelude.Text
createProvisionedProductPlan_provisionedProductName = Lens.lens (\CreateProvisionedProductPlan' {provisionedProductName} -> provisionedProductName) (\s@CreateProvisionedProductPlan' {} a -> s {provisionedProductName = a} :: CreateProvisionedProductPlan)

-- | The identifier of the provisioning artifact.
createProvisionedProductPlan_provisioningArtifactId :: Lens.Lens' CreateProvisionedProductPlan Prelude.Text
createProvisionedProductPlan_provisioningArtifactId = Lens.lens (\CreateProvisionedProductPlan' {provisioningArtifactId} -> provisioningArtifactId) (\s@CreateProvisionedProductPlan' {} a -> s {provisioningArtifactId = a} :: CreateProvisionedProductPlan)

-- | A unique identifier that you provide to ensure idempotency. If multiple
-- requests differ only by the idempotency token, the same response is
-- returned for each repeated request.
createProvisionedProductPlan_idempotencyToken :: Lens.Lens' CreateProvisionedProductPlan Prelude.Text
createProvisionedProductPlan_idempotencyToken = Lens.lens (\CreateProvisionedProductPlan' {idempotencyToken} -> idempotencyToken) (\s@CreateProvisionedProductPlan' {} a -> s {idempotencyToken = a} :: CreateProvisionedProductPlan)

instance
  Prelude.AWSRequest
    CreateProvisionedProductPlan
  where
  type
    Rs CreateProvisionedProductPlan =
      CreateProvisionedProductPlanResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateProvisionedProductPlanResponse'
            Prelude.<$> (x Prelude..?> "ProvisionProductId")
            Prelude.<*> (x Prelude..?> "ProvisionedProductName")
            Prelude.<*> (x Prelude..?> "ProvisioningArtifactId")
            Prelude.<*> (x Prelude..?> "PlanName")
            Prelude.<*> (x Prelude..?> "PlanId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateProvisionedProductPlan

instance Prelude.NFData CreateProvisionedProductPlan

instance
  Prelude.ToHeaders
    CreateProvisionedProductPlan
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWS242ServiceCatalogService.CreateProvisionedProductPlan" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateProvisionedProductPlan where
  toJSON CreateProvisionedProductPlan' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NotificationArns" Prelude..=)
              Prelude.<$> notificationArns,
            ("Tags" Prelude..=) Prelude.<$> tags,
            ("ProvisioningParameters" Prelude..=)
              Prelude.<$> provisioningParameters,
            ("PathId" Prelude..=) Prelude.<$> pathId,
            ("AcceptLanguage" Prelude..=)
              Prelude.<$> acceptLanguage,
            Prelude.Just ("PlanName" Prelude..= planName),
            Prelude.Just ("PlanType" Prelude..= planType),
            Prelude.Just ("ProductId" Prelude..= productId),
            Prelude.Just
              ( "ProvisionedProductName"
                  Prelude..= provisionedProductName
              ),
            Prelude.Just
              ( "ProvisioningArtifactId"
                  Prelude..= provisioningArtifactId
              ),
            Prelude.Just
              ("IdempotencyToken" Prelude..= idempotencyToken)
          ]
      )

instance Prelude.ToPath CreateProvisionedProductPlan where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateProvisionedProductPlan where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateProvisionedProductPlanResponse' smart constructor.
data CreateProvisionedProductPlanResponse = CreateProvisionedProductPlanResponse'
  { -- | The product identifier.
    provisionProductId :: Prelude.Maybe Prelude.Text,
    -- | The user-friendly name of the provisioned product.
    provisionedProductName :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the provisioning artifact.
    provisioningArtifactId :: Prelude.Maybe Prelude.Text,
    -- | The name of the plan.
    planName :: Prelude.Maybe Prelude.Text,
    -- | The plan identifier.
    planId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateProvisionedProductPlanResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'provisionProductId', 'createProvisionedProductPlanResponse_provisionProductId' - The product identifier.
--
-- 'provisionedProductName', 'createProvisionedProductPlanResponse_provisionedProductName' - The user-friendly name of the provisioned product.
--
-- 'provisioningArtifactId', 'createProvisionedProductPlanResponse_provisioningArtifactId' - The identifier of the provisioning artifact.
--
-- 'planName', 'createProvisionedProductPlanResponse_planName' - The name of the plan.
--
-- 'planId', 'createProvisionedProductPlanResponse_planId' - The plan identifier.
--
-- 'httpStatus', 'createProvisionedProductPlanResponse_httpStatus' - The response's http status code.
newCreateProvisionedProductPlanResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateProvisionedProductPlanResponse
newCreateProvisionedProductPlanResponse pHttpStatus_ =
  CreateProvisionedProductPlanResponse'
    { provisionProductId =
        Prelude.Nothing,
      provisionedProductName =
        Prelude.Nothing,
      provisioningArtifactId =
        Prelude.Nothing,
      planName = Prelude.Nothing,
      planId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The product identifier.
createProvisionedProductPlanResponse_provisionProductId :: Lens.Lens' CreateProvisionedProductPlanResponse (Prelude.Maybe Prelude.Text)
createProvisionedProductPlanResponse_provisionProductId = Lens.lens (\CreateProvisionedProductPlanResponse' {provisionProductId} -> provisionProductId) (\s@CreateProvisionedProductPlanResponse' {} a -> s {provisionProductId = a} :: CreateProvisionedProductPlanResponse)

-- | The user-friendly name of the provisioned product.
createProvisionedProductPlanResponse_provisionedProductName :: Lens.Lens' CreateProvisionedProductPlanResponse (Prelude.Maybe Prelude.Text)
createProvisionedProductPlanResponse_provisionedProductName = Lens.lens (\CreateProvisionedProductPlanResponse' {provisionedProductName} -> provisionedProductName) (\s@CreateProvisionedProductPlanResponse' {} a -> s {provisionedProductName = a} :: CreateProvisionedProductPlanResponse)

-- | The identifier of the provisioning artifact.
createProvisionedProductPlanResponse_provisioningArtifactId :: Lens.Lens' CreateProvisionedProductPlanResponse (Prelude.Maybe Prelude.Text)
createProvisionedProductPlanResponse_provisioningArtifactId = Lens.lens (\CreateProvisionedProductPlanResponse' {provisioningArtifactId} -> provisioningArtifactId) (\s@CreateProvisionedProductPlanResponse' {} a -> s {provisioningArtifactId = a} :: CreateProvisionedProductPlanResponse)

-- | The name of the plan.
createProvisionedProductPlanResponse_planName :: Lens.Lens' CreateProvisionedProductPlanResponse (Prelude.Maybe Prelude.Text)
createProvisionedProductPlanResponse_planName = Lens.lens (\CreateProvisionedProductPlanResponse' {planName} -> planName) (\s@CreateProvisionedProductPlanResponse' {} a -> s {planName = a} :: CreateProvisionedProductPlanResponse)

-- | The plan identifier.
createProvisionedProductPlanResponse_planId :: Lens.Lens' CreateProvisionedProductPlanResponse (Prelude.Maybe Prelude.Text)
createProvisionedProductPlanResponse_planId = Lens.lens (\CreateProvisionedProductPlanResponse' {planId} -> planId) (\s@CreateProvisionedProductPlanResponse' {} a -> s {planId = a} :: CreateProvisionedProductPlanResponse)

-- | The response's http status code.
createProvisionedProductPlanResponse_httpStatus :: Lens.Lens' CreateProvisionedProductPlanResponse Prelude.Int
createProvisionedProductPlanResponse_httpStatus = Lens.lens (\CreateProvisionedProductPlanResponse' {httpStatus} -> httpStatus) (\s@CreateProvisionedProductPlanResponse' {} a -> s {httpStatus = a} :: CreateProvisionedProductPlanResponse)

instance
  Prelude.NFData
    CreateProvisionedProductPlanResponse
