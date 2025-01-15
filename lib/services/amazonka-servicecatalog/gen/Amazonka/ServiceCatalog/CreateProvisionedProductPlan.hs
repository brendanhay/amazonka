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
-- Module      : Amazonka.ServiceCatalog.CreateProvisionedProductPlan
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a plan.
--
-- A plan includes the list of resources to be created (when provisioning a
-- new product) or modified (when updating a provisioned product) when the
-- plan is executed.
--
-- You can create one plan for each provisioned product. To create a plan
-- for an existing provisioned product, the product status must be
-- AVAILABLE or TAINTED.
--
-- To view the resource changes in the change set, use
-- DescribeProvisionedProductPlan. To create or modify the provisioned
-- product, use ExecuteProvisionedProductPlan.
module Amazonka.ServiceCatalog.CreateProvisionedProductPlan
  ( -- * Creating a Request
    CreateProvisionedProductPlan (..),
    newCreateProvisionedProductPlan,

    -- * Request Lenses
    createProvisionedProductPlan_acceptLanguage,
    createProvisionedProductPlan_notificationArns,
    createProvisionedProductPlan_pathId,
    createProvisionedProductPlan_provisioningParameters,
    createProvisionedProductPlan_tags,
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
    createProvisionedProductPlanResponse_planId,
    createProvisionedProductPlanResponse_planName,
    createProvisionedProductPlanResponse_provisionProductId,
    createProvisionedProductPlanResponse_provisionedProductName,
    createProvisionedProductPlanResponse_provisioningArtifactId,
    createProvisionedProductPlanResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newCreateProvisionedProductPlan' smart constructor.
data CreateProvisionedProductPlan = CreateProvisionedProductPlan'
  { -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | Passed to CloudFormation. The SNS topic ARNs to which to publish
    -- stack-related events.
    notificationArns :: Prelude.Maybe [Prelude.Text],
    -- | The path identifier of the product. This value is optional if the
    -- product has a default path, and required if the product has more than
    -- one path. To list the paths for a product, use ListLaunchPaths.
    pathId :: Prelude.Maybe Prelude.Text,
    -- | Parameters specified by the administrator that are required for
    -- provisioning the product.
    provisioningParameters :: Prelude.Maybe [UpdateProvisioningParameter],
    -- | One or more tags.
    --
    -- If the plan is for an existing provisioned product, the product must
    -- have a @RESOURCE_UPDATE@ constraint with
    -- @TagUpdatesOnProvisionedProduct@ set to @ALLOWED@ to allow tag updates.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the plan.
    planName :: Prelude.Text,
    -- | The plan type.
    planType :: ProvisionedProductPlanType,
    -- | The product identifier.
    productId :: Prelude.Text,
    -- | A user-friendly name for the provisioned product. This value must be
    -- unique for the Amazon Web Services account and cannot be updated after
    -- the product is provisioned.
    provisionedProductName :: Prelude.Text,
    -- | The identifier of the provisioning artifact.
    provisioningArtifactId :: Prelude.Text,
    -- | A unique identifier that you provide to ensure idempotency. If multiple
    -- requests differ only by the idempotency token, the same response is
    -- returned for each repeated request.
    idempotencyToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateProvisionedProductPlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptLanguage', 'createProvisionedProductPlan_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'notificationArns', 'createProvisionedProductPlan_notificationArns' - Passed to CloudFormation. The SNS topic ARNs to which to publish
-- stack-related events.
--
-- 'pathId', 'createProvisionedProductPlan_pathId' - The path identifier of the product. This value is optional if the
-- product has a default path, and required if the product has more than
-- one path. To list the paths for a product, use ListLaunchPaths.
--
-- 'provisioningParameters', 'createProvisionedProductPlan_provisioningParameters' - Parameters specified by the administrator that are required for
-- provisioning the product.
--
-- 'tags', 'createProvisionedProductPlan_tags' - One or more tags.
--
-- If the plan is for an existing provisioned product, the product must
-- have a @RESOURCE_UPDATE@ constraint with
-- @TagUpdatesOnProvisionedProduct@ set to @ALLOWED@ to allow tag updates.
--
-- 'planName', 'createProvisionedProductPlan_planName' - The name of the plan.
--
-- 'planType', 'createProvisionedProductPlan_planType' - The plan type.
--
-- 'productId', 'createProvisionedProductPlan_productId' - The product identifier.
--
-- 'provisionedProductName', 'createProvisionedProductPlan_provisionedProductName' - A user-friendly name for the provisioned product. This value must be
-- unique for the Amazon Web Services account and cannot be updated after
-- the product is provisioned.
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
      { acceptLanguage =
          Prelude.Nothing,
        notificationArns = Prelude.Nothing,
        pathId = Prelude.Nothing,
        provisioningParameters = Prelude.Nothing,
        tags = Prelude.Nothing,
        planName = pPlanName_,
        planType = pPlanType_,
        productId = pProductId_,
        provisionedProductName =
          pProvisionedProductName_,
        provisioningArtifactId =
          pProvisioningArtifactId_,
        idempotencyToken = pIdempotencyToken_
      }

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
createProvisionedProductPlan_acceptLanguage :: Lens.Lens' CreateProvisionedProductPlan (Prelude.Maybe Prelude.Text)
createProvisionedProductPlan_acceptLanguage = Lens.lens (\CreateProvisionedProductPlan' {acceptLanguage} -> acceptLanguage) (\s@CreateProvisionedProductPlan' {} a -> s {acceptLanguage = a} :: CreateProvisionedProductPlan)

-- | Passed to CloudFormation. The SNS topic ARNs to which to publish
-- stack-related events.
createProvisionedProductPlan_notificationArns :: Lens.Lens' CreateProvisionedProductPlan (Prelude.Maybe [Prelude.Text])
createProvisionedProductPlan_notificationArns = Lens.lens (\CreateProvisionedProductPlan' {notificationArns} -> notificationArns) (\s@CreateProvisionedProductPlan' {} a -> s {notificationArns = a} :: CreateProvisionedProductPlan) Prelude.. Lens.mapping Lens.coerced

-- | The path identifier of the product. This value is optional if the
-- product has a default path, and required if the product has more than
-- one path. To list the paths for a product, use ListLaunchPaths.
createProvisionedProductPlan_pathId :: Lens.Lens' CreateProvisionedProductPlan (Prelude.Maybe Prelude.Text)
createProvisionedProductPlan_pathId = Lens.lens (\CreateProvisionedProductPlan' {pathId} -> pathId) (\s@CreateProvisionedProductPlan' {} a -> s {pathId = a} :: CreateProvisionedProductPlan)

-- | Parameters specified by the administrator that are required for
-- provisioning the product.
createProvisionedProductPlan_provisioningParameters :: Lens.Lens' CreateProvisionedProductPlan (Prelude.Maybe [UpdateProvisioningParameter])
createProvisionedProductPlan_provisioningParameters = Lens.lens (\CreateProvisionedProductPlan' {provisioningParameters} -> provisioningParameters) (\s@CreateProvisionedProductPlan' {} a -> s {provisioningParameters = a} :: CreateProvisionedProductPlan) Prelude.. Lens.mapping Lens.coerced

-- | One or more tags.
--
-- If the plan is for an existing provisioned product, the product must
-- have a @RESOURCE_UPDATE@ constraint with
-- @TagUpdatesOnProvisionedProduct@ set to @ALLOWED@ to allow tag updates.
createProvisionedProductPlan_tags :: Lens.Lens' CreateProvisionedProductPlan (Prelude.Maybe [Tag])
createProvisionedProductPlan_tags = Lens.lens (\CreateProvisionedProductPlan' {tags} -> tags) (\s@CreateProvisionedProductPlan' {} a -> s {tags = a} :: CreateProvisionedProductPlan) Prelude.. Lens.mapping Lens.coerced

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
-- unique for the Amazon Web Services account and cannot be updated after
-- the product is provisioned.
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

instance Core.AWSRequest CreateProvisionedProductPlan where
  type
    AWSResponse CreateProvisionedProductPlan =
      CreateProvisionedProductPlanResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateProvisionedProductPlanResponse'
            Prelude.<$> (x Data..?> "PlanId")
            Prelude.<*> (x Data..?> "PlanName")
            Prelude.<*> (x Data..?> "ProvisionProductId")
            Prelude.<*> (x Data..?> "ProvisionedProductName")
            Prelude.<*> (x Data..?> "ProvisioningArtifactId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateProvisionedProductPlan
  where
  hashWithSalt _salt CreateProvisionedProductPlan' {..} =
    _salt
      `Prelude.hashWithSalt` acceptLanguage
      `Prelude.hashWithSalt` notificationArns
      `Prelude.hashWithSalt` pathId
      `Prelude.hashWithSalt` provisioningParameters
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` planName
      `Prelude.hashWithSalt` planType
      `Prelude.hashWithSalt` productId
      `Prelude.hashWithSalt` provisionedProductName
      `Prelude.hashWithSalt` provisioningArtifactId
      `Prelude.hashWithSalt` idempotencyToken

instance Prelude.NFData CreateProvisionedProductPlan where
  rnf CreateProvisionedProductPlan' {..} =
    Prelude.rnf acceptLanguage `Prelude.seq`
      Prelude.rnf notificationArns `Prelude.seq`
        Prelude.rnf pathId `Prelude.seq`
          Prelude.rnf provisioningParameters `Prelude.seq`
            Prelude.rnf tags `Prelude.seq`
              Prelude.rnf planName `Prelude.seq`
                Prelude.rnf planType `Prelude.seq`
                  Prelude.rnf productId `Prelude.seq`
                    Prelude.rnf provisionedProductName `Prelude.seq`
                      Prelude.rnf provisioningArtifactId `Prelude.seq`
                        Prelude.rnf idempotencyToken

instance Data.ToHeaders CreateProvisionedProductPlan where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.CreateProvisionedProductPlan" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateProvisionedProductPlan where
  toJSON CreateProvisionedProductPlan' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AcceptLanguage" Data..=)
              Prelude.<$> acceptLanguage,
            ("NotificationArns" Data..=)
              Prelude.<$> notificationArns,
            ("PathId" Data..=) Prelude.<$> pathId,
            ("ProvisioningParameters" Data..=)
              Prelude.<$> provisioningParameters,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("PlanName" Data..= planName),
            Prelude.Just ("PlanType" Data..= planType),
            Prelude.Just ("ProductId" Data..= productId),
            Prelude.Just
              ( "ProvisionedProductName"
                  Data..= provisionedProductName
              ),
            Prelude.Just
              ( "ProvisioningArtifactId"
                  Data..= provisioningArtifactId
              ),
            Prelude.Just
              ("IdempotencyToken" Data..= idempotencyToken)
          ]
      )

instance Data.ToPath CreateProvisionedProductPlan where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateProvisionedProductPlan where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateProvisionedProductPlanResponse' smart constructor.
data CreateProvisionedProductPlanResponse = CreateProvisionedProductPlanResponse'
  { -- | The plan identifier.
    planId :: Prelude.Maybe Prelude.Text,
    -- | The name of the plan.
    planName :: Prelude.Maybe Prelude.Text,
    -- | The product identifier.
    provisionProductId :: Prelude.Maybe Prelude.Text,
    -- | The user-friendly name of the provisioned product.
    provisionedProductName :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the provisioning artifact.
    provisioningArtifactId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateProvisionedProductPlanResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'planId', 'createProvisionedProductPlanResponse_planId' - The plan identifier.
--
-- 'planName', 'createProvisionedProductPlanResponse_planName' - The name of the plan.
--
-- 'provisionProductId', 'createProvisionedProductPlanResponse_provisionProductId' - The product identifier.
--
-- 'provisionedProductName', 'createProvisionedProductPlanResponse_provisionedProductName' - The user-friendly name of the provisioned product.
--
-- 'provisioningArtifactId', 'createProvisionedProductPlanResponse_provisioningArtifactId' - The identifier of the provisioning artifact.
--
-- 'httpStatus', 'createProvisionedProductPlanResponse_httpStatus' - The response's http status code.
newCreateProvisionedProductPlanResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateProvisionedProductPlanResponse
newCreateProvisionedProductPlanResponse pHttpStatus_ =
  CreateProvisionedProductPlanResponse'
    { planId =
        Prelude.Nothing,
      planName = Prelude.Nothing,
      provisionProductId = Prelude.Nothing,
      provisionedProductName =
        Prelude.Nothing,
      provisioningArtifactId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The plan identifier.
createProvisionedProductPlanResponse_planId :: Lens.Lens' CreateProvisionedProductPlanResponse (Prelude.Maybe Prelude.Text)
createProvisionedProductPlanResponse_planId = Lens.lens (\CreateProvisionedProductPlanResponse' {planId} -> planId) (\s@CreateProvisionedProductPlanResponse' {} a -> s {planId = a} :: CreateProvisionedProductPlanResponse)

-- | The name of the plan.
createProvisionedProductPlanResponse_planName :: Lens.Lens' CreateProvisionedProductPlanResponse (Prelude.Maybe Prelude.Text)
createProvisionedProductPlanResponse_planName = Lens.lens (\CreateProvisionedProductPlanResponse' {planName} -> planName) (\s@CreateProvisionedProductPlanResponse' {} a -> s {planName = a} :: CreateProvisionedProductPlanResponse)

-- | The product identifier.
createProvisionedProductPlanResponse_provisionProductId :: Lens.Lens' CreateProvisionedProductPlanResponse (Prelude.Maybe Prelude.Text)
createProvisionedProductPlanResponse_provisionProductId = Lens.lens (\CreateProvisionedProductPlanResponse' {provisionProductId} -> provisionProductId) (\s@CreateProvisionedProductPlanResponse' {} a -> s {provisionProductId = a} :: CreateProvisionedProductPlanResponse)

-- | The user-friendly name of the provisioned product.
createProvisionedProductPlanResponse_provisionedProductName :: Lens.Lens' CreateProvisionedProductPlanResponse (Prelude.Maybe Prelude.Text)
createProvisionedProductPlanResponse_provisionedProductName = Lens.lens (\CreateProvisionedProductPlanResponse' {provisionedProductName} -> provisionedProductName) (\s@CreateProvisionedProductPlanResponse' {} a -> s {provisionedProductName = a} :: CreateProvisionedProductPlanResponse)

-- | The identifier of the provisioning artifact.
createProvisionedProductPlanResponse_provisioningArtifactId :: Lens.Lens' CreateProvisionedProductPlanResponse (Prelude.Maybe Prelude.Text)
createProvisionedProductPlanResponse_provisioningArtifactId = Lens.lens (\CreateProvisionedProductPlanResponse' {provisioningArtifactId} -> provisioningArtifactId) (\s@CreateProvisionedProductPlanResponse' {} a -> s {provisioningArtifactId = a} :: CreateProvisionedProductPlanResponse)

-- | The response's http status code.
createProvisionedProductPlanResponse_httpStatus :: Lens.Lens' CreateProvisionedProductPlanResponse Prelude.Int
createProvisionedProductPlanResponse_httpStatus = Lens.lens (\CreateProvisionedProductPlanResponse' {httpStatus} -> httpStatus) (\s@CreateProvisionedProductPlanResponse' {} a -> s {httpStatus = a} :: CreateProvisionedProductPlanResponse)

instance
  Prelude.NFData
    CreateProvisionedProductPlanResponse
  where
  rnf CreateProvisionedProductPlanResponse' {..} =
    Prelude.rnf planId `Prelude.seq`
      Prelude.rnf planName `Prelude.seq`
        Prelude.rnf provisionProductId `Prelude.seq`
          Prelude.rnf provisionedProductName `Prelude.seq`
            Prelude.rnf provisioningArtifactId `Prelude.seq`
              Prelude.rnf httpStatus
