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
-- Module      : Network.AWS.ServiceCatalog.CreateConstraint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a constraint.
--
-- A delegated admin is authorized to invoke this command.
module Network.AWS.ServiceCatalog.CreateConstraint
  ( -- * Creating a Request
    CreateConstraint (..),
    newCreateConstraint,

    -- * Request Lenses
    createConstraint_description,
    createConstraint_acceptLanguage,
    createConstraint_portfolioId,
    createConstraint_productId,
    createConstraint_parameters,
    createConstraint_type,
    createConstraint_idempotencyToken,

    -- * Destructuring the Response
    CreateConstraintResponse (..),
    newCreateConstraintResponse,

    -- * Response Lenses
    createConstraintResponse_constraintParameters,
    createConstraintResponse_status,
    createConstraintResponse_constraintDetail,
    createConstraintResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newCreateConstraint' smart constructor.
data CreateConstraint = CreateConstraint'
  { -- | The description of the constraint.
    description :: Core.Maybe Core.Text,
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Core.Maybe Core.Text,
    -- | The portfolio identifier.
    portfolioId :: Core.Text,
    -- | The product identifier.
    productId :: Core.Text,
    -- | The constraint parameters, in JSON format. The syntax depends on the
    -- constraint type as follows:
    --
    -- [LAUNCH]
    --     You are required to specify either the @RoleArn@ or the
    --     @LocalRoleName@ but can\'t use both.
    --
    --     Specify the @RoleArn@ property as follows:
    --
    --     @{\"RoleArn\" : \"arn:aws:iam::123456789012:role\/LaunchRole\"}@
    --
    --     Specify the @LocalRoleName@ property as follows:
    --
    --     @{\"LocalRoleName\": \"SCBasicLaunchRole\"}@
    --
    --     If you specify the @LocalRoleName@ property, when an account uses
    --     the launch constraint, the IAM role with that name in the account
    --     will be used. This allows launch-role constraints to be
    --     account-agnostic so the administrator can create fewer resources per
    --     shared account.
    --
    --     The given role name must exist in the account used to create the
    --     launch constraint and the account of the user who launches a product
    --     with this launch constraint.
    --
    --     You cannot have both a @LAUNCH@ and a @STACKSET@ constraint.
    --
    --     You also cannot have more than one @LAUNCH@ constraint on a product
    --     and portfolio.
    --
    -- [NOTIFICATION]
    --     Specify the @NotificationArns@ property as follows:
    --
    --     @{\"NotificationArns\" : [\"arn:aws:sns:us-east-1:123456789012:Topic\"]}@
    --
    -- [RESOURCE_UPDATE]
    --     Specify the @TagUpdatesOnProvisionedProduct@ property as follows:
    --
    --     @{\"Version\":\"2.0\",\"Properties\":{\"TagUpdateOnProvisionedProduct\":\"String\"}}@
    --
    --     The @TagUpdatesOnProvisionedProduct@ property accepts a string value
    --     of @ALLOWED@ or @NOT_ALLOWED@.
    --
    -- [STACKSET]
    --     Specify the @Parameters@ property as follows:
    --
    --     @{\"Version\": \"String\", \"Properties\": {\"AccountList\": [ \"String\" ], \"RegionList\": [ \"String\" ], \"AdminRole\": \"String\", \"ExecutionRole\": \"String\"}}@
    --
    --     You cannot have both a @LAUNCH@ and a @STACKSET@ constraint.
    --
    --     You also cannot have more than one @STACKSET@ constraint on a
    --     product and portfolio.
    --
    --     Products with a @STACKSET@ constraint will launch an AWS
    --     CloudFormation stack set.
    --
    -- [TEMPLATE]
    --     Specify the @Rules@ property. For more information, see
    --     <http://docs.aws.amazon.com/servicecatalog/latest/adminguide/reference-template_constraint_rules.html Template Constraint Rules>.
    parameters :: Core.Text,
    -- | The type of constraint.
    --
    -- -   @LAUNCH@
    --
    -- -   @NOTIFICATION@
    --
    -- -   @RESOURCE_UPDATE@
    --
    -- -   @STACKSET@
    --
    -- -   @TEMPLATE@
    type' :: Core.Text,
    -- | A unique identifier that you provide to ensure idempotency. If multiple
    -- requests differ only by the idempotency token, the same response is
    -- returned for each repeated request.
    idempotencyToken :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateConstraint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createConstraint_description' - The description of the constraint.
--
-- 'acceptLanguage', 'createConstraint_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'portfolioId', 'createConstraint_portfolioId' - The portfolio identifier.
--
-- 'productId', 'createConstraint_productId' - The product identifier.
--
-- 'parameters', 'createConstraint_parameters' - The constraint parameters, in JSON format. The syntax depends on the
-- constraint type as follows:
--
-- [LAUNCH]
--     You are required to specify either the @RoleArn@ or the
--     @LocalRoleName@ but can\'t use both.
--
--     Specify the @RoleArn@ property as follows:
--
--     @{\"RoleArn\" : \"arn:aws:iam::123456789012:role\/LaunchRole\"}@
--
--     Specify the @LocalRoleName@ property as follows:
--
--     @{\"LocalRoleName\": \"SCBasicLaunchRole\"}@
--
--     If you specify the @LocalRoleName@ property, when an account uses
--     the launch constraint, the IAM role with that name in the account
--     will be used. This allows launch-role constraints to be
--     account-agnostic so the administrator can create fewer resources per
--     shared account.
--
--     The given role name must exist in the account used to create the
--     launch constraint and the account of the user who launches a product
--     with this launch constraint.
--
--     You cannot have both a @LAUNCH@ and a @STACKSET@ constraint.
--
--     You also cannot have more than one @LAUNCH@ constraint on a product
--     and portfolio.
--
-- [NOTIFICATION]
--     Specify the @NotificationArns@ property as follows:
--
--     @{\"NotificationArns\" : [\"arn:aws:sns:us-east-1:123456789012:Topic\"]}@
--
-- [RESOURCE_UPDATE]
--     Specify the @TagUpdatesOnProvisionedProduct@ property as follows:
--
--     @{\"Version\":\"2.0\",\"Properties\":{\"TagUpdateOnProvisionedProduct\":\"String\"}}@
--
--     The @TagUpdatesOnProvisionedProduct@ property accepts a string value
--     of @ALLOWED@ or @NOT_ALLOWED@.
--
-- [STACKSET]
--     Specify the @Parameters@ property as follows:
--
--     @{\"Version\": \"String\", \"Properties\": {\"AccountList\": [ \"String\" ], \"RegionList\": [ \"String\" ], \"AdminRole\": \"String\", \"ExecutionRole\": \"String\"}}@
--
--     You cannot have both a @LAUNCH@ and a @STACKSET@ constraint.
--
--     You also cannot have more than one @STACKSET@ constraint on a
--     product and portfolio.
--
--     Products with a @STACKSET@ constraint will launch an AWS
--     CloudFormation stack set.
--
-- [TEMPLATE]
--     Specify the @Rules@ property. For more information, see
--     <http://docs.aws.amazon.com/servicecatalog/latest/adminguide/reference-template_constraint_rules.html Template Constraint Rules>.
--
-- 'type'', 'createConstraint_type' - The type of constraint.
--
-- -   @LAUNCH@
--
-- -   @NOTIFICATION@
--
-- -   @RESOURCE_UPDATE@
--
-- -   @STACKSET@
--
-- -   @TEMPLATE@
--
-- 'idempotencyToken', 'createConstraint_idempotencyToken' - A unique identifier that you provide to ensure idempotency. If multiple
-- requests differ only by the idempotency token, the same response is
-- returned for each repeated request.
newCreateConstraint ::
  -- | 'portfolioId'
  Core.Text ->
  -- | 'productId'
  Core.Text ->
  -- | 'parameters'
  Core.Text ->
  -- | 'type''
  Core.Text ->
  -- | 'idempotencyToken'
  Core.Text ->
  CreateConstraint
newCreateConstraint
  pPortfolioId_
  pProductId_
  pParameters_
  pType_
  pIdempotencyToken_ =
    CreateConstraint'
      { description = Core.Nothing,
        acceptLanguage = Core.Nothing,
        portfolioId = pPortfolioId_,
        productId = pProductId_,
        parameters = pParameters_,
        type' = pType_,
        idempotencyToken = pIdempotencyToken_
      }

-- | The description of the constraint.
createConstraint_description :: Lens.Lens' CreateConstraint (Core.Maybe Core.Text)
createConstraint_description = Lens.lens (\CreateConstraint' {description} -> description) (\s@CreateConstraint' {} a -> s {description = a} :: CreateConstraint)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
createConstraint_acceptLanguage :: Lens.Lens' CreateConstraint (Core.Maybe Core.Text)
createConstraint_acceptLanguage = Lens.lens (\CreateConstraint' {acceptLanguage} -> acceptLanguage) (\s@CreateConstraint' {} a -> s {acceptLanguage = a} :: CreateConstraint)

-- | The portfolio identifier.
createConstraint_portfolioId :: Lens.Lens' CreateConstraint Core.Text
createConstraint_portfolioId = Lens.lens (\CreateConstraint' {portfolioId} -> portfolioId) (\s@CreateConstraint' {} a -> s {portfolioId = a} :: CreateConstraint)

-- | The product identifier.
createConstraint_productId :: Lens.Lens' CreateConstraint Core.Text
createConstraint_productId = Lens.lens (\CreateConstraint' {productId} -> productId) (\s@CreateConstraint' {} a -> s {productId = a} :: CreateConstraint)

-- | The constraint parameters, in JSON format. The syntax depends on the
-- constraint type as follows:
--
-- [LAUNCH]
--     You are required to specify either the @RoleArn@ or the
--     @LocalRoleName@ but can\'t use both.
--
--     Specify the @RoleArn@ property as follows:
--
--     @{\"RoleArn\" : \"arn:aws:iam::123456789012:role\/LaunchRole\"}@
--
--     Specify the @LocalRoleName@ property as follows:
--
--     @{\"LocalRoleName\": \"SCBasicLaunchRole\"}@
--
--     If you specify the @LocalRoleName@ property, when an account uses
--     the launch constraint, the IAM role with that name in the account
--     will be used. This allows launch-role constraints to be
--     account-agnostic so the administrator can create fewer resources per
--     shared account.
--
--     The given role name must exist in the account used to create the
--     launch constraint and the account of the user who launches a product
--     with this launch constraint.
--
--     You cannot have both a @LAUNCH@ and a @STACKSET@ constraint.
--
--     You also cannot have more than one @LAUNCH@ constraint on a product
--     and portfolio.
--
-- [NOTIFICATION]
--     Specify the @NotificationArns@ property as follows:
--
--     @{\"NotificationArns\" : [\"arn:aws:sns:us-east-1:123456789012:Topic\"]}@
--
-- [RESOURCE_UPDATE]
--     Specify the @TagUpdatesOnProvisionedProduct@ property as follows:
--
--     @{\"Version\":\"2.0\",\"Properties\":{\"TagUpdateOnProvisionedProduct\":\"String\"}}@
--
--     The @TagUpdatesOnProvisionedProduct@ property accepts a string value
--     of @ALLOWED@ or @NOT_ALLOWED@.
--
-- [STACKSET]
--     Specify the @Parameters@ property as follows:
--
--     @{\"Version\": \"String\", \"Properties\": {\"AccountList\": [ \"String\" ], \"RegionList\": [ \"String\" ], \"AdminRole\": \"String\", \"ExecutionRole\": \"String\"}}@
--
--     You cannot have both a @LAUNCH@ and a @STACKSET@ constraint.
--
--     You also cannot have more than one @STACKSET@ constraint on a
--     product and portfolio.
--
--     Products with a @STACKSET@ constraint will launch an AWS
--     CloudFormation stack set.
--
-- [TEMPLATE]
--     Specify the @Rules@ property. For more information, see
--     <http://docs.aws.amazon.com/servicecatalog/latest/adminguide/reference-template_constraint_rules.html Template Constraint Rules>.
createConstraint_parameters :: Lens.Lens' CreateConstraint Core.Text
createConstraint_parameters = Lens.lens (\CreateConstraint' {parameters} -> parameters) (\s@CreateConstraint' {} a -> s {parameters = a} :: CreateConstraint)

-- | The type of constraint.
--
-- -   @LAUNCH@
--
-- -   @NOTIFICATION@
--
-- -   @RESOURCE_UPDATE@
--
-- -   @STACKSET@
--
-- -   @TEMPLATE@
createConstraint_type :: Lens.Lens' CreateConstraint Core.Text
createConstraint_type = Lens.lens (\CreateConstraint' {type'} -> type') (\s@CreateConstraint' {} a -> s {type' = a} :: CreateConstraint)

-- | A unique identifier that you provide to ensure idempotency. If multiple
-- requests differ only by the idempotency token, the same response is
-- returned for each repeated request.
createConstraint_idempotencyToken :: Lens.Lens' CreateConstraint Core.Text
createConstraint_idempotencyToken = Lens.lens (\CreateConstraint' {idempotencyToken} -> idempotencyToken) (\s@CreateConstraint' {} a -> s {idempotencyToken = a} :: CreateConstraint)

instance Core.AWSRequest CreateConstraint where
  type
    AWSResponse CreateConstraint =
      CreateConstraintResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateConstraintResponse'
            Core.<$> (x Core..?> "ConstraintParameters")
            Core.<*> (x Core..?> "Status")
            Core.<*> (x Core..?> "ConstraintDetail")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateConstraint

instance Core.NFData CreateConstraint

instance Core.ToHeaders CreateConstraint where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.CreateConstraint" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateConstraint where
  toJSON CreateConstraint' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Description" Core..=) Core.<$> description,
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            Core.Just ("PortfolioId" Core..= portfolioId),
            Core.Just ("ProductId" Core..= productId),
            Core.Just ("Parameters" Core..= parameters),
            Core.Just ("Type" Core..= type'),
            Core.Just
              ("IdempotencyToken" Core..= idempotencyToken)
          ]
      )

instance Core.ToPath CreateConstraint where
  toPath = Core.const "/"

instance Core.ToQuery CreateConstraint where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateConstraintResponse' smart constructor.
data CreateConstraintResponse = CreateConstraintResponse'
  { -- | The constraint parameters.
    constraintParameters :: Core.Maybe Core.Text,
    -- | The status of the current request.
    status :: Core.Maybe RequestStatus,
    -- | Information about the constraint.
    constraintDetail :: Core.Maybe ConstraintDetail,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateConstraintResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'constraintParameters', 'createConstraintResponse_constraintParameters' - The constraint parameters.
--
-- 'status', 'createConstraintResponse_status' - The status of the current request.
--
-- 'constraintDetail', 'createConstraintResponse_constraintDetail' - Information about the constraint.
--
-- 'httpStatus', 'createConstraintResponse_httpStatus' - The response's http status code.
newCreateConstraintResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateConstraintResponse
newCreateConstraintResponse pHttpStatus_ =
  CreateConstraintResponse'
    { constraintParameters =
        Core.Nothing,
      status = Core.Nothing,
      constraintDetail = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The constraint parameters.
createConstraintResponse_constraintParameters :: Lens.Lens' CreateConstraintResponse (Core.Maybe Core.Text)
createConstraintResponse_constraintParameters = Lens.lens (\CreateConstraintResponse' {constraintParameters} -> constraintParameters) (\s@CreateConstraintResponse' {} a -> s {constraintParameters = a} :: CreateConstraintResponse)

-- | The status of the current request.
createConstraintResponse_status :: Lens.Lens' CreateConstraintResponse (Core.Maybe RequestStatus)
createConstraintResponse_status = Lens.lens (\CreateConstraintResponse' {status} -> status) (\s@CreateConstraintResponse' {} a -> s {status = a} :: CreateConstraintResponse)

-- | Information about the constraint.
createConstraintResponse_constraintDetail :: Lens.Lens' CreateConstraintResponse (Core.Maybe ConstraintDetail)
createConstraintResponse_constraintDetail = Lens.lens (\CreateConstraintResponse' {constraintDetail} -> constraintDetail) (\s@CreateConstraintResponse' {} a -> s {constraintDetail = a} :: CreateConstraintResponse)

-- | The response's http status code.
createConstraintResponse_httpStatus :: Lens.Lens' CreateConstraintResponse Core.Int
createConstraintResponse_httpStatus = Lens.lens (\CreateConstraintResponse' {httpStatus} -> httpStatus) (\s@CreateConstraintResponse' {} a -> s {httpStatus = a} :: CreateConstraintResponse)

instance Core.NFData CreateConstraintResponse
