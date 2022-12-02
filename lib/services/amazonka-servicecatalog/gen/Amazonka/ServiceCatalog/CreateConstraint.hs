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
-- Module      : Amazonka.ServiceCatalog.CreateConstraint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a constraint.
--
-- A delegated admin is authorized to invoke this command.
module Amazonka.ServiceCatalog.CreateConstraint
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
    createConstraintResponse_constraintDetail,
    createConstraintResponse_status,
    createConstraintResponse_constraintParameters,
    createConstraintResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newCreateConstraint' smart constructor.
data CreateConstraint = CreateConstraint'
  { -- | The description of the constraint.
    description :: Prelude.Maybe Prelude.Text,
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The portfolio identifier.
    portfolioId :: Prelude.Text,
    -- | The product identifier.
    productId :: Prelude.Text,
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
    --     Products with a @STACKSET@ constraint will launch an CloudFormation
    --     stack set.
    --
    -- [TEMPLATE]
    --     Specify the @Rules@ property. For more information, see
    --     <http://docs.aws.amazon.com/servicecatalog/latest/adminguide/reference-template_constraint_rules.html Template Constraint Rules>.
    parameters :: Prelude.Text,
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
    type' :: Prelude.Text,
    -- | A unique identifier that you provide to ensure idempotency. If multiple
    -- requests differ only by the idempotency token, the same response is
    -- returned for each repeated request.
    idempotencyToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
--     Products with a @STACKSET@ constraint will launch an CloudFormation
--     stack set.
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
  Prelude.Text ->
  -- | 'productId'
  Prelude.Text ->
  -- | 'parameters'
  Prelude.Text ->
  -- | 'type''
  Prelude.Text ->
  -- | 'idempotencyToken'
  Prelude.Text ->
  CreateConstraint
newCreateConstraint
  pPortfolioId_
  pProductId_
  pParameters_
  pType_
  pIdempotencyToken_ =
    CreateConstraint'
      { description = Prelude.Nothing,
        acceptLanguage = Prelude.Nothing,
        portfolioId = pPortfolioId_,
        productId = pProductId_,
        parameters = pParameters_,
        type' = pType_,
        idempotencyToken = pIdempotencyToken_
      }

-- | The description of the constraint.
createConstraint_description :: Lens.Lens' CreateConstraint (Prelude.Maybe Prelude.Text)
createConstraint_description = Lens.lens (\CreateConstraint' {description} -> description) (\s@CreateConstraint' {} a -> s {description = a} :: CreateConstraint)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
createConstraint_acceptLanguage :: Lens.Lens' CreateConstraint (Prelude.Maybe Prelude.Text)
createConstraint_acceptLanguage = Lens.lens (\CreateConstraint' {acceptLanguage} -> acceptLanguage) (\s@CreateConstraint' {} a -> s {acceptLanguage = a} :: CreateConstraint)

-- | The portfolio identifier.
createConstraint_portfolioId :: Lens.Lens' CreateConstraint Prelude.Text
createConstraint_portfolioId = Lens.lens (\CreateConstraint' {portfolioId} -> portfolioId) (\s@CreateConstraint' {} a -> s {portfolioId = a} :: CreateConstraint)

-- | The product identifier.
createConstraint_productId :: Lens.Lens' CreateConstraint Prelude.Text
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
--     Products with a @STACKSET@ constraint will launch an CloudFormation
--     stack set.
--
-- [TEMPLATE]
--     Specify the @Rules@ property. For more information, see
--     <http://docs.aws.amazon.com/servicecatalog/latest/adminguide/reference-template_constraint_rules.html Template Constraint Rules>.
createConstraint_parameters :: Lens.Lens' CreateConstraint Prelude.Text
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
createConstraint_type :: Lens.Lens' CreateConstraint Prelude.Text
createConstraint_type = Lens.lens (\CreateConstraint' {type'} -> type') (\s@CreateConstraint' {} a -> s {type' = a} :: CreateConstraint)

-- | A unique identifier that you provide to ensure idempotency. If multiple
-- requests differ only by the idempotency token, the same response is
-- returned for each repeated request.
createConstraint_idempotencyToken :: Lens.Lens' CreateConstraint Prelude.Text
createConstraint_idempotencyToken = Lens.lens (\CreateConstraint' {idempotencyToken} -> idempotencyToken) (\s@CreateConstraint' {} a -> s {idempotencyToken = a} :: CreateConstraint)

instance Core.AWSRequest CreateConstraint where
  type
    AWSResponse CreateConstraint =
      CreateConstraintResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateConstraintResponse'
            Prelude.<$> (x Data..?> "ConstraintDetail")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "ConstraintParameters")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateConstraint where
  hashWithSalt _salt CreateConstraint' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` acceptLanguage
      `Prelude.hashWithSalt` portfolioId
      `Prelude.hashWithSalt` productId
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` idempotencyToken

instance Prelude.NFData CreateConstraint where
  rnf CreateConstraint' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf acceptLanguage
      `Prelude.seq` Prelude.rnf portfolioId
      `Prelude.seq` Prelude.rnf productId
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf idempotencyToken

instance Data.ToHeaders CreateConstraint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.CreateConstraint" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateConstraint where
  toJSON CreateConstraint' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("AcceptLanguage" Data..=)
              Prelude.<$> acceptLanguage,
            Prelude.Just ("PortfolioId" Data..= portfolioId),
            Prelude.Just ("ProductId" Data..= productId),
            Prelude.Just ("Parameters" Data..= parameters),
            Prelude.Just ("Type" Data..= type'),
            Prelude.Just
              ("IdempotencyToken" Data..= idempotencyToken)
          ]
      )

instance Data.ToPath CreateConstraint where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateConstraint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateConstraintResponse' smart constructor.
data CreateConstraintResponse = CreateConstraintResponse'
  { -- | Information about the constraint.
    constraintDetail :: Prelude.Maybe ConstraintDetail,
    -- | The status of the current request.
    status :: Prelude.Maybe RequestStatus,
    -- | The constraint parameters.
    constraintParameters :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConstraintResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'constraintDetail', 'createConstraintResponse_constraintDetail' - Information about the constraint.
--
-- 'status', 'createConstraintResponse_status' - The status of the current request.
--
-- 'constraintParameters', 'createConstraintResponse_constraintParameters' - The constraint parameters.
--
-- 'httpStatus', 'createConstraintResponse_httpStatus' - The response's http status code.
newCreateConstraintResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateConstraintResponse
newCreateConstraintResponse pHttpStatus_ =
  CreateConstraintResponse'
    { constraintDetail =
        Prelude.Nothing,
      status = Prelude.Nothing,
      constraintParameters = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the constraint.
createConstraintResponse_constraintDetail :: Lens.Lens' CreateConstraintResponse (Prelude.Maybe ConstraintDetail)
createConstraintResponse_constraintDetail = Lens.lens (\CreateConstraintResponse' {constraintDetail} -> constraintDetail) (\s@CreateConstraintResponse' {} a -> s {constraintDetail = a} :: CreateConstraintResponse)

-- | The status of the current request.
createConstraintResponse_status :: Lens.Lens' CreateConstraintResponse (Prelude.Maybe RequestStatus)
createConstraintResponse_status = Lens.lens (\CreateConstraintResponse' {status} -> status) (\s@CreateConstraintResponse' {} a -> s {status = a} :: CreateConstraintResponse)

-- | The constraint parameters.
createConstraintResponse_constraintParameters :: Lens.Lens' CreateConstraintResponse (Prelude.Maybe Prelude.Text)
createConstraintResponse_constraintParameters = Lens.lens (\CreateConstraintResponse' {constraintParameters} -> constraintParameters) (\s@CreateConstraintResponse' {} a -> s {constraintParameters = a} :: CreateConstraintResponse)

-- | The response's http status code.
createConstraintResponse_httpStatus :: Lens.Lens' CreateConstraintResponse Prelude.Int
createConstraintResponse_httpStatus = Lens.lens (\CreateConstraintResponse' {httpStatus} -> httpStatus) (\s@CreateConstraintResponse' {} a -> s {httpStatus = a} :: CreateConstraintResponse)

instance Prelude.NFData CreateConstraintResponse where
  rnf CreateConstraintResponse' {..} =
    Prelude.rnf constraintDetail
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf constraintParameters
      `Prelude.seq` Prelude.rnf httpStatus
