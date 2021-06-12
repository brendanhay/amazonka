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
-- Module      : Network.AWS.ServiceCatalog.UpdateConstraint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified constraint.
module Network.AWS.ServiceCatalog.UpdateConstraint
  ( -- * Creating a Request
    UpdateConstraint (..),
    newUpdateConstraint,

    -- * Request Lenses
    updateConstraint_description,
    updateConstraint_parameters,
    updateConstraint_acceptLanguage,
    updateConstraint_id,

    -- * Destructuring the Response
    UpdateConstraintResponse (..),
    newUpdateConstraintResponse,

    -- * Response Lenses
    updateConstraintResponse_constraintParameters,
    updateConstraintResponse_status,
    updateConstraintResponse_constraintDetail,
    updateConstraintResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newUpdateConstraint' smart constructor.
data UpdateConstraint = UpdateConstraint'
  { -- | The updated description of the constraint.
    description :: Core.Maybe Core.Text,
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
    parameters :: Core.Maybe Core.Text,
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Core.Maybe Core.Text,
    -- | The identifier of the constraint.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateConstraint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateConstraint_description' - The updated description of the constraint.
--
-- 'parameters', 'updateConstraint_parameters' - The constraint parameters, in JSON format. The syntax depends on the
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
-- 'acceptLanguage', 'updateConstraint_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'id', 'updateConstraint_id' - The identifier of the constraint.
newUpdateConstraint ::
  -- | 'id'
  Core.Text ->
  UpdateConstraint
newUpdateConstraint pId_ =
  UpdateConstraint'
    { description = Core.Nothing,
      parameters = Core.Nothing,
      acceptLanguage = Core.Nothing,
      id = pId_
    }

-- | The updated description of the constraint.
updateConstraint_description :: Lens.Lens' UpdateConstraint (Core.Maybe Core.Text)
updateConstraint_description = Lens.lens (\UpdateConstraint' {description} -> description) (\s@UpdateConstraint' {} a -> s {description = a} :: UpdateConstraint)

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
updateConstraint_parameters :: Lens.Lens' UpdateConstraint (Core.Maybe Core.Text)
updateConstraint_parameters = Lens.lens (\UpdateConstraint' {parameters} -> parameters) (\s@UpdateConstraint' {} a -> s {parameters = a} :: UpdateConstraint)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
updateConstraint_acceptLanguage :: Lens.Lens' UpdateConstraint (Core.Maybe Core.Text)
updateConstraint_acceptLanguage = Lens.lens (\UpdateConstraint' {acceptLanguage} -> acceptLanguage) (\s@UpdateConstraint' {} a -> s {acceptLanguage = a} :: UpdateConstraint)

-- | The identifier of the constraint.
updateConstraint_id :: Lens.Lens' UpdateConstraint Core.Text
updateConstraint_id = Lens.lens (\UpdateConstraint' {id} -> id) (\s@UpdateConstraint' {} a -> s {id = a} :: UpdateConstraint)

instance Core.AWSRequest UpdateConstraint where
  type
    AWSResponse UpdateConstraint =
      UpdateConstraintResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateConstraintResponse'
            Core.<$> (x Core..?> "ConstraintParameters")
            Core.<*> (x Core..?> "Status")
            Core.<*> (x Core..?> "ConstraintDetail")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateConstraint

instance Core.NFData UpdateConstraint

instance Core.ToHeaders UpdateConstraint where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.UpdateConstraint" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateConstraint where
  toJSON UpdateConstraint' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Description" Core..=) Core.<$> description,
            ("Parameters" Core..=) Core.<$> parameters,
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            Core.Just ("Id" Core..= id)
          ]
      )

instance Core.ToPath UpdateConstraint where
  toPath = Core.const "/"

instance Core.ToQuery UpdateConstraint where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateConstraintResponse' smart constructor.
data UpdateConstraintResponse = UpdateConstraintResponse'
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
-- Create a value of 'UpdateConstraintResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'constraintParameters', 'updateConstraintResponse_constraintParameters' - The constraint parameters.
--
-- 'status', 'updateConstraintResponse_status' - The status of the current request.
--
-- 'constraintDetail', 'updateConstraintResponse_constraintDetail' - Information about the constraint.
--
-- 'httpStatus', 'updateConstraintResponse_httpStatus' - The response's http status code.
newUpdateConstraintResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateConstraintResponse
newUpdateConstraintResponse pHttpStatus_ =
  UpdateConstraintResponse'
    { constraintParameters =
        Core.Nothing,
      status = Core.Nothing,
      constraintDetail = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The constraint parameters.
updateConstraintResponse_constraintParameters :: Lens.Lens' UpdateConstraintResponse (Core.Maybe Core.Text)
updateConstraintResponse_constraintParameters = Lens.lens (\UpdateConstraintResponse' {constraintParameters} -> constraintParameters) (\s@UpdateConstraintResponse' {} a -> s {constraintParameters = a} :: UpdateConstraintResponse)

-- | The status of the current request.
updateConstraintResponse_status :: Lens.Lens' UpdateConstraintResponse (Core.Maybe RequestStatus)
updateConstraintResponse_status = Lens.lens (\UpdateConstraintResponse' {status} -> status) (\s@UpdateConstraintResponse' {} a -> s {status = a} :: UpdateConstraintResponse)

-- | Information about the constraint.
updateConstraintResponse_constraintDetail :: Lens.Lens' UpdateConstraintResponse (Core.Maybe ConstraintDetail)
updateConstraintResponse_constraintDetail = Lens.lens (\UpdateConstraintResponse' {constraintDetail} -> constraintDetail) (\s@UpdateConstraintResponse' {} a -> s {constraintDetail = a} :: UpdateConstraintResponse)

-- | The response's http status code.
updateConstraintResponse_httpStatus :: Lens.Lens' UpdateConstraintResponse Core.Int
updateConstraintResponse_httpStatus = Lens.lens (\UpdateConstraintResponse' {httpStatus} -> httpStatus) (\s@UpdateConstraintResponse' {} a -> s {httpStatus = a} :: UpdateConstraintResponse)

instance Core.NFData UpdateConstraintResponse
