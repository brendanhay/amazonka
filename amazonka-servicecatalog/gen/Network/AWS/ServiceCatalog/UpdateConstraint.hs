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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newUpdateConstraint' smart constructor.
data UpdateConstraint = UpdateConstraint'
  { -- | The updated description of the constraint.
    description :: Prelude.Maybe Prelude.Text,
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
    parameters :: Prelude.Maybe Prelude.Text,
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the constraint.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  UpdateConstraint
newUpdateConstraint pId_ =
  UpdateConstraint'
    { description = Prelude.Nothing,
      parameters = Prelude.Nothing,
      acceptLanguage = Prelude.Nothing,
      id = pId_
    }

-- | The updated description of the constraint.
updateConstraint_description :: Lens.Lens' UpdateConstraint (Prelude.Maybe Prelude.Text)
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
updateConstraint_parameters :: Lens.Lens' UpdateConstraint (Prelude.Maybe Prelude.Text)
updateConstraint_parameters = Lens.lens (\UpdateConstraint' {parameters} -> parameters) (\s@UpdateConstraint' {} a -> s {parameters = a} :: UpdateConstraint)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
updateConstraint_acceptLanguage :: Lens.Lens' UpdateConstraint (Prelude.Maybe Prelude.Text)
updateConstraint_acceptLanguage = Lens.lens (\UpdateConstraint' {acceptLanguage} -> acceptLanguage) (\s@UpdateConstraint' {} a -> s {acceptLanguage = a} :: UpdateConstraint)

-- | The identifier of the constraint.
updateConstraint_id :: Lens.Lens' UpdateConstraint Prelude.Text
updateConstraint_id = Lens.lens (\UpdateConstraint' {id} -> id) (\s@UpdateConstraint' {} a -> s {id = a} :: UpdateConstraint)

instance Prelude.AWSRequest UpdateConstraint where
  type Rs UpdateConstraint = UpdateConstraintResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateConstraintResponse'
            Prelude.<$> (x Prelude..?> "ConstraintParameters")
            Prelude.<*> (x Prelude..?> "Status")
            Prelude.<*> (x Prelude..?> "ConstraintDetail")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateConstraint

instance Prelude.NFData UpdateConstraint

instance Prelude.ToHeaders UpdateConstraint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWS242ServiceCatalogService.UpdateConstraint" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateConstraint where
  toJSON UpdateConstraint' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Description" Prelude..=) Prelude.<$> description,
            ("Parameters" Prelude..=) Prelude.<$> parameters,
            ("AcceptLanguage" Prelude..=)
              Prelude.<$> acceptLanguage,
            Prelude.Just ("Id" Prelude..= id)
          ]
      )

instance Prelude.ToPath UpdateConstraint where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateConstraint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateConstraintResponse' smart constructor.
data UpdateConstraintResponse = UpdateConstraintResponse'
  { -- | The constraint parameters.
    constraintParameters :: Prelude.Maybe Prelude.Text,
    -- | The status of the current request.
    status :: Prelude.Maybe RequestStatus,
    -- | Information about the constraint.
    constraintDetail :: Prelude.Maybe ConstraintDetail,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  UpdateConstraintResponse
newUpdateConstraintResponse pHttpStatus_ =
  UpdateConstraintResponse'
    { constraintParameters =
        Prelude.Nothing,
      status = Prelude.Nothing,
      constraintDetail = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The constraint parameters.
updateConstraintResponse_constraintParameters :: Lens.Lens' UpdateConstraintResponse (Prelude.Maybe Prelude.Text)
updateConstraintResponse_constraintParameters = Lens.lens (\UpdateConstraintResponse' {constraintParameters} -> constraintParameters) (\s@UpdateConstraintResponse' {} a -> s {constraintParameters = a} :: UpdateConstraintResponse)

-- | The status of the current request.
updateConstraintResponse_status :: Lens.Lens' UpdateConstraintResponse (Prelude.Maybe RequestStatus)
updateConstraintResponse_status = Lens.lens (\UpdateConstraintResponse' {status} -> status) (\s@UpdateConstraintResponse' {} a -> s {status = a} :: UpdateConstraintResponse)

-- | Information about the constraint.
updateConstraintResponse_constraintDetail :: Lens.Lens' UpdateConstraintResponse (Prelude.Maybe ConstraintDetail)
updateConstraintResponse_constraintDetail = Lens.lens (\UpdateConstraintResponse' {constraintDetail} -> constraintDetail) (\s@UpdateConstraintResponse' {} a -> s {constraintDetail = a} :: UpdateConstraintResponse)

-- | The response's http status code.
updateConstraintResponse_httpStatus :: Lens.Lens' UpdateConstraintResponse Prelude.Int
updateConstraintResponse_httpStatus = Lens.lens (\UpdateConstraintResponse' {httpStatus} -> httpStatus) (\s@UpdateConstraintResponse' {} a -> s {httpStatus = a} :: UpdateConstraintResponse)

instance Prelude.NFData UpdateConstraintResponse
