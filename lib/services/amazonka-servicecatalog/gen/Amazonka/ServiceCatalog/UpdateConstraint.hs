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
-- Module      : Amazonka.ServiceCatalog.UpdateConstraint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified constraint.
module Amazonka.ServiceCatalog.UpdateConstraint
  ( -- * Creating a Request
    UpdateConstraint (..),
    newUpdateConstraint,

    -- * Request Lenses
    updateConstraint_acceptLanguage,
    updateConstraint_description,
    updateConstraint_parameters,
    updateConstraint_id,

    -- * Destructuring the Response
    UpdateConstraintResponse (..),
    newUpdateConstraintResponse,

    -- * Response Lenses
    updateConstraintResponse_constraintDetail,
    updateConstraintResponse_constraintParameters,
    updateConstraintResponse_status,
    updateConstraintResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newUpdateConstraint' smart constructor.
data UpdateConstraint = UpdateConstraint'
  { -- | The language code.
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The updated description of the constraint.
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
    --     Products with a @STACKSET@ constraint will launch an CloudFormation
    --     stack set.
    --
    -- [TEMPLATE]
    --     Specify the @Rules@ property. For more information, see
    --     <http://docs.aws.amazon.com/servicecatalog/latest/adminguide/reference-template_constraint_rules.html Template Constraint Rules>.
    parameters :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the constraint.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateConstraint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptLanguage', 'updateConstraint_acceptLanguage' - The language code.
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
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
--     Products with a @STACKSET@ constraint will launch an CloudFormation
--     stack set.
--
-- [TEMPLATE]
--     Specify the @Rules@ property. For more information, see
--     <http://docs.aws.amazon.com/servicecatalog/latest/adminguide/reference-template_constraint_rules.html Template Constraint Rules>.
--
-- 'id', 'updateConstraint_id' - The identifier of the constraint.
newUpdateConstraint ::
  -- | 'id'
  Prelude.Text ->
  UpdateConstraint
newUpdateConstraint pId_ =
  UpdateConstraint'
    { acceptLanguage = Prelude.Nothing,
      description = Prelude.Nothing,
      parameters = Prelude.Nothing,
      id = pId_
    }

-- | The language code.
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
updateConstraint_acceptLanguage :: Lens.Lens' UpdateConstraint (Prelude.Maybe Prelude.Text)
updateConstraint_acceptLanguage = Lens.lens (\UpdateConstraint' {acceptLanguage} -> acceptLanguage) (\s@UpdateConstraint' {} a -> s {acceptLanguage = a} :: UpdateConstraint)

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
--     Products with a @STACKSET@ constraint will launch an CloudFormation
--     stack set.
--
-- [TEMPLATE]
--     Specify the @Rules@ property. For more information, see
--     <http://docs.aws.amazon.com/servicecatalog/latest/adminguide/reference-template_constraint_rules.html Template Constraint Rules>.
updateConstraint_parameters :: Lens.Lens' UpdateConstraint (Prelude.Maybe Prelude.Text)
updateConstraint_parameters = Lens.lens (\UpdateConstraint' {parameters} -> parameters) (\s@UpdateConstraint' {} a -> s {parameters = a} :: UpdateConstraint)

-- | The identifier of the constraint.
updateConstraint_id :: Lens.Lens' UpdateConstraint Prelude.Text
updateConstraint_id = Lens.lens (\UpdateConstraint' {id} -> id) (\s@UpdateConstraint' {} a -> s {id = a} :: UpdateConstraint)

instance Core.AWSRequest UpdateConstraint where
  type
    AWSResponse UpdateConstraint =
      UpdateConstraintResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateConstraintResponse'
            Prelude.<$> (x Data..?> "ConstraintDetail")
            Prelude.<*> (x Data..?> "ConstraintParameters")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateConstraint where
  hashWithSalt _salt UpdateConstraint' {..} =
    _salt
      `Prelude.hashWithSalt` acceptLanguage
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` id

instance Prelude.NFData UpdateConstraint where
  rnf UpdateConstraint' {..} =
    Prelude.rnf acceptLanguage
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders UpdateConstraint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.UpdateConstraint" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateConstraint where
  toJSON UpdateConstraint' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AcceptLanguage" Data..=)
              Prelude.<$> acceptLanguage,
            ("Description" Data..=) Prelude.<$> description,
            ("Parameters" Data..=) Prelude.<$> parameters,
            Prelude.Just ("Id" Data..= id)
          ]
      )

instance Data.ToPath UpdateConstraint where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateConstraint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateConstraintResponse' smart constructor.
data UpdateConstraintResponse = UpdateConstraintResponse'
  { -- | Information about the constraint.
    constraintDetail :: Prelude.Maybe ConstraintDetail,
    -- | The constraint parameters.
    constraintParameters :: Prelude.Maybe Prelude.Text,
    -- | The status of the current request.
    status :: Prelude.Maybe RequestStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateConstraintResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'constraintDetail', 'updateConstraintResponse_constraintDetail' - Information about the constraint.
--
-- 'constraintParameters', 'updateConstraintResponse_constraintParameters' - The constraint parameters.
--
-- 'status', 'updateConstraintResponse_status' - The status of the current request.
--
-- 'httpStatus', 'updateConstraintResponse_httpStatus' - The response's http status code.
newUpdateConstraintResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateConstraintResponse
newUpdateConstraintResponse pHttpStatus_ =
  UpdateConstraintResponse'
    { constraintDetail =
        Prelude.Nothing,
      constraintParameters = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the constraint.
updateConstraintResponse_constraintDetail :: Lens.Lens' UpdateConstraintResponse (Prelude.Maybe ConstraintDetail)
updateConstraintResponse_constraintDetail = Lens.lens (\UpdateConstraintResponse' {constraintDetail} -> constraintDetail) (\s@UpdateConstraintResponse' {} a -> s {constraintDetail = a} :: UpdateConstraintResponse)

-- | The constraint parameters.
updateConstraintResponse_constraintParameters :: Lens.Lens' UpdateConstraintResponse (Prelude.Maybe Prelude.Text)
updateConstraintResponse_constraintParameters = Lens.lens (\UpdateConstraintResponse' {constraintParameters} -> constraintParameters) (\s@UpdateConstraintResponse' {} a -> s {constraintParameters = a} :: UpdateConstraintResponse)

-- | The status of the current request.
updateConstraintResponse_status :: Lens.Lens' UpdateConstraintResponse (Prelude.Maybe RequestStatus)
updateConstraintResponse_status = Lens.lens (\UpdateConstraintResponse' {status} -> status) (\s@UpdateConstraintResponse' {} a -> s {status = a} :: UpdateConstraintResponse)

-- | The response's http status code.
updateConstraintResponse_httpStatus :: Lens.Lens' UpdateConstraintResponse Prelude.Int
updateConstraintResponse_httpStatus = Lens.lens (\UpdateConstraintResponse' {httpStatus} -> httpStatus) (\s@UpdateConstraintResponse' {} a -> s {httpStatus = a} :: UpdateConstraintResponse)

instance Prelude.NFData UpdateConstraintResponse where
  rnf UpdateConstraintResponse' {..} =
    Prelude.rnf constraintDetail
      `Prelude.seq` Prelude.rnf constraintParameters
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
