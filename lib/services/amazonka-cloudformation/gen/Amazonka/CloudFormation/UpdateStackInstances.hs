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
-- Module      : Amazonka.CloudFormation.UpdateStackInstances
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the parameter values for stack instances for the specified
-- accounts, within the specified Amazon Web Services Regions. A stack
-- instance refers to a stack in a specific account and Region.
--
-- You can only update stack instances in Amazon Web Services Regions and
-- accounts where they already exist; to create additional stack instances,
-- use
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_CreateStackInstances.html CreateStackInstances>.
--
-- During stack set updates, any parameters overridden for a stack instance
-- aren\'t updated, but retain their overridden value.
--
-- You can only update the parameter /values/ that are specified in the
-- stack set; to add or delete a parameter itself, use
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_UpdateStackSet.html UpdateStackSet>
-- to update the stack set template. If you add a parameter to a template,
-- before you can override the parameter value specified in the stack set
-- you must first use
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_UpdateStackSet.html UpdateStackSet>
-- to update all stack instances with the updated template and parameter
-- value specified in the stack set. Once a stack instance has been updated
-- with the new parameter, you can then override the parameter value using
-- @UpdateStackInstances@.
module Amazonka.CloudFormation.UpdateStackInstances
  ( -- * Creating a Request
    UpdateStackInstances (..),
    newUpdateStackInstances,

    -- * Request Lenses
    updateStackInstances_accounts,
    updateStackInstances_callAs,
    updateStackInstances_deploymentTargets,
    updateStackInstances_operationId,
    updateStackInstances_operationPreferences,
    updateStackInstances_parameterOverrides,
    updateStackInstances_stackSetName,
    updateStackInstances_regions,

    -- * Destructuring the Response
    UpdateStackInstancesResponse (..),
    newUpdateStackInstancesResponse,

    -- * Response Lenses
    updateStackInstancesResponse_operationId,
    updateStackInstancesResponse_httpStatus,
  )
where

import Amazonka.CloudFormation.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateStackInstances' smart constructor.
data UpdateStackInstances = UpdateStackInstances'
  { -- | [Self-managed permissions] The names of one or more Amazon Web Services
    -- accounts for which you want to update parameter values for stack
    -- instances. The overridden parameter values will be applied to all stack
    -- instances in the specified accounts and Amazon Web Services Regions.
    --
    -- You can specify @Accounts@ or @DeploymentTargets@, but not both.
    accounts :: Prelude.Maybe [Prelude.Text],
    -- | [Service-managed permissions] Specifies whether you are acting as an
    -- account administrator in the organization\'s management account or as a
    -- delegated administrator in a member account.
    --
    -- By default, @SELF@ is specified. Use @SELF@ for stack sets with
    -- self-managed permissions.
    --
    -- -   If you are signed in to the management account, specify @SELF@.
    --
    -- -   If you are signed in to a delegated administrator account, specify
    --     @DELEGATED_ADMIN@.
    --
    --     Your Amazon Web Services account must be registered as a delegated
    --     administrator in the management account. For more information, see
    --     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-orgs-delegated-admin.html Register a delegated administrator>
    --     in the /CloudFormation User Guide/.
    callAs :: Prelude.Maybe CallAs,
    -- | [Service-managed permissions] The Organizations accounts for which you
    -- want to update parameter values for stack instances. If your update
    -- targets OUs, the overridden parameter values only apply to the accounts
    -- that are currently in the target OUs and their child OUs. Accounts added
    -- to the target OUs and their child OUs in the future won\'t use the
    -- overridden values.
    --
    -- You can specify @Accounts@ or @DeploymentTargets@, but not both.
    deploymentTargets :: Prelude.Maybe DeploymentTargets,
    -- | The unique identifier for this stack set operation.
    --
    -- The operation ID also functions as an idempotency token, to ensure that
    -- CloudFormation performs the stack set operation only once, even if you
    -- retry the request multiple times. You might retry stack set operation
    -- requests to ensure that CloudFormation successfully received them.
    --
    -- If you don\'t specify an operation ID, the SDK generates one
    -- automatically.
    operationId :: Prelude.Maybe Prelude.Text,
    -- | Preferences for how CloudFormation performs this stack set operation.
    operationPreferences :: Prelude.Maybe StackSetOperationPreferences,
    -- | A list of input parameters whose values you want to update for the
    -- specified stack instances.
    --
    -- Any overridden parameter values will be applied to all stack instances
    -- in the specified accounts and Amazon Web Services Regions. When
    -- specifying parameters and their values, be aware of how CloudFormation
    -- sets parameter values during stack instance update operations:
    --
    -- -   To override the current value for a parameter, include the parameter
    --     and specify its value.
    --
    -- -   To leave an overridden parameter set to its present value, include
    --     the parameter and specify @UsePreviousValue@ as @true@. (You can\'t
    --     specify both a value and set @UsePreviousValue@ to @true@.)
    --
    -- -   To set an overridden parameter back to the value specified in the
    --     stack set, specify a parameter list but don\'t include the parameter
    --     in the list.
    --
    -- -   To leave all parameters set to their present values, don\'t specify
    --     this property at all.
    --
    -- During stack set updates, any parameter values overridden for a stack
    -- instance aren\'t updated, but retain their overridden value.
    --
    -- You can only override the parameter /values/ that are specified in the
    -- stack set; to add or delete a parameter itself, use @UpdateStackSet@ to
    -- update the stack set template. If you add a parameter to a template,
    -- before you can override the parameter value specified in the stack set
    -- you must first use
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_UpdateStackSet.html UpdateStackSet>
    -- to update all stack instances with the updated template and parameter
    -- value specified in the stack set. Once a stack instance has been updated
    -- with the new parameter, you can then override the parameter value using
    -- @UpdateStackInstances@.
    parameterOverrides :: Prelude.Maybe [Parameter],
    -- | The name or unique ID of the stack set associated with the stack
    -- instances.
    stackSetName :: Prelude.Text,
    -- | The names of one or more Amazon Web Services Regions in which you want
    -- to update parameter values for stack instances. The overridden parameter
    -- values will be applied to all stack instances in the specified accounts
    -- and Amazon Web Services Regions.
    regions :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateStackInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accounts', 'updateStackInstances_accounts' - [Self-managed permissions] The names of one or more Amazon Web Services
-- accounts for which you want to update parameter values for stack
-- instances. The overridden parameter values will be applied to all stack
-- instances in the specified accounts and Amazon Web Services Regions.
--
-- You can specify @Accounts@ or @DeploymentTargets@, but not both.
--
-- 'callAs', 'updateStackInstances_callAs' - [Service-managed permissions] Specifies whether you are acting as an
-- account administrator in the organization\'s management account or as a
-- delegated administrator in a member account.
--
-- By default, @SELF@ is specified. Use @SELF@ for stack sets with
-- self-managed permissions.
--
-- -   If you are signed in to the management account, specify @SELF@.
--
-- -   If you are signed in to a delegated administrator account, specify
--     @DELEGATED_ADMIN@.
--
--     Your Amazon Web Services account must be registered as a delegated
--     administrator in the management account. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-orgs-delegated-admin.html Register a delegated administrator>
--     in the /CloudFormation User Guide/.
--
-- 'deploymentTargets', 'updateStackInstances_deploymentTargets' - [Service-managed permissions] The Organizations accounts for which you
-- want to update parameter values for stack instances. If your update
-- targets OUs, the overridden parameter values only apply to the accounts
-- that are currently in the target OUs and their child OUs. Accounts added
-- to the target OUs and their child OUs in the future won\'t use the
-- overridden values.
--
-- You can specify @Accounts@ or @DeploymentTargets@, but not both.
--
-- 'operationId', 'updateStackInstances_operationId' - The unique identifier for this stack set operation.
--
-- The operation ID also functions as an idempotency token, to ensure that
-- CloudFormation performs the stack set operation only once, even if you
-- retry the request multiple times. You might retry stack set operation
-- requests to ensure that CloudFormation successfully received them.
--
-- If you don\'t specify an operation ID, the SDK generates one
-- automatically.
--
-- 'operationPreferences', 'updateStackInstances_operationPreferences' - Preferences for how CloudFormation performs this stack set operation.
--
-- 'parameterOverrides', 'updateStackInstances_parameterOverrides' - A list of input parameters whose values you want to update for the
-- specified stack instances.
--
-- Any overridden parameter values will be applied to all stack instances
-- in the specified accounts and Amazon Web Services Regions. When
-- specifying parameters and their values, be aware of how CloudFormation
-- sets parameter values during stack instance update operations:
--
-- -   To override the current value for a parameter, include the parameter
--     and specify its value.
--
-- -   To leave an overridden parameter set to its present value, include
--     the parameter and specify @UsePreviousValue@ as @true@. (You can\'t
--     specify both a value and set @UsePreviousValue@ to @true@.)
--
-- -   To set an overridden parameter back to the value specified in the
--     stack set, specify a parameter list but don\'t include the parameter
--     in the list.
--
-- -   To leave all parameters set to their present values, don\'t specify
--     this property at all.
--
-- During stack set updates, any parameter values overridden for a stack
-- instance aren\'t updated, but retain their overridden value.
--
-- You can only override the parameter /values/ that are specified in the
-- stack set; to add or delete a parameter itself, use @UpdateStackSet@ to
-- update the stack set template. If you add a parameter to a template,
-- before you can override the parameter value specified in the stack set
-- you must first use
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_UpdateStackSet.html UpdateStackSet>
-- to update all stack instances with the updated template and parameter
-- value specified in the stack set. Once a stack instance has been updated
-- with the new parameter, you can then override the parameter value using
-- @UpdateStackInstances@.
--
-- 'stackSetName', 'updateStackInstances_stackSetName' - The name or unique ID of the stack set associated with the stack
-- instances.
--
-- 'regions', 'updateStackInstances_regions' - The names of one or more Amazon Web Services Regions in which you want
-- to update parameter values for stack instances. The overridden parameter
-- values will be applied to all stack instances in the specified accounts
-- and Amazon Web Services Regions.
newUpdateStackInstances ::
  -- | 'stackSetName'
  Prelude.Text ->
  UpdateStackInstances
newUpdateStackInstances pStackSetName_ =
  UpdateStackInstances'
    { accounts = Prelude.Nothing,
      callAs = Prelude.Nothing,
      deploymentTargets = Prelude.Nothing,
      operationId = Prelude.Nothing,
      operationPreferences = Prelude.Nothing,
      parameterOverrides = Prelude.Nothing,
      stackSetName = pStackSetName_,
      regions = Prelude.mempty
    }

-- | [Self-managed permissions] The names of one or more Amazon Web Services
-- accounts for which you want to update parameter values for stack
-- instances. The overridden parameter values will be applied to all stack
-- instances in the specified accounts and Amazon Web Services Regions.
--
-- You can specify @Accounts@ or @DeploymentTargets@, but not both.
updateStackInstances_accounts :: Lens.Lens' UpdateStackInstances (Prelude.Maybe [Prelude.Text])
updateStackInstances_accounts = Lens.lens (\UpdateStackInstances' {accounts} -> accounts) (\s@UpdateStackInstances' {} a -> s {accounts = a} :: UpdateStackInstances) Prelude.. Lens.mapping Lens.coerced

-- | [Service-managed permissions] Specifies whether you are acting as an
-- account administrator in the organization\'s management account or as a
-- delegated administrator in a member account.
--
-- By default, @SELF@ is specified. Use @SELF@ for stack sets with
-- self-managed permissions.
--
-- -   If you are signed in to the management account, specify @SELF@.
--
-- -   If you are signed in to a delegated administrator account, specify
--     @DELEGATED_ADMIN@.
--
--     Your Amazon Web Services account must be registered as a delegated
--     administrator in the management account. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-orgs-delegated-admin.html Register a delegated administrator>
--     in the /CloudFormation User Guide/.
updateStackInstances_callAs :: Lens.Lens' UpdateStackInstances (Prelude.Maybe CallAs)
updateStackInstances_callAs = Lens.lens (\UpdateStackInstances' {callAs} -> callAs) (\s@UpdateStackInstances' {} a -> s {callAs = a} :: UpdateStackInstances)

-- | [Service-managed permissions] The Organizations accounts for which you
-- want to update parameter values for stack instances. If your update
-- targets OUs, the overridden parameter values only apply to the accounts
-- that are currently in the target OUs and their child OUs. Accounts added
-- to the target OUs and their child OUs in the future won\'t use the
-- overridden values.
--
-- You can specify @Accounts@ or @DeploymentTargets@, but not both.
updateStackInstances_deploymentTargets :: Lens.Lens' UpdateStackInstances (Prelude.Maybe DeploymentTargets)
updateStackInstances_deploymentTargets = Lens.lens (\UpdateStackInstances' {deploymentTargets} -> deploymentTargets) (\s@UpdateStackInstances' {} a -> s {deploymentTargets = a} :: UpdateStackInstances)

-- | The unique identifier for this stack set operation.
--
-- The operation ID also functions as an idempotency token, to ensure that
-- CloudFormation performs the stack set operation only once, even if you
-- retry the request multiple times. You might retry stack set operation
-- requests to ensure that CloudFormation successfully received them.
--
-- If you don\'t specify an operation ID, the SDK generates one
-- automatically.
updateStackInstances_operationId :: Lens.Lens' UpdateStackInstances (Prelude.Maybe Prelude.Text)
updateStackInstances_operationId = Lens.lens (\UpdateStackInstances' {operationId} -> operationId) (\s@UpdateStackInstances' {} a -> s {operationId = a} :: UpdateStackInstances)

-- | Preferences for how CloudFormation performs this stack set operation.
updateStackInstances_operationPreferences :: Lens.Lens' UpdateStackInstances (Prelude.Maybe StackSetOperationPreferences)
updateStackInstances_operationPreferences = Lens.lens (\UpdateStackInstances' {operationPreferences} -> operationPreferences) (\s@UpdateStackInstances' {} a -> s {operationPreferences = a} :: UpdateStackInstances)

-- | A list of input parameters whose values you want to update for the
-- specified stack instances.
--
-- Any overridden parameter values will be applied to all stack instances
-- in the specified accounts and Amazon Web Services Regions. When
-- specifying parameters and their values, be aware of how CloudFormation
-- sets parameter values during stack instance update operations:
--
-- -   To override the current value for a parameter, include the parameter
--     and specify its value.
--
-- -   To leave an overridden parameter set to its present value, include
--     the parameter and specify @UsePreviousValue@ as @true@. (You can\'t
--     specify both a value and set @UsePreviousValue@ to @true@.)
--
-- -   To set an overridden parameter back to the value specified in the
--     stack set, specify a parameter list but don\'t include the parameter
--     in the list.
--
-- -   To leave all parameters set to their present values, don\'t specify
--     this property at all.
--
-- During stack set updates, any parameter values overridden for a stack
-- instance aren\'t updated, but retain their overridden value.
--
-- You can only override the parameter /values/ that are specified in the
-- stack set; to add or delete a parameter itself, use @UpdateStackSet@ to
-- update the stack set template. If you add a parameter to a template,
-- before you can override the parameter value specified in the stack set
-- you must first use
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_UpdateStackSet.html UpdateStackSet>
-- to update all stack instances with the updated template and parameter
-- value specified in the stack set. Once a stack instance has been updated
-- with the new parameter, you can then override the parameter value using
-- @UpdateStackInstances@.
updateStackInstances_parameterOverrides :: Lens.Lens' UpdateStackInstances (Prelude.Maybe [Parameter])
updateStackInstances_parameterOverrides = Lens.lens (\UpdateStackInstances' {parameterOverrides} -> parameterOverrides) (\s@UpdateStackInstances' {} a -> s {parameterOverrides = a} :: UpdateStackInstances) Prelude.. Lens.mapping Lens.coerced

-- | The name or unique ID of the stack set associated with the stack
-- instances.
updateStackInstances_stackSetName :: Lens.Lens' UpdateStackInstances Prelude.Text
updateStackInstances_stackSetName = Lens.lens (\UpdateStackInstances' {stackSetName} -> stackSetName) (\s@UpdateStackInstances' {} a -> s {stackSetName = a} :: UpdateStackInstances)

-- | The names of one or more Amazon Web Services Regions in which you want
-- to update parameter values for stack instances. The overridden parameter
-- values will be applied to all stack instances in the specified accounts
-- and Amazon Web Services Regions.
updateStackInstances_regions :: Lens.Lens' UpdateStackInstances [Prelude.Text]
updateStackInstances_regions = Lens.lens (\UpdateStackInstances' {regions} -> regions) (\s@UpdateStackInstances' {} a -> s {regions = a} :: UpdateStackInstances) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateStackInstances where
  type
    AWSResponse UpdateStackInstances =
      UpdateStackInstancesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "UpdateStackInstancesResult"
      ( \s h x ->
          UpdateStackInstancesResponse'
            Prelude.<$> (x Data..@? "OperationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateStackInstances where
  hashWithSalt _salt UpdateStackInstances' {..} =
    _salt `Prelude.hashWithSalt` accounts
      `Prelude.hashWithSalt` callAs
      `Prelude.hashWithSalt` deploymentTargets
      `Prelude.hashWithSalt` operationId
      `Prelude.hashWithSalt` operationPreferences
      `Prelude.hashWithSalt` parameterOverrides
      `Prelude.hashWithSalt` stackSetName
      `Prelude.hashWithSalt` regions

instance Prelude.NFData UpdateStackInstances where
  rnf UpdateStackInstances' {..} =
    Prelude.rnf accounts
      `Prelude.seq` Prelude.rnf callAs
      `Prelude.seq` Prelude.rnf deploymentTargets
      `Prelude.seq` Prelude.rnf operationId
      `Prelude.seq` Prelude.rnf operationPreferences
      `Prelude.seq` Prelude.rnf parameterOverrides
      `Prelude.seq` Prelude.rnf stackSetName
      `Prelude.seq` Prelude.rnf regions

instance Data.ToHeaders UpdateStackInstances where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath UpdateStackInstances where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateStackInstances where
  toQuery UpdateStackInstances' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("UpdateStackInstances" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-15" :: Prelude.ByteString),
        "Accounts"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> accounts),
        "CallAs" Data.=: callAs,
        "DeploymentTargets" Data.=: deploymentTargets,
        "OperationId" Data.=: operationId,
        "OperationPreferences" Data.=: operationPreferences,
        "ParameterOverrides"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> parameterOverrides
            ),
        "StackSetName" Data.=: stackSetName,
        "Regions" Data.=: Data.toQueryList "member" regions
      ]

-- | /See:/ 'newUpdateStackInstancesResponse' smart constructor.
data UpdateStackInstancesResponse = UpdateStackInstancesResponse'
  { -- | The unique identifier for this stack set operation.
    operationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateStackInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operationId', 'updateStackInstancesResponse_operationId' - The unique identifier for this stack set operation.
--
-- 'httpStatus', 'updateStackInstancesResponse_httpStatus' - The response's http status code.
newUpdateStackInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateStackInstancesResponse
newUpdateStackInstancesResponse pHttpStatus_ =
  UpdateStackInstancesResponse'
    { operationId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier for this stack set operation.
updateStackInstancesResponse_operationId :: Lens.Lens' UpdateStackInstancesResponse (Prelude.Maybe Prelude.Text)
updateStackInstancesResponse_operationId = Lens.lens (\UpdateStackInstancesResponse' {operationId} -> operationId) (\s@UpdateStackInstancesResponse' {} a -> s {operationId = a} :: UpdateStackInstancesResponse)

-- | The response's http status code.
updateStackInstancesResponse_httpStatus :: Lens.Lens' UpdateStackInstancesResponse Prelude.Int
updateStackInstancesResponse_httpStatus = Lens.lens (\UpdateStackInstancesResponse' {httpStatus} -> httpStatus) (\s@UpdateStackInstancesResponse' {} a -> s {httpStatus = a} :: UpdateStackInstancesResponse)

instance Prelude.NFData UpdateStackInstancesResponse where
  rnf UpdateStackInstancesResponse' {..} =
    Prelude.rnf operationId
      `Prelude.seq` Prelude.rnf httpStatus
