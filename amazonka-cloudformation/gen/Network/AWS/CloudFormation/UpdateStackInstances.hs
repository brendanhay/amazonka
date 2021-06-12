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
-- Module      : Network.AWS.CloudFormation.UpdateStackInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the parameter values for stack instances for the specified
-- accounts, within the specified Regions. A stack instance refers to a
-- stack in a specific account and Region.
--
-- You can only update stack instances in Regions and accounts where they
-- already exist; to create additional stack instances, use
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_CreateStackInstances.html CreateStackInstances>.
--
-- During stack set updates, any parameters overridden for a stack instance
-- are not updated, but retain their overridden value.
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
module Network.AWS.CloudFormation.UpdateStackInstances
  ( -- * Creating a Request
    UpdateStackInstances (..),
    newUpdateStackInstances,

    -- * Request Lenses
    updateStackInstances_parameterOverrides,
    updateStackInstances_deploymentTargets,
    updateStackInstances_operationId,
    updateStackInstances_callAs,
    updateStackInstances_operationPreferences,
    updateStackInstances_accounts,
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

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateStackInstances' smart constructor.
data UpdateStackInstances = UpdateStackInstances'
  { -- | A list of input parameters whose values you want to update for the
    -- specified stack instances.
    --
    -- Any overridden parameter values will be applied to all stack instances
    -- in the specified accounts and Regions. When specifying parameters and
    -- their values, be aware of how AWS CloudFormation sets parameter values
    -- during stack instance update operations:
    --
    -- -   To override the current value for a parameter, include the parameter
    --     and specify its value.
    --
    -- -   To leave a parameter set to its present value, you can do one of the
    --     following:
    --
    --     -   Do not include the parameter in the list.
    --
    --     -   Include the parameter and specify @UsePreviousValue@ as @true@.
    --         (You cannot specify both a value and set @UsePreviousValue@ to
    --         @true@.)
    --
    -- -   To set all overridden parameter back to the values specified in the
    --     stack set, specify a parameter list but do not include any
    --     parameters.
    --
    -- -   To leave all parameters set to their present values, do not specify
    --     this property at all.
    --
    -- During stack set updates, any parameter values overridden for a stack
    -- instance are not updated, but retain their overridden value.
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
    parameterOverrides :: Core.Maybe [Parameter],
    -- | [Service-managed permissions] The AWS Organizations accounts for which
    -- you want to update parameter values for stack instances. If your update
    -- targets OUs, the overridden parameter values only apply to the accounts
    -- that are currently in the target OUs and their child OUs. Accounts added
    -- to the target OUs and their child OUs in the future won\'t use the
    -- overridden values.
    --
    -- You can specify @Accounts@ or @DeploymentTargets@, but not both.
    deploymentTargets :: Core.Maybe DeploymentTargets,
    -- | The unique identifier for this stack set operation.
    --
    -- The operation ID also functions as an idempotency token, to ensure that
    -- AWS CloudFormation performs the stack set operation only once, even if
    -- you retry the request multiple times. You might retry stack set
    -- operation requests to ensure that AWS CloudFormation successfully
    -- received them.
    --
    -- If you don\'t specify an operation ID, the SDK generates one
    -- automatically.
    operationId :: Core.Maybe Core.Text,
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
    --     Your AWS account must be registered as a delegated administrator in
    --     the management account. For more information, see
    --     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-orgs-delegated-admin.html Register a delegated administrator>
    --     in the /AWS CloudFormation User Guide/.
    callAs :: Core.Maybe CallAs,
    -- | Preferences for how AWS CloudFormation performs this stack set
    -- operation.
    operationPreferences :: Core.Maybe StackSetOperationPreferences,
    -- | [Self-managed permissions] The names of one or more AWS accounts for
    -- which you want to update parameter values for stack instances. The
    -- overridden parameter values will be applied to all stack instances in
    -- the specified accounts and Regions.
    --
    -- You can specify @Accounts@ or @DeploymentTargets@, but not both.
    accounts :: Core.Maybe [Core.Text],
    -- | The name or unique ID of the stack set associated with the stack
    -- instances.
    stackSetName :: Core.Text,
    -- | The names of one or more Regions in which you want to update parameter
    -- values for stack instances. The overridden parameter values will be
    -- applied to all stack instances in the specified accounts and Regions.
    regions :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateStackInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameterOverrides', 'updateStackInstances_parameterOverrides' - A list of input parameters whose values you want to update for the
-- specified stack instances.
--
-- Any overridden parameter values will be applied to all stack instances
-- in the specified accounts and Regions. When specifying parameters and
-- their values, be aware of how AWS CloudFormation sets parameter values
-- during stack instance update operations:
--
-- -   To override the current value for a parameter, include the parameter
--     and specify its value.
--
-- -   To leave a parameter set to its present value, you can do one of the
--     following:
--
--     -   Do not include the parameter in the list.
--
--     -   Include the parameter and specify @UsePreviousValue@ as @true@.
--         (You cannot specify both a value and set @UsePreviousValue@ to
--         @true@.)
--
-- -   To set all overridden parameter back to the values specified in the
--     stack set, specify a parameter list but do not include any
--     parameters.
--
-- -   To leave all parameters set to their present values, do not specify
--     this property at all.
--
-- During stack set updates, any parameter values overridden for a stack
-- instance are not updated, but retain their overridden value.
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
-- 'deploymentTargets', 'updateStackInstances_deploymentTargets' - [Service-managed permissions] The AWS Organizations accounts for which
-- you want to update parameter values for stack instances. If your update
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
-- AWS CloudFormation performs the stack set operation only once, even if
-- you retry the request multiple times. You might retry stack set
-- operation requests to ensure that AWS CloudFormation successfully
-- received them.
--
-- If you don\'t specify an operation ID, the SDK generates one
-- automatically.
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
--     Your AWS account must be registered as a delegated administrator in
--     the management account. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-orgs-delegated-admin.html Register a delegated administrator>
--     in the /AWS CloudFormation User Guide/.
--
-- 'operationPreferences', 'updateStackInstances_operationPreferences' - Preferences for how AWS CloudFormation performs this stack set
-- operation.
--
-- 'accounts', 'updateStackInstances_accounts' - [Self-managed permissions] The names of one or more AWS accounts for
-- which you want to update parameter values for stack instances. The
-- overridden parameter values will be applied to all stack instances in
-- the specified accounts and Regions.
--
-- You can specify @Accounts@ or @DeploymentTargets@, but not both.
--
-- 'stackSetName', 'updateStackInstances_stackSetName' - The name or unique ID of the stack set associated with the stack
-- instances.
--
-- 'regions', 'updateStackInstances_regions' - The names of one or more Regions in which you want to update parameter
-- values for stack instances. The overridden parameter values will be
-- applied to all stack instances in the specified accounts and Regions.
newUpdateStackInstances ::
  -- | 'stackSetName'
  Core.Text ->
  UpdateStackInstances
newUpdateStackInstances pStackSetName_ =
  UpdateStackInstances'
    { parameterOverrides =
        Core.Nothing,
      deploymentTargets = Core.Nothing,
      operationId = Core.Nothing,
      callAs = Core.Nothing,
      operationPreferences = Core.Nothing,
      accounts = Core.Nothing,
      stackSetName = pStackSetName_,
      regions = Core.mempty
    }

-- | A list of input parameters whose values you want to update for the
-- specified stack instances.
--
-- Any overridden parameter values will be applied to all stack instances
-- in the specified accounts and Regions. When specifying parameters and
-- their values, be aware of how AWS CloudFormation sets parameter values
-- during stack instance update operations:
--
-- -   To override the current value for a parameter, include the parameter
--     and specify its value.
--
-- -   To leave a parameter set to its present value, you can do one of the
--     following:
--
--     -   Do not include the parameter in the list.
--
--     -   Include the parameter and specify @UsePreviousValue@ as @true@.
--         (You cannot specify both a value and set @UsePreviousValue@ to
--         @true@.)
--
-- -   To set all overridden parameter back to the values specified in the
--     stack set, specify a parameter list but do not include any
--     parameters.
--
-- -   To leave all parameters set to their present values, do not specify
--     this property at all.
--
-- During stack set updates, any parameter values overridden for a stack
-- instance are not updated, but retain their overridden value.
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
updateStackInstances_parameterOverrides :: Lens.Lens' UpdateStackInstances (Core.Maybe [Parameter])
updateStackInstances_parameterOverrides = Lens.lens (\UpdateStackInstances' {parameterOverrides} -> parameterOverrides) (\s@UpdateStackInstances' {} a -> s {parameterOverrides = a} :: UpdateStackInstances) Core.. Lens.mapping Lens._Coerce

-- | [Service-managed permissions] The AWS Organizations accounts for which
-- you want to update parameter values for stack instances. If your update
-- targets OUs, the overridden parameter values only apply to the accounts
-- that are currently in the target OUs and their child OUs. Accounts added
-- to the target OUs and their child OUs in the future won\'t use the
-- overridden values.
--
-- You can specify @Accounts@ or @DeploymentTargets@, but not both.
updateStackInstances_deploymentTargets :: Lens.Lens' UpdateStackInstances (Core.Maybe DeploymentTargets)
updateStackInstances_deploymentTargets = Lens.lens (\UpdateStackInstances' {deploymentTargets} -> deploymentTargets) (\s@UpdateStackInstances' {} a -> s {deploymentTargets = a} :: UpdateStackInstances)

-- | The unique identifier for this stack set operation.
--
-- The operation ID also functions as an idempotency token, to ensure that
-- AWS CloudFormation performs the stack set operation only once, even if
-- you retry the request multiple times. You might retry stack set
-- operation requests to ensure that AWS CloudFormation successfully
-- received them.
--
-- If you don\'t specify an operation ID, the SDK generates one
-- automatically.
updateStackInstances_operationId :: Lens.Lens' UpdateStackInstances (Core.Maybe Core.Text)
updateStackInstances_operationId = Lens.lens (\UpdateStackInstances' {operationId} -> operationId) (\s@UpdateStackInstances' {} a -> s {operationId = a} :: UpdateStackInstances)

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
--     Your AWS account must be registered as a delegated administrator in
--     the management account. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-orgs-delegated-admin.html Register a delegated administrator>
--     in the /AWS CloudFormation User Guide/.
updateStackInstances_callAs :: Lens.Lens' UpdateStackInstances (Core.Maybe CallAs)
updateStackInstances_callAs = Lens.lens (\UpdateStackInstances' {callAs} -> callAs) (\s@UpdateStackInstances' {} a -> s {callAs = a} :: UpdateStackInstances)

-- | Preferences for how AWS CloudFormation performs this stack set
-- operation.
updateStackInstances_operationPreferences :: Lens.Lens' UpdateStackInstances (Core.Maybe StackSetOperationPreferences)
updateStackInstances_operationPreferences = Lens.lens (\UpdateStackInstances' {operationPreferences} -> operationPreferences) (\s@UpdateStackInstances' {} a -> s {operationPreferences = a} :: UpdateStackInstances)

-- | [Self-managed permissions] The names of one or more AWS accounts for
-- which you want to update parameter values for stack instances. The
-- overridden parameter values will be applied to all stack instances in
-- the specified accounts and Regions.
--
-- You can specify @Accounts@ or @DeploymentTargets@, but not both.
updateStackInstances_accounts :: Lens.Lens' UpdateStackInstances (Core.Maybe [Core.Text])
updateStackInstances_accounts = Lens.lens (\UpdateStackInstances' {accounts} -> accounts) (\s@UpdateStackInstances' {} a -> s {accounts = a} :: UpdateStackInstances) Core.. Lens.mapping Lens._Coerce

-- | The name or unique ID of the stack set associated with the stack
-- instances.
updateStackInstances_stackSetName :: Lens.Lens' UpdateStackInstances Core.Text
updateStackInstances_stackSetName = Lens.lens (\UpdateStackInstances' {stackSetName} -> stackSetName) (\s@UpdateStackInstances' {} a -> s {stackSetName = a} :: UpdateStackInstances)

-- | The names of one or more Regions in which you want to update parameter
-- values for stack instances. The overridden parameter values will be
-- applied to all stack instances in the specified accounts and Regions.
updateStackInstances_regions :: Lens.Lens' UpdateStackInstances [Core.Text]
updateStackInstances_regions = Lens.lens (\UpdateStackInstances' {regions} -> regions) (\s@UpdateStackInstances' {} a -> s {regions = a} :: UpdateStackInstances) Core.. Lens._Coerce

instance Core.AWSRequest UpdateStackInstances where
  type
    AWSResponse UpdateStackInstances =
      UpdateStackInstancesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "UpdateStackInstancesResult"
      ( \s h x ->
          UpdateStackInstancesResponse'
            Core.<$> (x Core..@? "OperationId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateStackInstances

instance Core.NFData UpdateStackInstances

instance Core.ToHeaders UpdateStackInstances where
  toHeaders = Core.const Core.mempty

instance Core.ToPath UpdateStackInstances where
  toPath = Core.const "/"

instance Core.ToQuery UpdateStackInstances where
  toQuery UpdateStackInstances' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("UpdateStackInstances" :: Core.ByteString),
        "Version" Core.=: ("2010-05-15" :: Core.ByteString),
        "ParameterOverrides"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Core.<$> parameterOverrides
            ),
        "DeploymentTargets" Core.=: deploymentTargets,
        "OperationId" Core.=: operationId,
        "CallAs" Core.=: callAs,
        "OperationPreferences" Core.=: operationPreferences,
        "Accounts"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> accounts),
        "StackSetName" Core.=: stackSetName,
        "Regions" Core.=: Core.toQueryList "member" regions
      ]

-- | /See:/ 'newUpdateStackInstancesResponse' smart constructor.
data UpdateStackInstancesResponse = UpdateStackInstancesResponse'
  { -- | The unique identifier for this stack set operation.
    operationId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  UpdateStackInstancesResponse
newUpdateStackInstancesResponse pHttpStatus_ =
  UpdateStackInstancesResponse'
    { operationId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier for this stack set operation.
updateStackInstancesResponse_operationId :: Lens.Lens' UpdateStackInstancesResponse (Core.Maybe Core.Text)
updateStackInstancesResponse_operationId = Lens.lens (\UpdateStackInstancesResponse' {operationId} -> operationId) (\s@UpdateStackInstancesResponse' {} a -> s {operationId = a} :: UpdateStackInstancesResponse)

-- | The response's http status code.
updateStackInstancesResponse_httpStatus :: Lens.Lens' UpdateStackInstancesResponse Core.Int
updateStackInstancesResponse_httpStatus = Lens.lens (\UpdateStackInstancesResponse' {httpStatus} -> httpStatus) (\s@UpdateStackInstancesResponse' {} a -> s {httpStatus = a} :: UpdateStackInstancesResponse)

instance Core.NFData UpdateStackInstancesResponse
