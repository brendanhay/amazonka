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
-- Module      : Network.AWS.CloudFormation.CreateStackInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates stack instances for the specified accounts, within the specified
-- Regions. A stack instance refers to a stack in a specific account and
-- Region. You must specify at least one value for either @Accounts@ or
-- @DeploymentTargets@, and you must specify at least one value for
-- @Regions@.
module Network.AWS.CloudFormation.CreateStackInstances
  ( -- * Creating a Request
    CreateStackInstances (..),
    newCreateStackInstances,

    -- * Request Lenses
    createStackInstances_parameterOverrides,
    createStackInstances_deploymentTargets,
    createStackInstances_operationId,
    createStackInstances_callAs,
    createStackInstances_operationPreferences,
    createStackInstances_accounts,
    createStackInstances_stackSetName,
    createStackInstances_regions,

    -- * Destructuring the Response
    CreateStackInstancesResponse (..),
    newCreateStackInstancesResponse,

    -- * Response Lenses
    createStackInstancesResponse_operationId,
    createStackInstancesResponse_httpStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateStackInstances' smart constructor.
data CreateStackInstances = CreateStackInstances'
  { -- | A list of stack set parameters whose values you want to override in the
    -- selected stack instances.
    --
    -- Any overridden parameter values will be applied to all stack instances
    -- in the specified accounts and Regions. When specifying parameters and
    -- their values, be aware of how AWS CloudFormation sets parameter values
    -- during stack instance operations:
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
    -- stack set; to add or delete a parameter itself, use
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_UpdateStackSet.html UpdateStackSet>
    -- to update the stack set template.
    parameterOverrides :: Prelude.Maybe [Parameter],
    -- | [Service-managed permissions] The AWS Organizations accounts for which
    -- to create stack instances in the specified Regions.
    --
    -- You can specify @Accounts@ or @DeploymentTargets@, but not both.
    deploymentTargets :: Prelude.Maybe DeploymentTargets,
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
    --
    -- Repeating this stack set operation with a new operation ID retries all
    -- stack instances whose status is @OUTDATED@.
    operationId :: Prelude.Maybe Prelude.Text,
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
    callAs :: Prelude.Maybe CallAs,
    -- | Preferences for how AWS CloudFormation performs this stack set
    -- operation.
    operationPreferences :: Prelude.Maybe StackSetOperationPreferences,
    -- | [Self-managed permissions] The names of one or more AWS accounts that
    -- you want to create stack instances in the specified Region(s) for.
    --
    -- You can specify @Accounts@ or @DeploymentTargets@, but not both.
    accounts :: Prelude.Maybe [Prelude.Text],
    -- | The name or unique ID of the stack set that you want to create stack
    -- instances from.
    stackSetName :: Prelude.Text,
    -- | The names of one or more Regions where you want to create stack
    -- instances using the specified AWS account(s).
    regions :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateStackInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameterOverrides', 'createStackInstances_parameterOverrides' - A list of stack set parameters whose values you want to override in the
-- selected stack instances.
--
-- Any overridden parameter values will be applied to all stack instances
-- in the specified accounts and Regions. When specifying parameters and
-- their values, be aware of how AWS CloudFormation sets parameter values
-- during stack instance operations:
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
-- stack set; to add or delete a parameter itself, use
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_UpdateStackSet.html UpdateStackSet>
-- to update the stack set template.
--
-- 'deploymentTargets', 'createStackInstances_deploymentTargets' - [Service-managed permissions] The AWS Organizations accounts for which
-- to create stack instances in the specified Regions.
--
-- You can specify @Accounts@ or @DeploymentTargets@, but not both.
--
-- 'operationId', 'createStackInstances_operationId' - The unique identifier for this stack set operation.
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
-- Repeating this stack set operation with a new operation ID retries all
-- stack instances whose status is @OUTDATED@.
--
-- 'callAs', 'createStackInstances_callAs' - [Service-managed permissions] Specifies whether you are acting as an
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
-- 'operationPreferences', 'createStackInstances_operationPreferences' - Preferences for how AWS CloudFormation performs this stack set
-- operation.
--
-- 'accounts', 'createStackInstances_accounts' - [Self-managed permissions] The names of one or more AWS accounts that
-- you want to create stack instances in the specified Region(s) for.
--
-- You can specify @Accounts@ or @DeploymentTargets@, but not both.
--
-- 'stackSetName', 'createStackInstances_stackSetName' - The name or unique ID of the stack set that you want to create stack
-- instances from.
--
-- 'regions', 'createStackInstances_regions' - The names of one or more Regions where you want to create stack
-- instances using the specified AWS account(s).
newCreateStackInstances ::
  -- | 'stackSetName'
  Prelude.Text ->
  CreateStackInstances
newCreateStackInstances pStackSetName_ =
  CreateStackInstances'
    { parameterOverrides =
        Prelude.Nothing,
      deploymentTargets = Prelude.Nothing,
      operationId = Prelude.Nothing,
      callAs = Prelude.Nothing,
      operationPreferences = Prelude.Nothing,
      accounts = Prelude.Nothing,
      stackSetName = pStackSetName_,
      regions = Prelude.mempty
    }

-- | A list of stack set parameters whose values you want to override in the
-- selected stack instances.
--
-- Any overridden parameter values will be applied to all stack instances
-- in the specified accounts and Regions. When specifying parameters and
-- their values, be aware of how AWS CloudFormation sets parameter values
-- during stack instance operations:
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
-- stack set; to add or delete a parameter itself, use
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_UpdateStackSet.html UpdateStackSet>
-- to update the stack set template.
createStackInstances_parameterOverrides :: Lens.Lens' CreateStackInstances (Prelude.Maybe [Parameter])
createStackInstances_parameterOverrides = Lens.lens (\CreateStackInstances' {parameterOverrides} -> parameterOverrides) (\s@CreateStackInstances' {} a -> s {parameterOverrides = a} :: CreateStackInstances) Prelude.. Lens.mapping Prelude._Coerce

-- | [Service-managed permissions] The AWS Organizations accounts for which
-- to create stack instances in the specified Regions.
--
-- You can specify @Accounts@ or @DeploymentTargets@, but not both.
createStackInstances_deploymentTargets :: Lens.Lens' CreateStackInstances (Prelude.Maybe DeploymentTargets)
createStackInstances_deploymentTargets = Lens.lens (\CreateStackInstances' {deploymentTargets} -> deploymentTargets) (\s@CreateStackInstances' {} a -> s {deploymentTargets = a} :: CreateStackInstances)

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
--
-- Repeating this stack set operation with a new operation ID retries all
-- stack instances whose status is @OUTDATED@.
createStackInstances_operationId :: Lens.Lens' CreateStackInstances (Prelude.Maybe Prelude.Text)
createStackInstances_operationId = Lens.lens (\CreateStackInstances' {operationId} -> operationId) (\s@CreateStackInstances' {} a -> s {operationId = a} :: CreateStackInstances)

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
createStackInstances_callAs :: Lens.Lens' CreateStackInstances (Prelude.Maybe CallAs)
createStackInstances_callAs = Lens.lens (\CreateStackInstances' {callAs} -> callAs) (\s@CreateStackInstances' {} a -> s {callAs = a} :: CreateStackInstances)

-- | Preferences for how AWS CloudFormation performs this stack set
-- operation.
createStackInstances_operationPreferences :: Lens.Lens' CreateStackInstances (Prelude.Maybe StackSetOperationPreferences)
createStackInstances_operationPreferences = Lens.lens (\CreateStackInstances' {operationPreferences} -> operationPreferences) (\s@CreateStackInstances' {} a -> s {operationPreferences = a} :: CreateStackInstances)

-- | [Self-managed permissions] The names of one or more AWS accounts that
-- you want to create stack instances in the specified Region(s) for.
--
-- You can specify @Accounts@ or @DeploymentTargets@, but not both.
createStackInstances_accounts :: Lens.Lens' CreateStackInstances (Prelude.Maybe [Prelude.Text])
createStackInstances_accounts = Lens.lens (\CreateStackInstances' {accounts} -> accounts) (\s@CreateStackInstances' {} a -> s {accounts = a} :: CreateStackInstances) Prelude.. Lens.mapping Prelude._Coerce

-- | The name or unique ID of the stack set that you want to create stack
-- instances from.
createStackInstances_stackSetName :: Lens.Lens' CreateStackInstances Prelude.Text
createStackInstances_stackSetName = Lens.lens (\CreateStackInstances' {stackSetName} -> stackSetName) (\s@CreateStackInstances' {} a -> s {stackSetName = a} :: CreateStackInstances)

-- | The names of one or more Regions where you want to create stack
-- instances using the specified AWS account(s).
createStackInstances_regions :: Lens.Lens' CreateStackInstances [Prelude.Text]
createStackInstances_regions = Lens.lens (\CreateStackInstances' {regions} -> regions) (\s@CreateStackInstances' {} a -> s {regions = a} :: CreateStackInstances) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest CreateStackInstances where
  type
    Rs CreateStackInstances =
      CreateStackInstancesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateStackInstancesResult"
      ( \s h x ->
          CreateStackInstancesResponse'
            Prelude.<$> (x Prelude..@? "OperationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateStackInstances

instance Prelude.NFData CreateStackInstances

instance Prelude.ToHeaders CreateStackInstances where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath CreateStackInstances where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateStackInstances where
  toQuery CreateStackInstances' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("CreateStackInstances" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-15" :: Prelude.ByteString),
        "ParameterOverrides"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "member"
                Prelude.<$> parameterOverrides
            ),
        "DeploymentTargets" Prelude.=: deploymentTargets,
        "OperationId" Prelude.=: operationId,
        "CallAs" Prelude.=: callAs,
        "OperationPreferences"
          Prelude.=: operationPreferences,
        "Accounts"
          Prelude.=: Prelude.toQuery
            (Prelude.toQueryList "member" Prelude.<$> accounts),
        "StackSetName" Prelude.=: stackSetName,
        "Regions"
          Prelude.=: Prelude.toQueryList "member" regions
      ]

-- | /See:/ 'newCreateStackInstancesResponse' smart constructor.
data CreateStackInstancesResponse = CreateStackInstancesResponse'
  { -- | The unique identifier for this stack set operation.
    operationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateStackInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operationId', 'createStackInstancesResponse_operationId' - The unique identifier for this stack set operation.
--
-- 'httpStatus', 'createStackInstancesResponse_httpStatus' - The response's http status code.
newCreateStackInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateStackInstancesResponse
newCreateStackInstancesResponse pHttpStatus_ =
  CreateStackInstancesResponse'
    { operationId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier for this stack set operation.
createStackInstancesResponse_operationId :: Lens.Lens' CreateStackInstancesResponse (Prelude.Maybe Prelude.Text)
createStackInstancesResponse_operationId = Lens.lens (\CreateStackInstancesResponse' {operationId} -> operationId) (\s@CreateStackInstancesResponse' {} a -> s {operationId = a} :: CreateStackInstancesResponse)

-- | The response's http status code.
createStackInstancesResponse_httpStatus :: Lens.Lens' CreateStackInstancesResponse Prelude.Int
createStackInstancesResponse_httpStatus = Lens.lens (\CreateStackInstancesResponse' {httpStatus} -> httpStatus) (\s@CreateStackInstancesResponse' {} a -> s {httpStatus = a} :: CreateStackInstancesResponse)

instance Prelude.NFData CreateStackInstancesResponse
