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
-- Module      : Network.AWS.CloudFormation.DeleteStackInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes stack instances for the specified accounts, in the specified
-- Regions.
module Network.AWS.CloudFormation.DeleteStackInstances
  ( -- * Creating a Request
    DeleteStackInstances (..),
    newDeleteStackInstances,

    -- * Request Lenses
    deleteStackInstances_deploymentTargets,
    deleteStackInstances_operationId,
    deleteStackInstances_callAs,
    deleteStackInstances_operationPreferences,
    deleteStackInstances_accounts,
    deleteStackInstances_stackSetName,
    deleteStackInstances_regions,
    deleteStackInstances_retainStacks,

    -- * Destructuring the Response
    DeleteStackInstancesResponse (..),
    newDeleteStackInstancesResponse,

    -- * Response Lenses
    deleteStackInstancesResponse_operationId,
    deleteStackInstancesResponse_httpStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteStackInstances' smart constructor.
data DeleteStackInstances = DeleteStackInstances'
  { -- | [Service-managed permissions] The AWS Organizations accounts from which
    -- to delete stack instances.
    --
    -- You can specify @Accounts@ or @DeploymentTargets@, but not both.
    deploymentTargets :: Core.Maybe DeploymentTargets,
    -- | The unique identifier for this stack set operation.
    --
    -- If you don\'t specify an operation ID, the SDK generates one
    -- automatically.
    --
    -- The operation ID also functions as an idempotency token, to ensure that
    -- AWS CloudFormation performs the stack set operation only once, even if
    -- you retry the request multiple times. You can retry stack set operation
    -- requests to ensure that AWS CloudFormation successfully received them.
    --
    -- Repeating this stack set operation with a new operation ID retries all
    -- stack instances whose status is @OUTDATED@.
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
    -- | [Self-managed permissions] The names of the AWS accounts that you want
    -- to delete stack instances for.
    --
    -- You can specify @Accounts@ or @DeploymentTargets@, but not both.
    accounts :: Core.Maybe [Core.Text],
    -- | The name or unique ID of the stack set that you want to delete stack
    -- instances for.
    stackSetName :: Core.Text,
    -- | The Regions where you want to delete stack set instances.
    regions :: [Core.Text],
    -- | Removes the stack instances from the specified stack set, but doesn\'t
    -- delete the stacks. You can\'t reassociate a retained stack or add an
    -- existing, saved stack to a new stack set.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-ops-options Stack set operation options>.
    retainStacks :: Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteStackInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentTargets', 'deleteStackInstances_deploymentTargets' - [Service-managed permissions] The AWS Organizations accounts from which
-- to delete stack instances.
--
-- You can specify @Accounts@ or @DeploymentTargets@, but not both.
--
-- 'operationId', 'deleteStackInstances_operationId' - The unique identifier for this stack set operation.
--
-- If you don\'t specify an operation ID, the SDK generates one
-- automatically.
--
-- The operation ID also functions as an idempotency token, to ensure that
-- AWS CloudFormation performs the stack set operation only once, even if
-- you retry the request multiple times. You can retry stack set operation
-- requests to ensure that AWS CloudFormation successfully received them.
--
-- Repeating this stack set operation with a new operation ID retries all
-- stack instances whose status is @OUTDATED@.
--
-- 'callAs', 'deleteStackInstances_callAs' - [Service-managed permissions] Specifies whether you are acting as an
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
-- 'operationPreferences', 'deleteStackInstances_operationPreferences' - Preferences for how AWS CloudFormation performs this stack set
-- operation.
--
-- 'accounts', 'deleteStackInstances_accounts' - [Self-managed permissions] The names of the AWS accounts that you want
-- to delete stack instances for.
--
-- You can specify @Accounts@ or @DeploymentTargets@, but not both.
--
-- 'stackSetName', 'deleteStackInstances_stackSetName' - The name or unique ID of the stack set that you want to delete stack
-- instances for.
--
-- 'regions', 'deleteStackInstances_regions' - The Regions where you want to delete stack set instances.
--
-- 'retainStacks', 'deleteStackInstances_retainStacks' - Removes the stack instances from the specified stack set, but doesn\'t
-- delete the stacks. You can\'t reassociate a retained stack or add an
-- existing, saved stack to a new stack set.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-ops-options Stack set operation options>.
newDeleteStackInstances ::
  -- | 'stackSetName'
  Core.Text ->
  -- | 'retainStacks'
  Core.Bool ->
  DeleteStackInstances
newDeleteStackInstances pStackSetName_ pRetainStacks_ =
  DeleteStackInstances'
    { deploymentTargets =
        Core.Nothing,
      operationId = Core.Nothing,
      callAs = Core.Nothing,
      operationPreferences = Core.Nothing,
      accounts = Core.Nothing,
      stackSetName = pStackSetName_,
      regions = Core.mempty,
      retainStacks = pRetainStacks_
    }

-- | [Service-managed permissions] The AWS Organizations accounts from which
-- to delete stack instances.
--
-- You can specify @Accounts@ or @DeploymentTargets@, but not both.
deleteStackInstances_deploymentTargets :: Lens.Lens' DeleteStackInstances (Core.Maybe DeploymentTargets)
deleteStackInstances_deploymentTargets = Lens.lens (\DeleteStackInstances' {deploymentTargets} -> deploymentTargets) (\s@DeleteStackInstances' {} a -> s {deploymentTargets = a} :: DeleteStackInstances)

-- | The unique identifier for this stack set operation.
--
-- If you don\'t specify an operation ID, the SDK generates one
-- automatically.
--
-- The operation ID also functions as an idempotency token, to ensure that
-- AWS CloudFormation performs the stack set operation only once, even if
-- you retry the request multiple times. You can retry stack set operation
-- requests to ensure that AWS CloudFormation successfully received them.
--
-- Repeating this stack set operation with a new operation ID retries all
-- stack instances whose status is @OUTDATED@.
deleteStackInstances_operationId :: Lens.Lens' DeleteStackInstances (Core.Maybe Core.Text)
deleteStackInstances_operationId = Lens.lens (\DeleteStackInstances' {operationId} -> operationId) (\s@DeleteStackInstances' {} a -> s {operationId = a} :: DeleteStackInstances)

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
deleteStackInstances_callAs :: Lens.Lens' DeleteStackInstances (Core.Maybe CallAs)
deleteStackInstances_callAs = Lens.lens (\DeleteStackInstances' {callAs} -> callAs) (\s@DeleteStackInstances' {} a -> s {callAs = a} :: DeleteStackInstances)

-- | Preferences for how AWS CloudFormation performs this stack set
-- operation.
deleteStackInstances_operationPreferences :: Lens.Lens' DeleteStackInstances (Core.Maybe StackSetOperationPreferences)
deleteStackInstances_operationPreferences = Lens.lens (\DeleteStackInstances' {operationPreferences} -> operationPreferences) (\s@DeleteStackInstances' {} a -> s {operationPreferences = a} :: DeleteStackInstances)

-- | [Self-managed permissions] The names of the AWS accounts that you want
-- to delete stack instances for.
--
-- You can specify @Accounts@ or @DeploymentTargets@, but not both.
deleteStackInstances_accounts :: Lens.Lens' DeleteStackInstances (Core.Maybe [Core.Text])
deleteStackInstances_accounts = Lens.lens (\DeleteStackInstances' {accounts} -> accounts) (\s@DeleteStackInstances' {} a -> s {accounts = a} :: DeleteStackInstances) Core.. Lens.mapping Lens._Coerce

-- | The name or unique ID of the stack set that you want to delete stack
-- instances for.
deleteStackInstances_stackSetName :: Lens.Lens' DeleteStackInstances Core.Text
deleteStackInstances_stackSetName = Lens.lens (\DeleteStackInstances' {stackSetName} -> stackSetName) (\s@DeleteStackInstances' {} a -> s {stackSetName = a} :: DeleteStackInstances)

-- | The Regions where you want to delete stack set instances.
deleteStackInstances_regions :: Lens.Lens' DeleteStackInstances [Core.Text]
deleteStackInstances_regions = Lens.lens (\DeleteStackInstances' {regions} -> regions) (\s@DeleteStackInstances' {} a -> s {regions = a} :: DeleteStackInstances) Core.. Lens._Coerce

-- | Removes the stack instances from the specified stack set, but doesn\'t
-- delete the stacks. You can\'t reassociate a retained stack or add an
-- existing, saved stack to a new stack set.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-ops-options Stack set operation options>.
deleteStackInstances_retainStacks :: Lens.Lens' DeleteStackInstances Core.Bool
deleteStackInstances_retainStacks = Lens.lens (\DeleteStackInstances' {retainStacks} -> retainStacks) (\s@DeleteStackInstances' {} a -> s {retainStacks = a} :: DeleteStackInstances)

instance Core.AWSRequest DeleteStackInstances where
  type
    AWSResponse DeleteStackInstances =
      DeleteStackInstancesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DeleteStackInstancesResult"
      ( \s h x ->
          DeleteStackInstancesResponse'
            Core.<$> (x Core..@? "OperationId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteStackInstances

instance Core.NFData DeleteStackInstances

instance Core.ToHeaders DeleteStackInstances where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteStackInstances where
  toPath = Core.const "/"

instance Core.ToQuery DeleteStackInstances where
  toQuery DeleteStackInstances' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteStackInstances" :: Core.ByteString),
        "Version" Core.=: ("2010-05-15" :: Core.ByteString),
        "DeploymentTargets" Core.=: deploymentTargets,
        "OperationId" Core.=: operationId,
        "CallAs" Core.=: callAs,
        "OperationPreferences" Core.=: operationPreferences,
        "Accounts"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> accounts),
        "StackSetName" Core.=: stackSetName,
        "Regions" Core.=: Core.toQueryList "member" regions,
        "RetainStacks" Core.=: retainStacks
      ]

-- | /See:/ 'newDeleteStackInstancesResponse' smart constructor.
data DeleteStackInstancesResponse = DeleteStackInstancesResponse'
  { -- | The unique identifier for this stack set operation.
    operationId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteStackInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operationId', 'deleteStackInstancesResponse_operationId' - The unique identifier for this stack set operation.
--
-- 'httpStatus', 'deleteStackInstancesResponse_httpStatus' - The response's http status code.
newDeleteStackInstancesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteStackInstancesResponse
newDeleteStackInstancesResponse pHttpStatus_ =
  DeleteStackInstancesResponse'
    { operationId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier for this stack set operation.
deleteStackInstancesResponse_operationId :: Lens.Lens' DeleteStackInstancesResponse (Core.Maybe Core.Text)
deleteStackInstancesResponse_operationId = Lens.lens (\DeleteStackInstancesResponse' {operationId} -> operationId) (\s@DeleteStackInstancesResponse' {} a -> s {operationId = a} :: DeleteStackInstancesResponse)

-- | The response's http status code.
deleteStackInstancesResponse_httpStatus :: Lens.Lens' DeleteStackInstancesResponse Core.Int
deleteStackInstancesResponse_httpStatus = Lens.lens (\DeleteStackInstancesResponse' {httpStatus} -> httpStatus) (\s@DeleteStackInstancesResponse' {} a -> s {httpStatus = a} :: DeleteStackInstancesResponse)

instance Core.NFData DeleteStackInstancesResponse
