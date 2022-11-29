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
-- Module      : Amazonka.CloudFormation.DeleteStackInstances
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes stack instances for the specified accounts, in the specified
-- Amazon Web Services Regions.
module Amazonka.CloudFormation.DeleteStackInstances
  ( -- * Creating a Request
    DeleteStackInstances (..),
    newDeleteStackInstances,

    -- * Request Lenses
    deleteStackInstances_operationPreferences,
    deleteStackInstances_callAs,
    deleteStackInstances_operationId,
    deleteStackInstances_accounts,
    deleteStackInstances_deploymentTargets,
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

import Amazonka.CloudFormation.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteStackInstances' smart constructor.
data DeleteStackInstances = DeleteStackInstances'
  { -- | Preferences for how CloudFormation performs this stack set operation.
    operationPreferences :: Prelude.Maybe StackSetOperationPreferences,
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
    -- | The unique identifier for this stack set operation.
    --
    -- If you don\'t specify an operation ID, the SDK generates one
    -- automatically.
    --
    -- The operation ID also functions as an idempotency token, to ensure that
    -- CloudFormation performs the stack set operation only once, even if you
    -- retry the request multiple times. You can retry stack set operation
    -- requests to ensure that CloudFormation successfully received them.
    --
    -- Repeating this stack set operation with a new operation ID retries all
    -- stack instances whose status is @OUTDATED@.
    operationId :: Prelude.Maybe Prelude.Text,
    -- | [Self-managed permissions] The names of the Amazon Web Services accounts
    -- that you want to delete stack instances for.
    --
    -- You can specify @Accounts@ or @DeploymentTargets@, but not both.
    accounts :: Prelude.Maybe [Prelude.Text],
    -- | [Service-managed permissions] The Organizations accounts from which to
    -- delete stack instances.
    --
    -- You can specify @Accounts@ or @DeploymentTargets@, but not both.
    deploymentTargets :: Prelude.Maybe DeploymentTargets,
    -- | The name or unique ID of the stack set that you want to delete stack
    -- instances for.
    stackSetName :: Prelude.Text,
    -- | The Amazon Web Services Regions where you want to delete stack set
    -- instances.
    regions :: [Prelude.Text],
    -- | Removes the stack instances from the specified stack set, but doesn\'t
    -- delete the stacks. You can\'t reassociate a retained stack or add an
    -- existing, saved stack to a new stack set.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-ops-options Stack set operation options>.
    retainStacks :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteStackInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operationPreferences', 'deleteStackInstances_operationPreferences' - Preferences for how CloudFormation performs this stack set operation.
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
--     Your Amazon Web Services account must be registered as a delegated
--     administrator in the management account. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-orgs-delegated-admin.html Register a delegated administrator>
--     in the /CloudFormation User Guide/.
--
-- 'operationId', 'deleteStackInstances_operationId' - The unique identifier for this stack set operation.
--
-- If you don\'t specify an operation ID, the SDK generates one
-- automatically.
--
-- The operation ID also functions as an idempotency token, to ensure that
-- CloudFormation performs the stack set operation only once, even if you
-- retry the request multiple times. You can retry stack set operation
-- requests to ensure that CloudFormation successfully received them.
--
-- Repeating this stack set operation with a new operation ID retries all
-- stack instances whose status is @OUTDATED@.
--
-- 'accounts', 'deleteStackInstances_accounts' - [Self-managed permissions] The names of the Amazon Web Services accounts
-- that you want to delete stack instances for.
--
-- You can specify @Accounts@ or @DeploymentTargets@, but not both.
--
-- 'deploymentTargets', 'deleteStackInstances_deploymentTargets' - [Service-managed permissions] The Organizations accounts from which to
-- delete stack instances.
--
-- You can specify @Accounts@ or @DeploymentTargets@, but not both.
--
-- 'stackSetName', 'deleteStackInstances_stackSetName' - The name or unique ID of the stack set that you want to delete stack
-- instances for.
--
-- 'regions', 'deleteStackInstances_regions' - The Amazon Web Services Regions where you want to delete stack set
-- instances.
--
-- 'retainStacks', 'deleteStackInstances_retainStacks' - Removes the stack instances from the specified stack set, but doesn\'t
-- delete the stacks. You can\'t reassociate a retained stack or add an
-- existing, saved stack to a new stack set.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-ops-options Stack set operation options>.
newDeleteStackInstances ::
  -- | 'stackSetName'
  Prelude.Text ->
  -- | 'retainStacks'
  Prelude.Bool ->
  DeleteStackInstances
newDeleteStackInstances pStackSetName_ pRetainStacks_ =
  DeleteStackInstances'
    { operationPreferences =
        Prelude.Nothing,
      callAs = Prelude.Nothing,
      operationId = Prelude.Nothing,
      accounts = Prelude.Nothing,
      deploymentTargets = Prelude.Nothing,
      stackSetName = pStackSetName_,
      regions = Prelude.mempty,
      retainStacks = pRetainStacks_
    }

-- | Preferences for how CloudFormation performs this stack set operation.
deleteStackInstances_operationPreferences :: Lens.Lens' DeleteStackInstances (Prelude.Maybe StackSetOperationPreferences)
deleteStackInstances_operationPreferences = Lens.lens (\DeleteStackInstances' {operationPreferences} -> operationPreferences) (\s@DeleteStackInstances' {} a -> s {operationPreferences = a} :: DeleteStackInstances)

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
deleteStackInstances_callAs :: Lens.Lens' DeleteStackInstances (Prelude.Maybe CallAs)
deleteStackInstances_callAs = Lens.lens (\DeleteStackInstances' {callAs} -> callAs) (\s@DeleteStackInstances' {} a -> s {callAs = a} :: DeleteStackInstances)

-- | The unique identifier for this stack set operation.
--
-- If you don\'t specify an operation ID, the SDK generates one
-- automatically.
--
-- The operation ID also functions as an idempotency token, to ensure that
-- CloudFormation performs the stack set operation only once, even if you
-- retry the request multiple times. You can retry stack set operation
-- requests to ensure that CloudFormation successfully received them.
--
-- Repeating this stack set operation with a new operation ID retries all
-- stack instances whose status is @OUTDATED@.
deleteStackInstances_operationId :: Lens.Lens' DeleteStackInstances (Prelude.Maybe Prelude.Text)
deleteStackInstances_operationId = Lens.lens (\DeleteStackInstances' {operationId} -> operationId) (\s@DeleteStackInstances' {} a -> s {operationId = a} :: DeleteStackInstances)

-- | [Self-managed permissions] The names of the Amazon Web Services accounts
-- that you want to delete stack instances for.
--
-- You can specify @Accounts@ or @DeploymentTargets@, but not both.
deleteStackInstances_accounts :: Lens.Lens' DeleteStackInstances (Prelude.Maybe [Prelude.Text])
deleteStackInstances_accounts = Lens.lens (\DeleteStackInstances' {accounts} -> accounts) (\s@DeleteStackInstances' {} a -> s {accounts = a} :: DeleteStackInstances) Prelude.. Lens.mapping Lens.coerced

-- | [Service-managed permissions] The Organizations accounts from which to
-- delete stack instances.
--
-- You can specify @Accounts@ or @DeploymentTargets@, but not both.
deleteStackInstances_deploymentTargets :: Lens.Lens' DeleteStackInstances (Prelude.Maybe DeploymentTargets)
deleteStackInstances_deploymentTargets = Lens.lens (\DeleteStackInstances' {deploymentTargets} -> deploymentTargets) (\s@DeleteStackInstances' {} a -> s {deploymentTargets = a} :: DeleteStackInstances)

-- | The name or unique ID of the stack set that you want to delete stack
-- instances for.
deleteStackInstances_stackSetName :: Lens.Lens' DeleteStackInstances Prelude.Text
deleteStackInstances_stackSetName = Lens.lens (\DeleteStackInstances' {stackSetName} -> stackSetName) (\s@DeleteStackInstances' {} a -> s {stackSetName = a} :: DeleteStackInstances)

-- | The Amazon Web Services Regions where you want to delete stack set
-- instances.
deleteStackInstances_regions :: Lens.Lens' DeleteStackInstances [Prelude.Text]
deleteStackInstances_regions = Lens.lens (\DeleteStackInstances' {regions} -> regions) (\s@DeleteStackInstances' {} a -> s {regions = a} :: DeleteStackInstances) Prelude.. Lens.coerced

-- | Removes the stack instances from the specified stack set, but doesn\'t
-- delete the stacks. You can\'t reassociate a retained stack or add an
-- existing, saved stack to a new stack set.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-ops-options Stack set operation options>.
deleteStackInstances_retainStacks :: Lens.Lens' DeleteStackInstances Prelude.Bool
deleteStackInstances_retainStacks = Lens.lens (\DeleteStackInstances' {retainStacks} -> retainStacks) (\s@DeleteStackInstances' {} a -> s {retainStacks = a} :: DeleteStackInstances)

instance Core.AWSRequest DeleteStackInstances where
  type
    AWSResponse DeleteStackInstances =
      DeleteStackInstancesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeleteStackInstancesResult"
      ( \s h x ->
          DeleteStackInstancesResponse'
            Prelude.<$> (x Core..@? "OperationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteStackInstances where
  hashWithSalt _salt DeleteStackInstances' {..} =
    _salt `Prelude.hashWithSalt` operationPreferences
      `Prelude.hashWithSalt` callAs
      `Prelude.hashWithSalt` operationId
      `Prelude.hashWithSalt` accounts
      `Prelude.hashWithSalt` deploymentTargets
      `Prelude.hashWithSalt` stackSetName
      `Prelude.hashWithSalt` regions
      `Prelude.hashWithSalt` retainStacks

instance Prelude.NFData DeleteStackInstances where
  rnf DeleteStackInstances' {..} =
    Prelude.rnf operationPreferences
      `Prelude.seq` Prelude.rnf callAs
      `Prelude.seq` Prelude.rnf operationId
      `Prelude.seq` Prelude.rnf accounts
      `Prelude.seq` Prelude.rnf deploymentTargets
      `Prelude.seq` Prelude.rnf stackSetName
      `Prelude.seq` Prelude.rnf regions
      `Prelude.seq` Prelude.rnf retainStacks

instance Core.ToHeaders DeleteStackInstances where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteStackInstances where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteStackInstances where
  toQuery DeleteStackInstances' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DeleteStackInstances" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-05-15" :: Prelude.ByteString),
        "OperationPreferences" Core.=: operationPreferences,
        "CallAs" Core.=: callAs,
        "OperationId" Core.=: operationId,
        "Accounts"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> accounts),
        "DeploymentTargets" Core.=: deploymentTargets,
        "StackSetName" Core.=: stackSetName,
        "Regions" Core.=: Core.toQueryList "member" regions,
        "RetainStacks" Core.=: retainStacks
      ]

-- | /See:/ 'newDeleteStackInstancesResponse' smart constructor.
data DeleteStackInstancesResponse = DeleteStackInstancesResponse'
  { -- | The unique identifier for this stack set operation.
    operationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteStackInstancesResponse
newDeleteStackInstancesResponse pHttpStatus_ =
  DeleteStackInstancesResponse'
    { operationId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier for this stack set operation.
deleteStackInstancesResponse_operationId :: Lens.Lens' DeleteStackInstancesResponse (Prelude.Maybe Prelude.Text)
deleteStackInstancesResponse_operationId = Lens.lens (\DeleteStackInstancesResponse' {operationId} -> operationId) (\s@DeleteStackInstancesResponse' {} a -> s {operationId = a} :: DeleteStackInstancesResponse)

-- | The response's http status code.
deleteStackInstancesResponse_httpStatus :: Lens.Lens' DeleteStackInstancesResponse Prelude.Int
deleteStackInstancesResponse_httpStatus = Lens.lens (\DeleteStackInstancesResponse' {httpStatus} -> httpStatus) (\s@DeleteStackInstancesResponse' {} a -> s {httpStatus = a} :: DeleteStackInstancesResponse)

instance Prelude.NFData DeleteStackInstancesResponse where
  rnf DeleteStackInstancesResponse' {..} =
    Prelude.rnf operationId
      `Prelude.seq` Prelude.rnf httpStatus
