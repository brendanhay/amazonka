{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.DeleteStackInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes stack instances for the specified accounts, in the specified Regions.
module Network.AWS.CloudFormation.DeleteStackInstances
  ( -- * Creating a request
    DeleteStackInstances (..),
    mkDeleteStackInstances,

    -- ** Request lenses
    dAccounts,
    dRegions,
    dOperationPreferences,
    dOperationId,
    dRetainStacks,
    dDeploymentTargets,
    dStackSetName,

    -- * Destructuring the response
    DeleteStackInstancesResponse (..),
    mkDeleteStackInstancesResponse,

    -- ** Response lenses
    dsisrsOperationId,
    dsisrsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteStackInstances' smart constructor.
data DeleteStackInstances = DeleteStackInstances'
  { -- | [@Self-managed@ permissions] The names of the AWS accounts that you want to delete stack instances for.
    --
    -- You can specify @Accounts@ or @DeploymentTargets@ , but not both.
    accounts :: Lude.Maybe [Lude.Text],
    -- | The Regions where you want to delete stack set instances.
    regions :: [Lude.Text],
    -- | Preferences for how AWS CloudFormation performs this stack set operation.
    operationPreferences :: Lude.Maybe StackSetOperationPreferences,
    -- | The unique identifier for this stack set operation.
    --
    -- If you don't specify an operation ID, the SDK generates one automatically.
    -- The operation ID also functions as an idempotency token, to ensure that AWS CloudFormation performs the stack set operation only once, even if you retry the request multiple times. You can retry stack set operation requests to ensure that AWS CloudFormation successfully received them.
    -- Repeating this stack set operation with a new operation ID retries all stack instances whose status is @OUTDATED@ .
    operationId :: Lude.Maybe Lude.Text,
    -- | Removes the stack instances from the specified stack set, but doesn't delete the stacks. You can't reassociate a retained stack or add an existing, saved stack to a new stack set.
    --
    -- For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-ops-options Stack set operation options> .
    retainStacks :: Lude.Bool,
    -- | [@Service-managed@ permissions] The AWS Organizations accounts from which to delete stack instances.
    --
    -- You can specify @Accounts@ or @DeploymentTargets@ , but not both.
    deploymentTargets :: Lude.Maybe DeploymentTargets,
    -- | The name or unique ID of the stack set that you want to delete stack instances for.
    stackSetName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteStackInstances' with the minimum fields required to make a request.
--
-- * 'accounts' - [@Self-managed@ permissions] The names of the AWS accounts that you want to delete stack instances for.
--
-- You can specify @Accounts@ or @DeploymentTargets@ , but not both.
-- * 'regions' - The Regions where you want to delete stack set instances.
-- * 'operationPreferences' - Preferences for how AWS CloudFormation performs this stack set operation.
-- * 'operationId' - The unique identifier for this stack set operation.
--
-- If you don't specify an operation ID, the SDK generates one automatically.
-- The operation ID also functions as an idempotency token, to ensure that AWS CloudFormation performs the stack set operation only once, even if you retry the request multiple times. You can retry stack set operation requests to ensure that AWS CloudFormation successfully received them.
-- Repeating this stack set operation with a new operation ID retries all stack instances whose status is @OUTDATED@ .
-- * 'retainStacks' - Removes the stack instances from the specified stack set, but doesn't delete the stacks. You can't reassociate a retained stack or add an existing, saved stack to a new stack set.
--
-- For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-ops-options Stack set operation options> .
-- * 'deploymentTargets' - [@Service-managed@ permissions] The AWS Organizations accounts from which to delete stack instances.
--
-- You can specify @Accounts@ or @DeploymentTargets@ , but not both.
-- * 'stackSetName' - The name or unique ID of the stack set that you want to delete stack instances for.
mkDeleteStackInstances ::
  -- | 'retainStacks'
  Lude.Bool ->
  -- | 'stackSetName'
  Lude.Text ->
  DeleteStackInstances
mkDeleteStackInstances pRetainStacks_ pStackSetName_ =
  DeleteStackInstances'
    { accounts = Lude.Nothing,
      regions = Lude.mempty,
      operationPreferences = Lude.Nothing,
      operationId = Lude.Nothing,
      retainStacks = pRetainStacks_,
      deploymentTargets = Lude.Nothing,
      stackSetName = pStackSetName_
    }

-- | [@Self-managed@ permissions] The names of the AWS accounts that you want to delete stack instances for.
--
-- You can specify @Accounts@ or @DeploymentTargets@ , but not both.
--
-- /Note:/ Consider using 'accounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAccounts :: Lens.Lens' DeleteStackInstances (Lude.Maybe [Lude.Text])
dAccounts = Lens.lens (accounts :: DeleteStackInstances -> Lude.Maybe [Lude.Text]) (\s a -> s {accounts = a} :: DeleteStackInstances)
{-# DEPRECATED dAccounts "Use generic-lens or generic-optics with 'accounts' instead." #-}

-- | The Regions where you want to delete stack set instances.
--
-- /Note:/ Consider using 'regions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRegions :: Lens.Lens' DeleteStackInstances [Lude.Text]
dRegions = Lens.lens (regions :: DeleteStackInstances -> [Lude.Text]) (\s a -> s {regions = a} :: DeleteStackInstances)
{-# DEPRECATED dRegions "Use generic-lens or generic-optics with 'regions' instead." #-}

-- | Preferences for how AWS CloudFormation performs this stack set operation.
--
-- /Note:/ Consider using 'operationPreferences' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dOperationPreferences :: Lens.Lens' DeleteStackInstances (Lude.Maybe StackSetOperationPreferences)
dOperationPreferences = Lens.lens (operationPreferences :: DeleteStackInstances -> Lude.Maybe StackSetOperationPreferences) (\s a -> s {operationPreferences = a} :: DeleteStackInstances)
{-# DEPRECATED dOperationPreferences "Use generic-lens or generic-optics with 'operationPreferences' instead." #-}

-- | The unique identifier for this stack set operation.
--
-- If you don't specify an operation ID, the SDK generates one automatically.
-- The operation ID also functions as an idempotency token, to ensure that AWS CloudFormation performs the stack set operation only once, even if you retry the request multiple times. You can retry stack set operation requests to ensure that AWS CloudFormation successfully received them.
-- Repeating this stack set operation with a new operation ID retries all stack instances whose status is @OUTDATED@ .
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dOperationId :: Lens.Lens' DeleteStackInstances (Lude.Maybe Lude.Text)
dOperationId = Lens.lens (operationId :: DeleteStackInstances -> Lude.Maybe Lude.Text) (\s a -> s {operationId = a} :: DeleteStackInstances)
{-# DEPRECATED dOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | Removes the stack instances from the specified stack set, but doesn't delete the stacks. You can't reassociate a retained stack or add an existing, saved stack to a new stack set.
--
-- For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-ops-options Stack set operation options> .
--
-- /Note:/ Consider using 'retainStacks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRetainStacks :: Lens.Lens' DeleteStackInstances Lude.Bool
dRetainStacks = Lens.lens (retainStacks :: DeleteStackInstances -> Lude.Bool) (\s a -> s {retainStacks = a} :: DeleteStackInstances)
{-# DEPRECATED dRetainStacks "Use generic-lens or generic-optics with 'retainStacks' instead." #-}

-- | [@Service-managed@ permissions] The AWS Organizations accounts from which to delete stack instances.
--
-- You can specify @Accounts@ or @DeploymentTargets@ , but not both.
--
-- /Note:/ Consider using 'deploymentTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDeploymentTargets :: Lens.Lens' DeleteStackInstances (Lude.Maybe DeploymentTargets)
dDeploymentTargets = Lens.lens (deploymentTargets :: DeleteStackInstances -> Lude.Maybe DeploymentTargets) (\s a -> s {deploymentTargets = a} :: DeleteStackInstances)
{-# DEPRECATED dDeploymentTargets "Use generic-lens or generic-optics with 'deploymentTargets' instead." #-}

-- | The name or unique ID of the stack set that you want to delete stack instances for.
--
-- /Note:/ Consider using 'stackSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dStackSetName :: Lens.Lens' DeleteStackInstances Lude.Text
dStackSetName = Lens.lens (stackSetName :: DeleteStackInstances -> Lude.Text) (\s a -> s {stackSetName = a} :: DeleteStackInstances)
{-# DEPRECATED dStackSetName "Use generic-lens or generic-optics with 'stackSetName' instead." #-}

instance Lude.AWSRequest DeleteStackInstances where
  type Rs DeleteStackInstances = DeleteStackInstancesResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "DeleteStackInstancesResult"
      ( \s h x ->
          DeleteStackInstancesResponse'
            Lude.<$> (x Lude..@? "OperationId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteStackInstances where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteStackInstances where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteStackInstances where
  toQuery DeleteStackInstances' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteStackInstances" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "Accounts"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> accounts),
        "Regions" Lude.=: Lude.toQueryList "member" regions,
        "OperationPreferences" Lude.=: operationPreferences,
        "OperationId" Lude.=: operationId,
        "RetainStacks" Lude.=: retainStacks,
        "DeploymentTargets" Lude.=: deploymentTargets,
        "StackSetName" Lude.=: stackSetName
      ]

-- | /See:/ 'mkDeleteStackInstancesResponse' smart constructor.
data DeleteStackInstancesResponse = DeleteStackInstancesResponse'
  { -- | The unique identifier for this stack set operation.
    operationId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteStackInstancesResponse' with the minimum fields required to make a request.
--
-- * 'operationId' - The unique identifier for this stack set operation.
-- * 'responseStatus' - The response status code.
mkDeleteStackInstancesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteStackInstancesResponse
mkDeleteStackInstancesResponse pResponseStatus_ =
  DeleteStackInstancesResponse'
    { operationId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique identifier for this stack set operation.
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsisrsOperationId :: Lens.Lens' DeleteStackInstancesResponse (Lude.Maybe Lude.Text)
dsisrsOperationId = Lens.lens (operationId :: DeleteStackInstancesResponse -> Lude.Maybe Lude.Text) (\s a -> s {operationId = a} :: DeleteStackInstancesResponse)
{-# DEPRECATED dsisrsOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsisrsResponseStatus :: Lens.Lens' DeleteStackInstancesResponse Lude.Int
dsisrsResponseStatus = Lens.lens (responseStatus :: DeleteStackInstancesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteStackInstancesResponse)
{-# DEPRECATED dsisrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
