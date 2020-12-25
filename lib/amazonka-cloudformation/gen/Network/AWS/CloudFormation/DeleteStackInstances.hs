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
    dsiStackSetName,
    dsiRegions,
    dsiRetainStacks,
    dsiAccounts,
    dsiDeploymentTargets,
    dsiOperationId,
    dsiOperationPreferences,

    -- * Destructuring the response
    DeleteStackInstancesResponse (..),
    mkDeleteStackInstancesResponse,

    -- ** Response lenses
    dsirrsOperationId,
    dsirrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteStackInstances' smart constructor.
data DeleteStackInstances = DeleteStackInstances'
  { -- | The name or unique ID of the stack set that you want to delete stack instances for.
    stackSetName :: Types.StackSetName,
    -- | The Regions where you want to delete stack set instances.
    regions :: [Types.Region],
    -- | Removes the stack instances from the specified stack set, but doesn't delete the stacks. You can't reassociate a retained stack or add an existing, saved stack to a new stack set.
    --
    -- For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-ops-options Stack set operation options> .
    retainStacks :: Core.Bool,
    -- | [@Self-managed@ permissions] The names of the AWS accounts that you want to delete stack instances for.
    --
    -- You can specify @Accounts@ or @DeploymentTargets@ , but not both.
    accounts :: Core.Maybe [Types.Account],
    -- | [@Service-managed@ permissions] The AWS Organizations accounts from which to delete stack instances.
    --
    -- You can specify @Accounts@ or @DeploymentTargets@ , but not both.
    deploymentTargets :: Core.Maybe Types.DeploymentTargets,
    -- | The unique identifier for this stack set operation.
    --
    -- If you don't specify an operation ID, the SDK generates one automatically.
    -- The operation ID also functions as an idempotency token, to ensure that AWS CloudFormation performs the stack set operation only once, even if you retry the request multiple times. You can retry stack set operation requests to ensure that AWS CloudFormation successfully received them.
    -- Repeating this stack set operation with a new operation ID retries all stack instances whose status is @OUTDATED@ .
    operationId :: Core.Maybe Types.OperationId,
    -- | Preferences for how AWS CloudFormation performs this stack set operation.
    operationPreferences :: Core.Maybe Types.StackSetOperationPreferences
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteStackInstances' value with any optional fields omitted.
mkDeleteStackInstances ::
  -- | 'stackSetName'
  Types.StackSetName ->
  -- | 'retainStacks'
  Core.Bool ->
  DeleteStackInstances
mkDeleteStackInstances stackSetName retainStacks =
  DeleteStackInstances'
    { stackSetName,
      regions = Core.mempty,
      retainStacks,
      accounts = Core.Nothing,
      deploymentTargets = Core.Nothing,
      operationId = Core.Nothing,
      operationPreferences = Core.Nothing
    }

-- | The name or unique ID of the stack set that you want to delete stack instances for.
--
-- /Note:/ Consider using 'stackSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiStackSetName :: Lens.Lens' DeleteStackInstances Types.StackSetName
dsiStackSetName = Lens.field @"stackSetName"
{-# DEPRECATED dsiStackSetName "Use generic-lens or generic-optics with 'stackSetName' instead." #-}

-- | The Regions where you want to delete stack set instances.
--
-- /Note:/ Consider using 'regions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiRegions :: Lens.Lens' DeleteStackInstances [Types.Region]
dsiRegions = Lens.field @"regions"
{-# DEPRECATED dsiRegions "Use generic-lens or generic-optics with 'regions' instead." #-}

-- | Removes the stack instances from the specified stack set, but doesn't delete the stacks. You can't reassociate a retained stack or add an existing, saved stack to a new stack set.
--
-- For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-ops-options Stack set operation options> .
--
-- /Note:/ Consider using 'retainStacks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiRetainStacks :: Lens.Lens' DeleteStackInstances Core.Bool
dsiRetainStacks = Lens.field @"retainStacks"
{-# DEPRECATED dsiRetainStacks "Use generic-lens or generic-optics with 'retainStacks' instead." #-}

-- | [@Self-managed@ permissions] The names of the AWS accounts that you want to delete stack instances for.
--
-- You can specify @Accounts@ or @DeploymentTargets@ , but not both.
--
-- /Note:/ Consider using 'accounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiAccounts :: Lens.Lens' DeleteStackInstances (Core.Maybe [Types.Account])
dsiAccounts = Lens.field @"accounts"
{-# DEPRECATED dsiAccounts "Use generic-lens or generic-optics with 'accounts' instead." #-}

-- | [@Service-managed@ permissions] The AWS Organizations accounts from which to delete stack instances.
--
-- You can specify @Accounts@ or @DeploymentTargets@ , but not both.
--
-- /Note:/ Consider using 'deploymentTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiDeploymentTargets :: Lens.Lens' DeleteStackInstances (Core.Maybe Types.DeploymentTargets)
dsiDeploymentTargets = Lens.field @"deploymentTargets"
{-# DEPRECATED dsiDeploymentTargets "Use generic-lens or generic-optics with 'deploymentTargets' instead." #-}

-- | The unique identifier for this stack set operation.
--
-- If you don't specify an operation ID, the SDK generates one automatically.
-- The operation ID also functions as an idempotency token, to ensure that AWS CloudFormation performs the stack set operation only once, even if you retry the request multiple times. You can retry stack set operation requests to ensure that AWS CloudFormation successfully received them.
-- Repeating this stack set operation with a new operation ID retries all stack instances whose status is @OUTDATED@ .
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiOperationId :: Lens.Lens' DeleteStackInstances (Core.Maybe Types.OperationId)
dsiOperationId = Lens.field @"operationId"
{-# DEPRECATED dsiOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | Preferences for how AWS CloudFormation performs this stack set operation.
--
-- /Note:/ Consider using 'operationPreferences' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiOperationPreferences :: Lens.Lens' DeleteStackInstances (Core.Maybe Types.StackSetOperationPreferences)
dsiOperationPreferences = Lens.field @"operationPreferences"
{-# DEPRECATED dsiOperationPreferences "Use generic-lens or generic-optics with 'operationPreferences' instead." #-}

instance Core.AWSRequest DeleteStackInstances where
  type Rs DeleteStackInstances = DeleteStackInstancesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DeleteStackInstances")
                Core.<> (Core.pure ("Version", "2010-05-15"))
                Core.<> (Core.toQueryValue "StackSetName" stackSetName)
                Core.<> (Core.toQueryValue "Regions" (Core.toQueryList "member" regions))
                Core.<> (Core.toQueryValue "RetainStacks" retainStacks)
                Core.<> ( Core.toQueryValue
                            "Accounts"
                            (Core.toQueryList "member" Core.<$> accounts)
                        )
                Core.<> (Core.toQueryValue "DeploymentTargets" Core.<$> deploymentTargets)
                Core.<> (Core.toQueryValue "OperationId" Core.<$> operationId)
                Core.<> ( Core.toQueryValue "OperationPreferences"
                            Core.<$> operationPreferences
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "DeleteStackInstancesResult"
      ( \s h x ->
          DeleteStackInstancesResponse'
            Core.<$> (x Core..@? "OperationId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteStackInstancesResponse' smart constructor.
data DeleteStackInstancesResponse = DeleteStackInstancesResponse'
  { -- | The unique identifier for this stack set operation.
    operationId :: Core.Maybe Types.OperationId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteStackInstancesResponse' value with any optional fields omitted.
mkDeleteStackInstancesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteStackInstancesResponse
mkDeleteStackInstancesResponse responseStatus =
  DeleteStackInstancesResponse'
    { operationId = Core.Nothing,
      responseStatus
    }

-- | The unique identifier for this stack set operation.
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsirrsOperationId :: Lens.Lens' DeleteStackInstancesResponse (Core.Maybe Types.OperationId)
dsirrsOperationId = Lens.field @"operationId"
{-# DEPRECATED dsirrsOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsirrsResponseStatus :: Lens.Lens' DeleteStackInstancesResponse Core.Int
dsirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
