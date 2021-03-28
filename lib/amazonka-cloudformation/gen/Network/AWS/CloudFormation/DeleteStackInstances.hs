{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DeleteStackInstances (..)
    , mkDeleteStackInstances
    -- ** Request lenses
    , dsiStackSetName
    , dsiRegions
    , dsiRetainStacks
    , dsiAccounts
    , dsiDeploymentTargets
    , dsiOperationId
    , dsiOperationPreferences

    -- * Destructuring the response
    , DeleteStackInstancesResponse (..)
    , mkDeleteStackInstancesResponse
    -- ** Response lenses
    , dsirrsOperationId
    , dsirrsResponseStatus
    ) where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteStackInstances' smart constructor.
data DeleteStackInstances = DeleteStackInstances'
  { stackSetName :: Types.StackSetName
    -- ^ The name or unique ID of the stack set that you want to delete stack instances for.
  , regions :: [Types.Region]
    -- ^ The Regions where you want to delete stack set instances. 
  , retainStacks :: Core.Bool
    -- ^ Removes the stack instances from the specified stack set, but doesn't delete the stacks. You can't reassociate a retained stack or add an existing, saved stack to a new stack set.
--
-- For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-ops-options Stack set operation options> .
  , accounts :: Core.Maybe [Types.Account]
    -- ^ [@Self-managed@ permissions] The names of the AWS accounts that you want to delete stack instances for.
--
-- You can specify @Accounts@ or @DeploymentTargets@ , but not both.
  , deploymentTargets :: Core.Maybe Types.DeploymentTargets
    -- ^ [@Service-managed@ permissions] The AWS Organizations accounts from which to delete stack instances.
--
-- You can specify @Accounts@ or @DeploymentTargets@ , but not both.
  , operationId :: Core.Maybe Types.OperationId
    -- ^ The unique identifier for this stack set operation. 
--
-- If you don't specify an operation ID, the SDK generates one automatically. 
-- The operation ID also functions as an idempotency token, to ensure that AWS CloudFormation performs the stack set operation only once, even if you retry the request multiple times. You can retry stack set operation requests to ensure that AWS CloudFormation successfully received them.
-- Repeating this stack set operation with a new operation ID retries all stack instances whose status is @OUTDATED@ . 
  , operationPreferences :: Core.Maybe Types.StackSetOperationPreferences
    -- ^ Preferences for how AWS CloudFormation performs this stack set operation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteStackInstances' value with any optional fields omitted.
mkDeleteStackInstances
    :: Types.StackSetName -- ^ 'stackSetName'
    -> Core.Bool -- ^ 'retainStacks'
    -> DeleteStackInstances
mkDeleteStackInstances stackSetName retainStacks
  = DeleteStackInstances'{stackSetName, regions = Core.mempty,
                          retainStacks, accounts = Core.Nothing,
                          deploymentTargets = Core.Nothing, operationId = Core.Nothing,
                          operationPreferences = Core.Nothing}

-- | The name or unique ID of the stack set that you want to delete stack instances for.
--
-- /Note:/ Consider using 'stackSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiStackSetName :: Lens.Lens' DeleteStackInstances Types.StackSetName
dsiStackSetName = Lens.field @"stackSetName"
{-# INLINEABLE dsiStackSetName #-}
{-# DEPRECATED stackSetName "Use generic-lens or generic-optics with 'stackSetName' instead"  #-}

-- | The Regions where you want to delete stack set instances. 
--
-- /Note:/ Consider using 'regions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiRegions :: Lens.Lens' DeleteStackInstances [Types.Region]
dsiRegions = Lens.field @"regions"
{-# INLINEABLE dsiRegions #-}
{-# DEPRECATED regions "Use generic-lens or generic-optics with 'regions' instead"  #-}

-- | Removes the stack instances from the specified stack set, but doesn't delete the stacks. You can't reassociate a retained stack or add an existing, saved stack to a new stack set.
--
-- For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-ops-options Stack set operation options> .
--
-- /Note:/ Consider using 'retainStacks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiRetainStacks :: Lens.Lens' DeleteStackInstances Core.Bool
dsiRetainStacks = Lens.field @"retainStacks"
{-# INLINEABLE dsiRetainStacks #-}
{-# DEPRECATED retainStacks "Use generic-lens or generic-optics with 'retainStacks' instead"  #-}

-- | [@Self-managed@ permissions] The names of the AWS accounts that you want to delete stack instances for.
--
-- You can specify @Accounts@ or @DeploymentTargets@ , but not both.
--
-- /Note:/ Consider using 'accounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiAccounts :: Lens.Lens' DeleteStackInstances (Core.Maybe [Types.Account])
dsiAccounts = Lens.field @"accounts"
{-# INLINEABLE dsiAccounts #-}
{-# DEPRECATED accounts "Use generic-lens or generic-optics with 'accounts' instead"  #-}

-- | [@Service-managed@ permissions] The AWS Organizations accounts from which to delete stack instances.
--
-- You can specify @Accounts@ or @DeploymentTargets@ , but not both.
--
-- /Note:/ Consider using 'deploymentTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiDeploymentTargets :: Lens.Lens' DeleteStackInstances (Core.Maybe Types.DeploymentTargets)
dsiDeploymentTargets = Lens.field @"deploymentTargets"
{-# INLINEABLE dsiDeploymentTargets #-}
{-# DEPRECATED deploymentTargets "Use generic-lens or generic-optics with 'deploymentTargets' instead"  #-}

-- | The unique identifier for this stack set operation. 
--
-- If you don't specify an operation ID, the SDK generates one automatically. 
-- The operation ID also functions as an idempotency token, to ensure that AWS CloudFormation performs the stack set operation only once, even if you retry the request multiple times. You can retry stack set operation requests to ensure that AWS CloudFormation successfully received them.
-- Repeating this stack set operation with a new operation ID retries all stack instances whose status is @OUTDATED@ . 
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiOperationId :: Lens.Lens' DeleteStackInstances (Core.Maybe Types.OperationId)
dsiOperationId = Lens.field @"operationId"
{-# INLINEABLE dsiOperationId #-}
{-# DEPRECATED operationId "Use generic-lens or generic-optics with 'operationId' instead"  #-}

-- | Preferences for how AWS CloudFormation performs this stack set operation.
--
-- /Note:/ Consider using 'operationPreferences' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiOperationPreferences :: Lens.Lens' DeleteStackInstances (Core.Maybe Types.StackSetOperationPreferences)
dsiOperationPreferences = Lens.field @"operationPreferences"
{-# INLINEABLE dsiOperationPreferences #-}
{-# DEPRECATED operationPreferences "Use generic-lens or generic-optics with 'operationPreferences' instead"  #-}

instance Core.ToQuery DeleteStackInstances where
        toQuery DeleteStackInstances{..}
          = Core.toQueryPair "Action" ("DeleteStackInstances" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-15" :: Core.Text)
              Core.<> Core.toQueryPair "StackSetName" stackSetName
              Core.<>
              Core.toQueryPair "Regions" (Core.toQueryList "member" regions)
              Core.<> Core.toQueryPair "RetainStacks" retainStacks
              Core.<>
              Core.toQueryPair "Accounts"
                (Core.maybe Core.mempty (Core.toQueryList "member") accounts)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DeploymentTargets")
                deploymentTargets
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "OperationId") operationId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "OperationPreferences")
                operationPreferences

instance Core.ToHeaders DeleteStackInstances where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteStackInstances where
        type Rs DeleteStackInstances = DeleteStackInstancesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "DeleteStackInstancesResult"
              (\ s h x ->
                 DeleteStackInstancesResponse' Core.<$>
                   (x Core..@? "OperationId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteStackInstancesResponse' smart constructor.
data DeleteStackInstancesResponse = DeleteStackInstancesResponse'
  { operationId :: Core.Maybe Types.OperationId
    -- ^ The unique identifier for this stack set operation.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteStackInstancesResponse' value with any optional fields omitted.
mkDeleteStackInstancesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteStackInstancesResponse
mkDeleteStackInstancesResponse responseStatus
  = DeleteStackInstancesResponse'{operationId = Core.Nothing,
                                  responseStatus}

-- | The unique identifier for this stack set operation.
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsirrsOperationId :: Lens.Lens' DeleteStackInstancesResponse (Core.Maybe Types.OperationId)
dsirrsOperationId = Lens.field @"operationId"
{-# INLINEABLE dsirrsOperationId #-}
{-# DEPRECATED operationId "Use generic-lens or generic-optics with 'operationId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsirrsResponseStatus :: Lens.Lens' DeleteStackInstancesResponse Core.Int
dsirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
