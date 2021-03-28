{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.CreateStackInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates stack instances for the specified accounts, within the specified Regions. A stack instance refers to a stack in a specific account and Region. You must specify at least one value for either @Accounts@ or @DeploymentTargets@ , and you must specify at least one value for @Regions@ .
module Network.AWS.CloudFormation.CreateStackInstances
    (
    -- * Creating a request
      CreateStackInstances (..)
    , mkCreateStackInstances
    -- ** Request lenses
    , csiStackSetName
    , csiRegions
    , csiAccounts
    , csiDeploymentTargets
    , csiOperationId
    , csiOperationPreferences
    , csiParameterOverrides

    -- * Destructuring the response
    , CreateStackInstancesResponse (..)
    , mkCreateStackInstancesResponse
    -- ** Response lenses
    , csirrsOperationId
    , csirrsResponseStatus
    ) where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateStackInstances' smart constructor.
data CreateStackInstances = CreateStackInstances'
  { stackSetName :: Types.StackSetName
    -- ^ The name or unique ID of the stack set that you want to create stack instances from.
  , regions :: [Types.Region]
    -- ^ The names of one or more Regions where you want to create stack instances using the specified AWS account(s). 
  , accounts :: Core.Maybe [Types.Account]
    -- ^ [@Self-managed@ permissions] The names of one or more AWS accounts that you want to create stack instances in the specified Region(s) for.
--
-- You can specify @Accounts@ or @DeploymentTargets@ , but not both.
  , deploymentTargets :: Core.Maybe Types.DeploymentTargets
    -- ^ [@Service-managed@ permissions] The AWS Organizations accounts for which to create stack instances in the specified Regions.
--
-- You can specify @Accounts@ or @DeploymentTargets@ , but not both.
  , operationId :: Core.Maybe Types.OperationId
    -- ^ The unique identifier for this stack set operation. 
--
-- The operation ID also functions as an idempotency token, to ensure that AWS CloudFormation performs the stack set operation only once, even if you retry the request multiple times. You might retry stack set operation requests to ensure that AWS CloudFormation successfully received them.
-- If you don't specify an operation ID, the SDK generates one automatically. 
-- Repeating this stack set operation with a new operation ID retries all stack instances whose status is @OUTDATED@ . 
  , operationPreferences :: Core.Maybe Types.StackSetOperationPreferences
    -- ^ Preferences for how AWS CloudFormation performs this stack set operation.
  , parameterOverrides :: Core.Maybe [Types.Parameter]
    -- ^ A list of stack set parameters whose values you want to override in the selected stack instances.
--
-- Any overridden parameter values will be applied to all stack instances in the specified accounts and Regions. When specifying parameters and their values, be aware of how AWS CloudFormation sets parameter values during stack instance operations:
--
--     * To override the current value for a parameter, include the parameter and specify its value.
--
--
--     * To leave a parameter set to its present value, you can do one of the following:
--
--     * Do not include the parameter in the list.
--
--
--     * Include the parameter and specify @UsePreviousValue@ as @true@ . (You cannot specify both a value and set @UsePreviousValue@ to @true@ .)
--
--
--
--
--     * To set all overridden parameter back to the values specified in the stack set, specify a parameter list but do not include any parameters.
--
--
--     * To leave all parameters set to their present values, do not specify this property at all.
--
--
-- During stack set updates, any parameter values overridden for a stack instance are not updated, but retain their overridden value.
-- You can only override the parameter /values/ that are specified in the stack set; to add or delete a parameter itself, use <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_UpdateStackSet.html UpdateStackSet> to update the stack set template.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateStackInstances' value with any optional fields omitted.
mkCreateStackInstances
    :: Types.StackSetName -- ^ 'stackSetName'
    -> CreateStackInstances
mkCreateStackInstances stackSetName
  = CreateStackInstances'{stackSetName, regions = Core.mempty,
                          accounts = Core.Nothing, deploymentTargets = Core.Nothing,
                          operationId = Core.Nothing, operationPreferences = Core.Nothing,
                          parameterOverrides = Core.Nothing}

-- | The name or unique ID of the stack set that you want to create stack instances from.
--
-- /Note:/ Consider using 'stackSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csiStackSetName :: Lens.Lens' CreateStackInstances Types.StackSetName
csiStackSetName = Lens.field @"stackSetName"
{-# INLINEABLE csiStackSetName #-}
{-# DEPRECATED stackSetName "Use generic-lens or generic-optics with 'stackSetName' instead"  #-}

-- | The names of one or more Regions where you want to create stack instances using the specified AWS account(s). 
--
-- /Note:/ Consider using 'regions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csiRegions :: Lens.Lens' CreateStackInstances [Types.Region]
csiRegions = Lens.field @"regions"
{-# INLINEABLE csiRegions #-}
{-# DEPRECATED regions "Use generic-lens or generic-optics with 'regions' instead"  #-}

-- | [@Self-managed@ permissions] The names of one or more AWS accounts that you want to create stack instances in the specified Region(s) for.
--
-- You can specify @Accounts@ or @DeploymentTargets@ , but not both.
--
-- /Note:/ Consider using 'accounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csiAccounts :: Lens.Lens' CreateStackInstances (Core.Maybe [Types.Account])
csiAccounts = Lens.field @"accounts"
{-# INLINEABLE csiAccounts #-}
{-# DEPRECATED accounts "Use generic-lens or generic-optics with 'accounts' instead"  #-}

-- | [@Service-managed@ permissions] The AWS Organizations accounts for which to create stack instances in the specified Regions.
--
-- You can specify @Accounts@ or @DeploymentTargets@ , but not both.
--
-- /Note:/ Consider using 'deploymentTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csiDeploymentTargets :: Lens.Lens' CreateStackInstances (Core.Maybe Types.DeploymentTargets)
csiDeploymentTargets = Lens.field @"deploymentTargets"
{-# INLINEABLE csiDeploymentTargets #-}
{-# DEPRECATED deploymentTargets "Use generic-lens or generic-optics with 'deploymentTargets' instead"  #-}

-- | The unique identifier for this stack set operation. 
--
-- The operation ID also functions as an idempotency token, to ensure that AWS CloudFormation performs the stack set operation only once, even if you retry the request multiple times. You might retry stack set operation requests to ensure that AWS CloudFormation successfully received them.
-- If you don't specify an operation ID, the SDK generates one automatically. 
-- Repeating this stack set operation with a new operation ID retries all stack instances whose status is @OUTDATED@ . 
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csiOperationId :: Lens.Lens' CreateStackInstances (Core.Maybe Types.OperationId)
csiOperationId = Lens.field @"operationId"
{-# INLINEABLE csiOperationId #-}
{-# DEPRECATED operationId "Use generic-lens or generic-optics with 'operationId' instead"  #-}

-- | Preferences for how AWS CloudFormation performs this stack set operation.
--
-- /Note:/ Consider using 'operationPreferences' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csiOperationPreferences :: Lens.Lens' CreateStackInstances (Core.Maybe Types.StackSetOperationPreferences)
csiOperationPreferences = Lens.field @"operationPreferences"
{-# INLINEABLE csiOperationPreferences #-}
{-# DEPRECATED operationPreferences "Use generic-lens or generic-optics with 'operationPreferences' instead"  #-}

-- | A list of stack set parameters whose values you want to override in the selected stack instances.
--
-- Any overridden parameter values will be applied to all stack instances in the specified accounts and Regions. When specifying parameters and their values, be aware of how AWS CloudFormation sets parameter values during stack instance operations:
--
--     * To override the current value for a parameter, include the parameter and specify its value.
--
--
--     * To leave a parameter set to its present value, you can do one of the following:
--
--     * Do not include the parameter in the list.
--
--
--     * Include the parameter and specify @UsePreviousValue@ as @true@ . (You cannot specify both a value and set @UsePreviousValue@ to @true@ .)
--
--
--
--
--     * To set all overridden parameter back to the values specified in the stack set, specify a parameter list but do not include any parameters.
--
--
--     * To leave all parameters set to their present values, do not specify this property at all.
--
--
-- During stack set updates, any parameter values overridden for a stack instance are not updated, but retain their overridden value.
-- You can only override the parameter /values/ that are specified in the stack set; to add or delete a parameter itself, use <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_UpdateStackSet.html UpdateStackSet> to update the stack set template.
--
-- /Note:/ Consider using 'parameterOverrides' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csiParameterOverrides :: Lens.Lens' CreateStackInstances (Core.Maybe [Types.Parameter])
csiParameterOverrides = Lens.field @"parameterOverrides"
{-# INLINEABLE csiParameterOverrides #-}
{-# DEPRECATED parameterOverrides "Use generic-lens or generic-optics with 'parameterOverrides' instead"  #-}

instance Core.ToQuery CreateStackInstances where
        toQuery CreateStackInstances{..}
          = Core.toQueryPair "Action" ("CreateStackInstances" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-15" :: Core.Text)
              Core.<> Core.toQueryPair "StackSetName" stackSetName
              Core.<>
              Core.toQueryPair "Regions" (Core.toQueryList "member" regions)
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
              Core.<>
              Core.toQueryPair "ParameterOverrides"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   parameterOverrides)

instance Core.ToHeaders CreateStackInstances where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateStackInstances where
        type Rs CreateStackInstances = CreateStackInstancesResponse
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
          = Response.receiveXMLWrapper "CreateStackInstancesResult"
              (\ s h x ->
                 CreateStackInstancesResponse' Core.<$>
                   (x Core..@? "OperationId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateStackInstancesResponse' smart constructor.
data CreateStackInstancesResponse = CreateStackInstancesResponse'
  { operationId :: Core.Maybe Types.OperationId
    -- ^ The unique identifier for this stack set operation.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateStackInstancesResponse' value with any optional fields omitted.
mkCreateStackInstancesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateStackInstancesResponse
mkCreateStackInstancesResponse responseStatus
  = CreateStackInstancesResponse'{operationId = Core.Nothing,
                                  responseStatus}

-- | The unique identifier for this stack set operation.
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csirrsOperationId :: Lens.Lens' CreateStackInstancesResponse (Core.Maybe Types.OperationId)
csirrsOperationId = Lens.field @"operationId"
{-# INLINEABLE csirrsOperationId #-}
{-# DEPRECATED operationId "Use generic-lens or generic-optics with 'operationId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csirrsResponseStatus :: Lens.Lens' CreateStackInstancesResponse Core.Int
csirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE csirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
