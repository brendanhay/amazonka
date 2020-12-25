{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateStackInstances (..),
    mkCreateStackInstances,

    -- ** Request lenses
    csiStackSetName,
    csiRegions,
    csiAccounts,
    csiDeploymentTargets,
    csiOperationId,
    csiOperationPreferences,
    csiParameterOverrides,

    -- * Destructuring the response
    CreateStackInstancesResponse (..),
    mkCreateStackInstancesResponse,

    -- ** Response lenses
    csirrsOperationId,
    csirrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateStackInstances' smart constructor.
data CreateStackInstances = CreateStackInstances'
  { -- | The name or unique ID of the stack set that you want to create stack instances from.
    stackSetName :: Types.StackSetName,
    -- | The names of one or more Regions where you want to create stack instances using the specified AWS account(s).
    regions :: [Types.Region],
    -- | [@Self-managed@ permissions] The names of one or more AWS accounts that you want to create stack instances in the specified Region(s) for.
    --
    -- You can specify @Accounts@ or @DeploymentTargets@ , but not both.
    accounts :: Core.Maybe [Types.Account],
    -- | [@Service-managed@ permissions] The AWS Organizations accounts for which to create stack instances in the specified Regions.
    --
    -- You can specify @Accounts@ or @DeploymentTargets@ , but not both.
    deploymentTargets :: Core.Maybe Types.DeploymentTargets,
    -- | The unique identifier for this stack set operation.
    --
    -- The operation ID also functions as an idempotency token, to ensure that AWS CloudFormation performs the stack set operation only once, even if you retry the request multiple times. You might retry stack set operation requests to ensure that AWS CloudFormation successfully received them.
    -- If you don't specify an operation ID, the SDK generates one automatically.
    -- Repeating this stack set operation with a new operation ID retries all stack instances whose status is @OUTDATED@ .
    operationId :: Core.Maybe Types.OperationId,
    -- | Preferences for how AWS CloudFormation performs this stack set operation.
    operationPreferences :: Core.Maybe Types.StackSetOperationPreferences,
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
    parameterOverrides :: Core.Maybe [Types.Parameter]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateStackInstances' value with any optional fields omitted.
mkCreateStackInstances ::
  -- | 'stackSetName'
  Types.StackSetName ->
  CreateStackInstances
mkCreateStackInstances stackSetName =
  CreateStackInstances'
    { stackSetName,
      regions = Core.mempty,
      accounts = Core.Nothing,
      deploymentTargets = Core.Nothing,
      operationId = Core.Nothing,
      operationPreferences = Core.Nothing,
      parameterOverrides = Core.Nothing
    }

-- | The name or unique ID of the stack set that you want to create stack instances from.
--
-- /Note:/ Consider using 'stackSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csiStackSetName :: Lens.Lens' CreateStackInstances Types.StackSetName
csiStackSetName = Lens.field @"stackSetName"
{-# DEPRECATED csiStackSetName "Use generic-lens or generic-optics with 'stackSetName' instead." #-}

-- | The names of one or more Regions where you want to create stack instances using the specified AWS account(s).
--
-- /Note:/ Consider using 'regions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csiRegions :: Lens.Lens' CreateStackInstances [Types.Region]
csiRegions = Lens.field @"regions"
{-# DEPRECATED csiRegions "Use generic-lens or generic-optics with 'regions' instead." #-}

-- | [@Self-managed@ permissions] The names of one or more AWS accounts that you want to create stack instances in the specified Region(s) for.
--
-- You can specify @Accounts@ or @DeploymentTargets@ , but not both.
--
-- /Note:/ Consider using 'accounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csiAccounts :: Lens.Lens' CreateStackInstances (Core.Maybe [Types.Account])
csiAccounts = Lens.field @"accounts"
{-# DEPRECATED csiAccounts "Use generic-lens or generic-optics with 'accounts' instead." #-}

-- | [@Service-managed@ permissions] The AWS Organizations accounts for which to create stack instances in the specified Regions.
--
-- You can specify @Accounts@ or @DeploymentTargets@ , but not both.
--
-- /Note:/ Consider using 'deploymentTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csiDeploymentTargets :: Lens.Lens' CreateStackInstances (Core.Maybe Types.DeploymentTargets)
csiDeploymentTargets = Lens.field @"deploymentTargets"
{-# DEPRECATED csiDeploymentTargets "Use generic-lens or generic-optics with 'deploymentTargets' instead." #-}

-- | The unique identifier for this stack set operation.
--
-- The operation ID also functions as an idempotency token, to ensure that AWS CloudFormation performs the stack set operation only once, even if you retry the request multiple times. You might retry stack set operation requests to ensure that AWS CloudFormation successfully received them.
-- If you don't specify an operation ID, the SDK generates one automatically.
-- Repeating this stack set operation with a new operation ID retries all stack instances whose status is @OUTDATED@ .
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csiOperationId :: Lens.Lens' CreateStackInstances (Core.Maybe Types.OperationId)
csiOperationId = Lens.field @"operationId"
{-# DEPRECATED csiOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | Preferences for how AWS CloudFormation performs this stack set operation.
--
-- /Note:/ Consider using 'operationPreferences' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csiOperationPreferences :: Lens.Lens' CreateStackInstances (Core.Maybe Types.StackSetOperationPreferences)
csiOperationPreferences = Lens.field @"operationPreferences"
{-# DEPRECATED csiOperationPreferences "Use generic-lens or generic-optics with 'operationPreferences' instead." #-}

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
{-# DEPRECATED csiParameterOverrides "Use generic-lens or generic-optics with 'parameterOverrides' instead." #-}

instance Core.AWSRequest CreateStackInstances where
  type Rs CreateStackInstances = CreateStackInstancesResponse
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
            ( Core.pure ("Action", "CreateStackInstances")
                Core.<> (Core.pure ("Version", "2010-05-15"))
                Core.<> (Core.toQueryValue "StackSetName" stackSetName)
                Core.<> (Core.toQueryValue "Regions" (Core.toQueryList "member" regions))
                Core.<> ( Core.toQueryValue
                            "Accounts"
                            (Core.toQueryList "member" Core.<$> accounts)
                        )
                Core.<> (Core.toQueryValue "DeploymentTargets" Core.<$> deploymentTargets)
                Core.<> (Core.toQueryValue "OperationId" Core.<$> operationId)
                Core.<> ( Core.toQueryValue "OperationPreferences"
                            Core.<$> operationPreferences
                        )
                Core.<> ( Core.toQueryValue
                            "ParameterOverrides"
                            (Core.toQueryList "member" Core.<$> parameterOverrides)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "CreateStackInstancesResult"
      ( \s h x ->
          CreateStackInstancesResponse'
            Core.<$> (x Core..@? "OperationId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateStackInstancesResponse' smart constructor.
data CreateStackInstancesResponse = CreateStackInstancesResponse'
  { -- | The unique identifier for this stack set operation.
    operationId :: Core.Maybe Types.OperationId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateStackInstancesResponse' value with any optional fields omitted.
mkCreateStackInstancesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateStackInstancesResponse
mkCreateStackInstancesResponse responseStatus =
  CreateStackInstancesResponse'
    { operationId = Core.Nothing,
      responseStatus
    }

-- | The unique identifier for this stack set operation.
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csirrsOperationId :: Lens.Lens' CreateStackInstancesResponse (Core.Maybe Types.OperationId)
csirrsOperationId = Lens.field @"operationId"
{-# DEPRECATED csirrsOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csirrsResponseStatus :: Lens.Lens' CreateStackInstancesResponse Core.Int
csirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED csirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
