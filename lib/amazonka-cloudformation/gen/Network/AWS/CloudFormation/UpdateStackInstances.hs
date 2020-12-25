{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.UpdateStackInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the parameter values for stack instances for the specified accounts, within the specified Regions. A stack instance refers to a stack in a specific account and Region.
--
-- You can only update stack instances in Regions and accounts where they already exist; to create additional stack instances, use <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_CreateStackInstances.html CreateStackInstances> .
-- During stack set updates, any parameters overridden for a stack instance are not updated, but retain their overridden value.
-- You can only update the parameter /values/ that are specified in the stack set; to add or delete a parameter itself, use <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_UpdateStackSet.html UpdateStackSet> to update the stack set template. If you add a parameter to a template, before you can override the parameter value specified in the stack set you must first use <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_UpdateStackSet.html UpdateStackSet> to update all stack instances with the updated template and parameter value specified in the stack set. Once a stack instance has been updated with the new parameter, you can then override the parameter value using @UpdateStackInstances@ .
module Network.AWS.CloudFormation.UpdateStackInstances
  ( -- * Creating a request
    UpdateStackInstances (..),
    mkUpdateStackInstances,

    -- ** Request lenses
    usiStackSetName,
    usiRegions,
    usiAccounts,
    usiDeploymentTargets,
    usiOperationId,
    usiOperationPreferences,
    usiParameterOverrides,

    -- * Destructuring the response
    UpdateStackInstancesResponse (..),
    mkUpdateStackInstancesResponse,

    -- ** Response lenses
    usirrsOperationId,
    usirrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateStackInstances' smart constructor.
data UpdateStackInstances = UpdateStackInstances'
  { -- | The name or unique ID of the stack set associated with the stack instances.
    stackSetName :: Types.StackSetName,
    -- | The names of one or more Regions in which you want to update parameter values for stack instances. The overridden parameter values will be applied to all stack instances in the specified accounts and Regions.
    regions :: [Types.Region],
    -- | [@Self-managed@ permissions] The names of one or more AWS accounts for which you want to update parameter values for stack instances. The overridden parameter values will be applied to all stack instances in the specified accounts and Regions.
    --
    -- You can specify @Accounts@ or @DeploymentTargets@ , but not both.
    accounts :: Core.Maybe [Types.Account],
    -- | [@Service-managed@ permissions] The AWS Organizations accounts for which you want to update parameter values for stack instances. If your update targets OUs, the overridden parameter values only apply to the accounts that are currently in the target OUs and their child OUs. Accounts added to the target OUs and their child OUs in the future won't use the overridden values.
    --
    -- You can specify @Accounts@ or @DeploymentTargets@ , but not both.
    deploymentTargets :: Core.Maybe Types.DeploymentTargets,
    -- | The unique identifier for this stack set operation.
    --
    -- The operation ID also functions as an idempotency token, to ensure that AWS CloudFormation performs the stack set operation only once, even if you retry the request multiple times. You might retry stack set operation requests to ensure that AWS CloudFormation successfully received them.
    -- If you don't specify an operation ID, the SDK generates one automatically.
    operationId :: Core.Maybe Types.OperationId,
    -- | Preferences for how AWS CloudFormation performs this stack set operation.
    operationPreferences :: Core.Maybe Types.StackSetOperationPreferences,
    -- | A list of input parameters whose values you want to update for the specified stack instances.
    --
    -- Any overridden parameter values will be applied to all stack instances in the specified accounts and Regions. When specifying parameters and their values, be aware of how AWS CloudFormation sets parameter values during stack instance update operations:
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
    -- You can only override the parameter /values/ that are specified in the stack set; to add or delete a parameter itself, use @UpdateStackSet@ to update the stack set template. If you add a parameter to a template, before you can override the parameter value specified in the stack set you must first use <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_UpdateStackSet.html UpdateStackSet> to update all stack instances with the updated template and parameter value specified in the stack set. Once a stack instance has been updated with the new parameter, you can then override the parameter value using @UpdateStackInstances@ .
    parameterOverrides :: Core.Maybe [Types.Parameter]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateStackInstances' value with any optional fields omitted.
mkUpdateStackInstances ::
  -- | 'stackSetName'
  Types.StackSetName ->
  UpdateStackInstances
mkUpdateStackInstances stackSetName =
  UpdateStackInstances'
    { stackSetName,
      regions = Core.mempty,
      accounts = Core.Nothing,
      deploymentTargets = Core.Nothing,
      operationId = Core.Nothing,
      operationPreferences = Core.Nothing,
      parameterOverrides = Core.Nothing
    }

-- | The name or unique ID of the stack set associated with the stack instances.
--
-- /Note:/ Consider using 'stackSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usiStackSetName :: Lens.Lens' UpdateStackInstances Types.StackSetName
usiStackSetName = Lens.field @"stackSetName"
{-# DEPRECATED usiStackSetName "Use generic-lens or generic-optics with 'stackSetName' instead." #-}

-- | The names of one or more Regions in which you want to update parameter values for stack instances. The overridden parameter values will be applied to all stack instances in the specified accounts and Regions.
--
-- /Note:/ Consider using 'regions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usiRegions :: Lens.Lens' UpdateStackInstances [Types.Region]
usiRegions = Lens.field @"regions"
{-# DEPRECATED usiRegions "Use generic-lens or generic-optics with 'regions' instead." #-}

-- | [@Self-managed@ permissions] The names of one or more AWS accounts for which you want to update parameter values for stack instances. The overridden parameter values will be applied to all stack instances in the specified accounts and Regions.
--
-- You can specify @Accounts@ or @DeploymentTargets@ , but not both.
--
-- /Note:/ Consider using 'accounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usiAccounts :: Lens.Lens' UpdateStackInstances (Core.Maybe [Types.Account])
usiAccounts = Lens.field @"accounts"
{-# DEPRECATED usiAccounts "Use generic-lens or generic-optics with 'accounts' instead." #-}

-- | [@Service-managed@ permissions] The AWS Organizations accounts for which you want to update parameter values for stack instances. If your update targets OUs, the overridden parameter values only apply to the accounts that are currently in the target OUs and their child OUs. Accounts added to the target OUs and their child OUs in the future won't use the overridden values.
--
-- You can specify @Accounts@ or @DeploymentTargets@ , but not both.
--
-- /Note:/ Consider using 'deploymentTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usiDeploymentTargets :: Lens.Lens' UpdateStackInstances (Core.Maybe Types.DeploymentTargets)
usiDeploymentTargets = Lens.field @"deploymentTargets"
{-# DEPRECATED usiDeploymentTargets "Use generic-lens or generic-optics with 'deploymentTargets' instead." #-}

-- | The unique identifier for this stack set operation.
--
-- The operation ID also functions as an idempotency token, to ensure that AWS CloudFormation performs the stack set operation only once, even if you retry the request multiple times. You might retry stack set operation requests to ensure that AWS CloudFormation successfully received them.
-- If you don't specify an operation ID, the SDK generates one automatically.
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usiOperationId :: Lens.Lens' UpdateStackInstances (Core.Maybe Types.OperationId)
usiOperationId = Lens.field @"operationId"
{-# DEPRECATED usiOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | Preferences for how AWS CloudFormation performs this stack set operation.
--
-- /Note:/ Consider using 'operationPreferences' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usiOperationPreferences :: Lens.Lens' UpdateStackInstances (Core.Maybe Types.StackSetOperationPreferences)
usiOperationPreferences = Lens.field @"operationPreferences"
{-# DEPRECATED usiOperationPreferences "Use generic-lens or generic-optics with 'operationPreferences' instead." #-}

-- | A list of input parameters whose values you want to update for the specified stack instances.
--
-- Any overridden parameter values will be applied to all stack instances in the specified accounts and Regions. When specifying parameters and their values, be aware of how AWS CloudFormation sets parameter values during stack instance update operations:
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
-- You can only override the parameter /values/ that are specified in the stack set; to add or delete a parameter itself, use @UpdateStackSet@ to update the stack set template. If you add a parameter to a template, before you can override the parameter value specified in the stack set you must first use <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_UpdateStackSet.html UpdateStackSet> to update all stack instances with the updated template and parameter value specified in the stack set. Once a stack instance has been updated with the new parameter, you can then override the parameter value using @UpdateStackInstances@ .
--
-- /Note:/ Consider using 'parameterOverrides' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usiParameterOverrides :: Lens.Lens' UpdateStackInstances (Core.Maybe [Types.Parameter])
usiParameterOverrides = Lens.field @"parameterOverrides"
{-# DEPRECATED usiParameterOverrides "Use generic-lens or generic-optics with 'parameterOverrides' instead." #-}

instance Core.AWSRequest UpdateStackInstances where
  type Rs UpdateStackInstances = UpdateStackInstancesResponse
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
            ( Core.pure ("Action", "UpdateStackInstances")
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
      "UpdateStackInstancesResult"
      ( \s h x ->
          UpdateStackInstancesResponse'
            Core.<$> (x Core..@? "OperationId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateStackInstancesResponse' smart constructor.
data UpdateStackInstancesResponse = UpdateStackInstancesResponse'
  { -- | The unique identifier for this stack set operation.
    operationId :: Core.Maybe Types.OperationId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateStackInstancesResponse' value with any optional fields omitted.
mkUpdateStackInstancesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateStackInstancesResponse
mkUpdateStackInstancesResponse responseStatus =
  UpdateStackInstancesResponse'
    { operationId = Core.Nothing,
      responseStatus
    }

-- | The unique identifier for this stack set operation.
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usirrsOperationId :: Lens.Lens' UpdateStackInstancesResponse (Core.Maybe Types.OperationId)
usirrsOperationId = Lens.field @"operationId"
{-# DEPRECATED usirrsOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usirrsResponseStatus :: Lens.Lens' UpdateStackInstancesResponse Core.Int
usirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED usirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
