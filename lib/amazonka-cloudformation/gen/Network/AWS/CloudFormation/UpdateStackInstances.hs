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
    usiAccounts,
    usiRegions,
    usiOperationPreferences,
    usiOperationId,
    usiDeploymentTargets,
    usiStackSetName,
    usiParameterOverrides,

    -- * Destructuring the response
    UpdateStackInstancesResponse (..),
    mkUpdateStackInstancesResponse,

    -- ** Response lenses
    usirsOperationId,
    usirsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateStackInstances' smart constructor.
data UpdateStackInstances = UpdateStackInstances'
  { -- | [@Self-managed@ permissions] The names of one or more AWS accounts for which you want to update parameter values for stack instances. The overridden parameter values will be applied to all stack instances in the specified accounts and Regions.
    --
    -- You can specify @Accounts@ or @DeploymentTargets@ , but not both.
    accounts :: Lude.Maybe [Lude.Text],
    -- | The names of one or more Regions in which you want to update parameter values for stack instances. The overridden parameter values will be applied to all stack instances in the specified accounts and Regions.
    regions :: [Lude.Text],
    -- | Preferences for how AWS CloudFormation performs this stack set operation.
    operationPreferences :: Lude.Maybe StackSetOperationPreferences,
    -- | The unique identifier for this stack set operation.
    --
    -- The operation ID also functions as an idempotency token, to ensure that AWS CloudFormation performs the stack set operation only once, even if you retry the request multiple times. You might retry stack set operation requests to ensure that AWS CloudFormation successfully received them.
    -- If you don't specify an operation ID, the SDK generates one automatically.
    operationId :: Lude.Maybe Lude.Text,
    -- | [@Service-managed@ permissions] The AWS Organizations accounts for which you want to update parameter values for stack instances. If your update targets OUs, the overridden parameter values only apply to the accounts that are currently in the target OUs and their child OUs. Accounts added to the target OUs and their child OUs in the future won't use the overridden values.
    --
    -- You can specify @Accounts@ or @DeploymentTargets@ , but not both.
    deploymentTargets :: Lude.Maybe DeploymentTargets,
    -- | The name or unique ID of the stack set associated with the stack instances.
    stackSetName :: Lude.Text,
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
    parameterOverrides :: Lude.Maybe [Parameter]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateStackInstances' with the minimum fields required to make a request.
--
-- * 'accounts' - [@Self-managed@ permissions] The names of one or more AWS accounts for which you want to update parameter values for stack instances. The overridden parameter values will be applied to all stack instances in the specified accounts and Regions.
--
-- You can specify @Accounts@ or @DeploymentTargets@ , but not both.
-- * 'regions' - The names of one or more Regions in which you want to update parameter values for stack instances. The overridden parameter values will be applied to all stack instances in the specified accounts and Regions.
-- * 'operationPreferences' - Preferences for how AWS CloudFormation performs this stack set operation.
-- * 'operationId' - The unique identifier for this stack set operation.
--
-- The operation ID also functions as an idempotency token, to ensure that AWS CloudFormation performs the stack set operation only once, even if you retry the request multiple times. You might retry stack set operation requests to ensure that AWS CloudFormation successfully received them.
-- If you don't specify an operation ID, the SDK generates one automatically.
-- * 'deploymentTargets' - [@Service-managed@ permissions] The AWS Organizations accounts for which you want to update parameter values for stack instances. If your update targets OUs, the overridden parameter values only apply to the accounts that are currently in the target OUs and their child OUs. Accounts added to the target OUs and their child OUs in the future won't use the overridden values.
--
-- You can specify @Accounts@ or @DeploymentTargets@ , but not both.
-- * 'stackSetName' - The name or unique ID of the stack set associated with the stack instances.
-- * 'parameterOverrides' - A list of input parameters whose values you want to update for the specified stack instances.
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
mkUpdateStackInstances ::
  -- | 'stackSetName'
  Lude.Text ->
  UpdateStackInstances
mkUpdateStackInstances pStackSetName_ =
  UpdateStackInstances'
    { accounts = Lude.Nothing,
      regions = Lude.mempty,
      operationPreferences = Lude.Nothing,
      operationId = Lude.Nothing,
      deploymentTargets = Lude.Nothing,
      stackSetName = pStackSetName_,
      parameterOverrides = Lude.Nothing
    }

-- | [@Self-managed@ permissions] The names of one or more AWS accounts for which you want to update parameter values for stack instances. The overridden parameter values will be applied to all stack instances in the specified accounts and Regions.
--
-- You can specify @Accounts@ or @DeploymentTargets@ , but not both.
--
-- /Note:/ Consider using 'accounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usiAccounts :: Lens.Lens' UpdateStackInstances (Lude.Maybe [Lude.Text])
usiAccounts = Lens.lens (accounts :: UpdateStackInstances -> Lude.Maybe [Lude.Text]) (\s a -> s {accounts = a} :: UpdateStackInstances)
{-# DEPRECATED usiAccounts "Use generic-lens or generic-optics with 'accounts' instead." #-}

-- | The names of one or more Regions in which you want to update parameter values for stack instances. The overridden parameter values will be applied to all stack instances in the specified accounts and Regions.
--
-- /Note:/ Consider using 'regions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usiRegions :: Lens.Lens' UpdateStackInstances [Lude.Text]
usiRegions = Lens.lens (regions :: UpdateStackInstances -> [Lude.Text]) (\s a -> s {regions = a} :: UpdateStackInstances)
{-# DEPRECATED usiRegions "Use generic-lens or generic-optics with 'regions' instead." #-}

-- | Preferences for how AWS CloudFormation performs this stack set operation.
--
-- /Note:/ Consider using 'operationPreferences' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usiOperationPreferences :: Lens.Lens' UpdateStackInstances (Lude.Maybe StackSetOperationPreferences)
usiOperationPreferences = Lens.lens (operationPreferences :: UpdateStackInstances -> Lude.Maybe StackSetOperationPreferences) (\s a -> s {operationPreferences = a} :: UpdateStackInstances)
{-# DEPRECATED usiOperationPreferences "Use generic-lens or generic-optics with 'operationPreferences' instead." #-}

-- | The unique identifier for this stack set operation.
--
-- The operation ID also functions as an idempotency token, to ensure that AWS CloudFormation performs the stack set operation only once, even if you retry the request multiple times. You might retry stack set operation requests to ensure that AWS CloudFormation successfully received them.
-- If you don't specify an operation ID, the SDK generates one automatically.
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usiOperationId :: Lens.Lens' UpdateStackInstances (Lude.Maybe Lude.Text)
usiOperationId = Lens.lens (operationId :: UpdateStackInstances -> Lude.Maybe Lude.Text) (\s a -> s {operationId = a} :: UpdateStackInstances)
{-# DEPRECATED usiOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | [@Service-managed@ permissions] The AWS Organizations accounts for which you want to update parameter values for stack instances. If your update targets OUs, the overridden parameter values only apply to the accounts that are currently in the target OUs and their child OUs. Accounts added to the target OUs and their child OUs in the future won't use the overridden values.
--
-- You can specify @Accounts@ or @DeploymentTargets@ , but not both.
--
-- /Note:/ Consider using 'deploymentTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usiDeploymentTargets :: Lens.Lens' UpdateStackInstances (Lude.Maybe DeploymentTargets)
usiDeploymentTargets = Lens.lens (deploymentTargets :: UpdateStackInstances -> Lude.Maybe DeploymentTargets) (\s a -> s {deploymentTargets = a} :: UpdateStackInstances)
{-# DEPRECATED usiDeploymentTargets "Use generic-lens or generic-optics with 'deploymentTargets' instead." #-}

-- | The name or unique ID of the stack set associated with the stack instances.
--
-- /Note:/ Consider using 'stackSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usiStackSetName :: Lens.Lens' UpdateStackInstances Lude.Text
usiStackSetName = Lens.lens (stackSetName :: UpdateStackInstances -> Lude.Text) (\s a -> s {stackSetName = a} :: UpdateStackInstances)
{-# DEPRECATED usiStackSetName "Use generic-lens or generic-optics with 'stackSetName' instead." #-}

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
usiParameterOverrides :: Lens.Lens' UpdateStackInstances (Lude.Maybe [Parameter])
usiParameterOverrides = Lens.lens (parameterOverrides :: UpdateStackInstances -> Lude.Maybe [Parameter]) (\s a -> s {parameterOverrides = a} :: UpdateStackInstances)
{-# DEPRECATED usiParameterOverrides "Use generic-lens or generic-optics with 'parameterOverrides' instead." #-}

instance Lude.AWSRequest UpdateStackInstances where
  type Rs UpdateStackInstances = UpdateStackInstancesResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "UpdateStackInstancesResult"
      ( \s h x ->
          UpdateStackInstancesResponse'
            Lude.<$> (x Lude..@? "OperationId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateStackInstances where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UpdateStackInstances where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateStackInstances where
  toQuery UpdateStackInstances' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("UpdateStackInstances" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "Accounts"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> accounts),
        "Regions" Lude.=: Lude.toQueryList "member" regions,
        "OperationPreferences" Lude.=: operationPreferences,
        "OperationId" Lude.=: operationId,
        "DeploymentTargets" Lude.=: deploymentTargets,
        "StackSetName" Lude.=: stackSetName,
        "ParameterOverrides"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> parameterOverrides)
      ]

-- | /See:/ 'mkUpdateStackInstancesResponse' smart constructor.
data UpdateStackInstancesResponse = UpdateStackInstancesResponse'
  { -- | The unique identifier for this stack set operation.
    operationId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateStackInstancesResponse' with the minimum fields required to make a request.
--
-- * 'operationId' - The unique identifier for this stack set operation.
-- * 'responseStatus' - The response status code.
mkUpdateStackInstancesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateStackInstancesResponse
mkUpdateStackInstancesResponse pResponseStatus_ =
  UpdateStackInstancesResponse'
    { operationId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique identifier for this stack set operation.
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usirsOperationId :: Lens.Lens' UpdateStackInstancesResponse (Lude.Maybe Lude.Text)
usirsOperationId = Lens.lens (operationId :: UpdateStackInstancesResponse -> Lude.Maybe Lude.Text) (\s a -> s {operationId = a} :: UpdateStackInstancesResponse)
{-# DEPRECATED usirsOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usirsResponseStatus :: Lens.Lens' UpdateStackInstancesResponse Lude.Int
usirsResponseStatus = Lens.lens (responseStatus :: UpdateStackInstancesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateStackInstancesResponse)
{-# DEPRECATED usirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
