{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    csiAccounts,
    csiOperationPreferences,
    csiOperationId,
    csiDeploymentTargets,
    csiParameterOverrides,
    csiStackSetName,
    csiRegions,

    -- * Destructuring the response
    CreateStackInstancesResponse (..),
    mkCreateStackInstancesResponse,

    -- ** Response lenses
    csirsOperationId,
    csirsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateStackInstances' smart constructor.
data CreateStackInstances = CreateStackInstances'
  { accounts ::
      Lude.Maybe [Lude.Text],
    operationPreferences ::
      Lude.Maybe StackSetOperationPreferences,
    operationId :: Lude.Maybe Lude.Text,
    deploymentTargets :: Lude.Maybe DeploymentTargets,
    parameterOverrides :: Lude.Maybe [Parameter],
    stackSetName :: Lude.Text,
    regions :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateStackInstances' with the minimum fields required to make a request.
--
-- * 'accounts' - [@Self-managed@ permissions] The names of one or more AWS accounts that you want to create stack instances in the specified Region(s) for.
--
-- You can specify @Accounts@ or @DeploymentTargets@ , but not both.
-- * 'deploymentTargets' - [@Service-managed@ permissions] The AWS Organizations accounts for which to create stack instances in the specified Regions.
--
-- You can specify @Accounts@ or @DeploymentTargets@ , but not both.
-- * 'operationId' - The unique identifier for this stack set operation.
--
-- The operation ID also functions as an idempotency token, to ensure that AWS CloudFormation performs the stack set operation only once, even if you retry the request multiple times. You might retry stack set operation requests to ensure that AWS CloudFormation successfully received them.
-- If you don't specify an operation ID, the SDK generates one automatically.
-- Repeating this stack set operation with a new operation ID retries all stack instances whose status is @OUTDATED@ .
-- * 'operationPreferences' - Preferences for how AWS CloudFormation performs this stack set operation.
-- * 'parameterOverrides' - A list of stack set parameters whose values you want to override in the selected stack instances.
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
-- * 'regions' - The names of one or more Regions where you want to create stack instances using the specified AWS account(s).
-- * 'stackSetName' - The name or unique ID of the stack set that you want to create stack instances from.
mkCreateStackInstances ::
  -- | 'stackSetName'
  Lude.Text ->
  CreateStackInstances
mkCreateStackInstances pStackSetName_ =
  CreateStackInstances'
    { accounts = Lude.Nothing,
      operationPreferences = Lude.Nothing,
      operationId = Lude.Nothing,
      deploymentTargets = Lude.Nothing,
      parameterOverrides = Lude.Nothing,
      stackSetName = pStackSetName_,
      regions = Lude.mempty
    }

-- | [@Self-managed@ permissions] The names of one or more AWS accounts that you want to create stack instances in the specified Region(s) for.
--
-- You can specify @Accounts@ or @DeploymentTargets@ , but not both.
--
-- /Note:/ Consider using 'accounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csiAccounts :: Lens.Lens' CreateStackInstances (Lude.Maybe [Lude.Text])
csiAccounts = Lens.lens (accounts :: CreateStackInstances -> Lude.Maybe [Lude.Text]) (\s a -> s {accounts = a} :: CreateStackInstances)
{-# DEPRECATED csiAccounts "Use generic-lens or generic-optics with 'accounts' instead." #-}

-- | Preferences for how AWS CloudFormation performs this stack set operation.
--
-- /Note:/ Consider using 'operationPreferences' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csiOperationPreferences :: Lens.Lens' CreateStackInstances (Lude.Maybe StackSetOperationPreferences)
csiOperationPreferences = Lens.lens (operationPreferences :: CreateStackInstances -> Lude.Maybe StackSetOperationPreferences) (\s a -> s {operationPreferences = a} :: CreateStackInstances)
{-# DEPRECATED csiOperationPreferences "Use generic-lens or generic-optics with 'operationPreferences' instead." #-}

-- | The unique identifier for this stack set operation.
--
-- The operation ID also functions as an idempotency token, to ensure that AWS CloudFormation performs the stack set operation only once, even if you retry the request multiple times. You might retry stack set operation requests to ensure that AWS CloudFormation successfully received them.
-- If you don't specify an operation ID, the SDK generates one automatically.
-- Repeating this stack set operation with a new operation ID retries all stack instances whose status is @OUTDATED@ .
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csiOperationId :: Lens.Lens' CreateStackInstances (Lude.Maybe Lude.Text)
csiOperationId = Lens.lens (operationId :: CreateStackInstances -> Lude.Maybe Lude.Text) (\s a -> s {operationId = a} :: CreateStackInstances)
{-# DEPRECATED csiOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | [@Service-managed@ permissions] The AWS Organizations accounts for which to create stack instances in the specified Regions.
--
-- You can specify @Accounts@ or @DeploymentTargets@ , but not both.
--
-- /Note:/ Consider using 'deploymentTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csiDeploymentTargets :: Lens.Lens' CreateStackInstances (Lude.Maybe DeploymentTargets)
csiDeploymentTargets = Lens.lens (deploymentTargets :: CreateStackInstances -> Lude.Maybe DeploymentTargets) (\s a -> s {deploymentTargets = a} :: CreateStackInstances)
{-# DEPRECATED csiDeploymentTargets "Use generic-lens or generic-optics with 'deploymentTargets' instead." #-}

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
csiParameterOverrides :: Lens.Lens' CreateStackInstances (Lude.Maybe [Parameter])
csiParameterOverrides = Lens.lens (parameterOverrides :: CreateStackInstances -> Lude.Maybe [Parameter]) (\s a -> s {parameterOverrides = a} :: CreateStackInstances)
{-# DEPRECATED csiParameterOverrides "Use generic-lens or generic-optics with 'parameterOverrides' instead." #-}

-- | The name or unique ID of the stack set that you want to create stack instances from.
--
-- /Note:/ Consider using 'stackSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csiStackSetName :: Lens.Lens' CreateStackInstances Lude.Text
csiStackSetName = Lens.lens (stackSetName :: CreateStackInstances -> Lude.Text) (\s a -> s {stackSetName = a} :: CreateStackInstances)
{-# DEPRECATED csiStackSetName "Use generic-lens or generic-optics with 'stackSetName' instead." #-}

-- | The names of one or more Regions where you want to create stack instances using the specified AWS account(s).
--
-- /Note:/ Consider using 'regions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csiRegions :: Lens.Lens' CreateStackInstances [Lude.Text]
csiRegions = Lens.lens (regions :: CreateStackInstances -> [Lude.Text]) (\s a -> s {regions = a} :: CreateStackInstances)
{-# DEPRECATED csiRegions "Use generic-lens or generic-optics with 'regions' instead." #-}

instance Lude.AWSRequest CreateStackInstances where
  type Rs CreateStackInstances = CreateStackInstancesResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "CreateStackInstancesResult"
      ( \s h x ->
          CreateStackInstancesResponse'
            Lude.<$> (x Lude..@? "OperationId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateStackInstances where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateStackInstances where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateStackInstances where
  toQuery CreateStackInstances' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateStackInstances" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "Accounts"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> accounts),
        "OperationPreferences" Lude.=: operationPreferences,
        "OperationId" Lude.=: operationId,
        "DeploymentTargets" Lude.=: deploymentTargets,
        "ParameterOverrides"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> parameterOverrides),
        "StackSetName" Lude.=: stackSetName,
        "Regions" Lude.=: Lude.toQueryList "member" regions
      ]

-- | /See:/ 'mkCreateStackInstancesResponse' smart constructor.
data CreateStackInstancesResponse = CreateStackInstancesResponse'
  { operationId ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateStackInstancesResponse' with the minimum fields required to make a request.
--
-- * 'operationId' - The unique identifier for this stack set operation.
-- * 'responseStatus' - The response status code.
mkCreateStackInstancesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateStackInstancesResponse
mkCreateStackInstancesResponse pResponseStatus_ =
  CreateStackInstancesResponse'
    { operationId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique identifier for this stack set operation.
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csirsOperationId :: Lens.Lens' CreateStackInstancesResponse (Lude.Maybe Lude.Text)
csirsOperationId = Lens.lens (operationId :: CreateStackInstancesResponse -> Lude.Maybe Lude.Text) (\s a -> s {operationId = a} :: CreateStackInstancesResponse)
{-# DEPRECATED csirsOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csirsResponseStatus :: Lens.Lens' CreateStackInstancesResponse Lude.Int
csirsResponseStatus = Lens.lens (responseStatus :: CreateStackInstancesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateStackInstancesResponse)
{-# DEPRECATED csirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
