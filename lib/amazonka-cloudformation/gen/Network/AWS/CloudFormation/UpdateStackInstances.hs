{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
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
--
-- You can only update stack instances in Regions and accounts where they already exist; to create additional stack instances, use <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_CreateStackInstances.html CreateStackInstances> .
--
-- During stack set updates, any parameters overridden for a stack instance are not updated, but retain their overridden value.
--
-- You can only update the parameter /values/ that are specified in the stack set; to add or delete a parameter itself, use <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_UpdateStackSet.html UpdateStackSet> to update the stack set template. If you add a parameter to a template, before you can override the parameter value specified in the stack set you must first use <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_UpdateStackSet.html UpdateStackSet> to update all stack instances with the updated template and parameter value specified in the stack set. Once a stack instance has been updated with the new parameter, you can then override the parameter value using @UpdateStackInstances@ .
module Network.AWS.CloudFormation.UpdateStackInstances
  ( -- * Creating a Request
    updateStackInstances,
    UpdateStackInstances,

    -- * Request Lenses
    usiAccounts,
    usiOperationPreferences,
    usiOperationId,
    usiDeploymentTargets,
    usiParameterOverrides,
    usiStackSetName,
    usiRegions,

    -- * Destructuring the Response
    updateStackInstancesResponse,
    UpdateStackInstancesResponse,

    -- * Response Lenses
    usirsOperationId,
    usirsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateStackInstances' smart constructor.
data UpdateStackInstances = UpdateStackInstances'
  { _usiAccounts ::
      !(Maybe [Text]),
    _usiOperationPreferences ::
      !(Maybe StackSetOperationPreferences),
    _usiOperationId :: !(Maybe Text),
    _usiDeploymentTargets ::
      !(Maybe DeploymentTargets),
    _usiParameterOverrides :: !(Maybe [Parameter]),
    _usiStackSetName :: !Text,
    _usiRegions :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateStackInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usiAccounts' - [@Self-managed@ permissions] The names of one or more AWS accounts for which you want to update parameter values for stack instances. The overridden parameter values will be applied to all stack instances in the specified accounts and Regions. You can specify @Accounts@ or @DeploymentTargets@ , but not both.
--
-- * 'usiOperationPreferences' - Preferences for how AWS CloudFormation performs this stack set operation.
--
-- * 'usiOperationId' - The unique identifier for this stack set operation.  The operation ID also functions as an idempotency token, to ensure that AWS CloudFormation performs the stack set operation only once, even if you retry the request multiple times. You might retry stack set operation requests to ensure that AWS CloudFormation successfully received them. If you don't specify an operation ID, the SDK generates one automatically.
--
-- * 'usiDeploymentTargets' - [@Service-managed@ permissions] The AWS Organizations accounts for which you want to update parameter values for stack instances. If your update targets OUs, the overridden parameter values only apply to the accounts that are currently in the target OUs and their child OUs. Accounts added to the target OUs and their child OUs in the future won't use the overridden values. You can specify @Accounts@ or @DeploymentTargets@ , but not both.
--
-- * 'usiParameterOverrides' - A list of input parameters whose values you want to update for the specified stack instances.  Any overridden parameter values will be applied to all stack instances in the specified accounts and Regions. When specifying parameters and their values, be aware of how AWS CloudFormation sets parameter values during stack instance update operations:     * To override the current value for a parameter, include the parameter and specify its value.     * To leave a parameter set to its present value, you can do one of the following:     * Do not include the parameter in the list.     * Include the parameter and specify @UsePreviousValue@ as @true@ . (You cannot specify both a value and set @UsePreviousValue@ to @true@ .)     * To set all overridden parameter back to the values specified in the stack set, specify a parameter list but do not include any parameters.     * To leave all parameters set to their present values, do not specify this property at all. During stack set updates, any parameter values overridden for a stack instance are not updated, but retain their overridden value. You can only override the parameter /values/ that are specified in the stack set; to add or delete a parameter itself, use @UpdateStackSet@ to update the stack set template. If you add a parameter to a template, before you can override the parameter value specified in the stack set you must first use <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_UpdateStackSet.html UpdateStackSet> to update all stack instances with the updated template and parameter value specified in the stack set. Once a stack instance has been updated with the new parameter, you can then override the parameter value using @UpdateStackInstances@ .
--
-- * 'usiStackSetName' - The name or unique ID of the stack set associated with the stack instances.
--
-- * 'usiRegions' - The names of one or more Regions in which you want to update parameter values for stack instances. The overridden parameter values will be applied to all stack instances in the specified accounts and Regions.
updateStackInstances ::
  -- | 'usiStackSetName'
  Text ->
  UpdateStackInstances
updateStackInstances pStackSetName_ =
  UpdateStackInstances'
    { _usiAccounts = Nothing,
      _usiOperationPreferences = Nothing,
      _usiOperationId = Nothing,
      _usiDeploymentTargets = Nothing,
      _usiParameterOverrides = Nothing,
      _usiStackSetName = pStackSetName_,
      _usiRegions = mempty
    }

-- | [@Self-managed@ permissions] The names of one or more AWS accounts for which you want to update parameter values for stack instances. The overridden parameter values will be applied to all stack instances in the specified accounts and Regions. You can specify @Accounts@ or @DeploymentTargets@ , but not both.
usiAccounts :: Lens' UpdateStackInstances [Text]
usiAccounts = lens _usiAccounts (\s a -> s {_usiAccounts = a}) . _Default . _Coerce

-- | Preferences for how AWS CloudFormation performs this stack set operation.
usiOperationPreferences :: Lens' UpdateStackInstances (Maybe StackSetOperationPreferences)
usiOperationPreferences = lens _usiOperationPreferences (\s a -> s {_usiOperationPreferences = a})

-- | The unique identifier for this stack set operation.  The operation ID also functions as an idempotency token, to ensure that AWS CloudFormation performs the stack set operation only once, even if you retry the request multiple times. You might retry stack set operation requests to ensure that AWS CloudFormation successfully received them. If you don't specify an operation ID, the SDK generates one automatically.
usiOperationId :: Lens' UpdateStackInstances (Maybe Text)
usiOperationId = lens _usiOperationId (\s a -> s {_usiOperationId = a})

-- | [@Service-managed@ permissions] The AWS Organizations accounts for which you want to update parameter values for stack instances. If your update targets OUs, the overridden parameter values only apply to the accounts that are currently in the target OUs and their child OUs. Accounts added to the target OUs and their child OUs in the future won't use the overridden values. You can specify @Accounts@ or @DeploymentTargets@ , but not both.
usiDeploymentTargets :: Lens' UpdateStackInstances (Maybe DeploymentTargets)
usiDeploymentTargets = lens _usiDeploymentTargets (\s a -> s {_usiDeploymentTargets = a})

-- | A list of input parameters whose values you want to update for the specified stack instances.  Any overridden parameter values will be applied to all stack instances in the specified accounts and Regions. When specifying parameters and their values, be aware of how AWS CloudFormation sets parameter values during stack instance update operations:     * To override the current value for a parameter, include the parameter and specify its value.     * To leave a parameter set to its present value, you can do one of the following:     * Do not include the parameter in the list.     * Include the parameter and specify @UsePreviousValue@ as @true@ . (You cannot specify both a value and set @UsePreviousValue@ to @true@ .)     * To set all overridden parameter back to the values specified in the stack set, specify a parameter list but do not include any parameters.     * To leave all parameters set to their present values, do not specify this property at all. During stack set updates, any parameter values overridden for a stack instance are not updated, but retain their overridden value. You can only override the parameter /values/ that are specified in the stack set; to add or delete a parameter itself, use @UpdateStackSet@ to update the stack set template. If you add a parameter to a template, before you can override the parameter value specified in the stack set you must first use <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_UpdateStackSet.html UpdateStackSet> to update all stack instances with the updated template and parameter value specified in the stack set. Once a stack instance has been updated with the new parameter, you can then override the parameter value using @UpdateStackInstances@ .
usiParameterOverrides :: Lens' UpdateStackInstances [Parameter]
usiParameterOverrides = lens _usiParameterOverrides (\s a -> s {_usiParameterOverrides = a}) . _Default . _Coerce

-- | The name or unique ID of the stack set associated with the stack instances.
usiStackSetName :: Lens' UpdateStackInstances Text
usiStackSetName = lens _usiStackSetName (\s a -> s {_usiStackSetName = a})

-- | The names of one or more Regions in which you want to update parameter values for stack instances. The overridden parameter values will be applied to all stack instances in the specified accounts and Regions.
usiRegions :: Lens' UpdateStackInstances [Text]
usiRegions = lens _usiRegions (\s a -> s {_usiRegions = a}) . _Coerce

instance AWSRequest UpdateStackInstances where
  type Rs UpdateStackInstances = UpdateStackInstancesResponse
  request = postQuery cloudFormation
  response =
    receiveXMLWrapper
      "UpdateStackInstancesResult"
      ( \s h x ->
          UpdateStackInstancesResponse'
            <$> (x .@? "OperationId") <*> (pure (fromEnum s))
      )

instance Hashable UpdateStackInstances

instance NFData UpdateStackInstances

instance ToHeaders UpdateStackInstances where
  toHeaders = const mempty

instance ToPath UpdateStackInstances where
  toPath = const "/"

instance ToQuery UpdateStackInstances where
  toQuery UpdateStackInstances' {..} =
    mconcat
      [ "Action" =: ("UpdateStackInstances" :: ByteString),
        "Version" =: ("2010-05-15" :: ByteString),
        "Accounts" =: toQuery (toQueryList "member" <$> _usiAccounts),
        "OperationPreferences" =: _usiOperationPreferences,
        "OperationId" =: _usiOperationId,
        "DeploymentTargets" =: _usiDeploymentTargets,
        "ParameterOverrides"
          =: toQuery (toQueryList "member" <$> _usiParameterOverrides),
        "StackSetName" =: _usiStackSetName,
        "Regions" =: toQueryList "member" _usiRegions
      ]

-- | /See:/ 'updateStackInstancesResponse' smart constructor.
data UpdateStackInstancesResponse = UpdateStackInstancesResponse'
  { _usirsOperationId ::
      !(Maybe Text),
    _usirsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateStackInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usirsOperationId' - The unique identifier for this stack set operation.
--
-- * 'usirsResponseStatus' - -- | The response status code.
updateStackInstancesResponse ::
  -- | 'usirsResponseStatus'
  Int ->
  UpdateStackInstancesResponse
updateStackInstancesResponse pResponseStatus_ =
  UpdateStackInstancesResponse'
    { _usirsOperationId = Nothing,
      _usirsResponseStatus = pResponseStatus_
    }

-- | The unique identifier for this stack set operation.
usirsOperationId :: Lens' UpdateStackInstancesResponse (Maybe Text)
usirsOperationId = lens _usirsOperationId (\s a -> s {_usirsOperationId = a})

-- | -- | The response status code.
usirsResponseStatus :: Lens' UpdateStackInstancesResponse Int
usirsResponseStatus = lens _usirsResponseStatus (\s a -> s {_usirsResponseStatus = a})

instance NFData UpdateStackInstancesResponse
