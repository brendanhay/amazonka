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
-- Module      : Network.AWS.CloudFormation.CreateStackInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates stack instances for the specified accounts, within the specified Regions. A stack instance refers to a stack in a specific account and Region. You must specify at least one value for either @Accounts@ or @DeploymentTargets@ , and you must specify at least one value for @Regions@ .
module Network.AWS.CloudFormation.CreateStackInstances
  ( -- * Creating a Request
    createStackInstances,
    CreateStackInstances,

    -- * Request Lenses
    csiAccounts,
    csiOperationPreferences,
    csiOperationId,
    csiDeploymentTargets,
    csiParameterOverrides,
    csiStackSetName,
    csiRegions,

    -- * Destructuring the Response
    createStackInstancesResponse,
    CreateStackInstancesResponse,

    -- * Response Lenses
    csirsOperationId,
    csirsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createStackInstances' smart constructor.
data CreateStackInstances = CreateStackInstances'
  { _csiAccounts ::
      !(Maybe [Text]),
    _csiOperationPreferences ::
      !(Maybe StackSetOperationPreferences),
    _csiOperationId :: !(Maybe Text),
    _csiDeploymentTargets ::
      !(Maybe DeploymentTargets),
    _csiParameterOverrides :: !(Maybe [Parameter]),
    _csiStackSetName :: !Text,
    _csiRegions :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateStackInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csiAccounts' - [@Self-managed@ permissions] The names of one or more AWS accounts that you want to create stack instances in the specified Region(s) for. You can specify @Accounts@ or @DeploymentTargets@ , but not both.
--
-- * 'csiOperationPreferences' - Preferences for how AWS CloudFormation performs this stack set operation.
--
-- * 'csiOperationId' - The unique identifier for this stack set operation.  The operation ID also functions as an idempotency token, to ensure that AWS CloudFormation performs the stack set operation only once, even if you retry the request multiple times. You might retry stack set operation requests to ensure that AWS CloudFormation successfully received them. If you don't specify an operation ID, the SDK generates one automatically.  Repeating this stack set operation with a new operation ID retries all stack instances whose status is @OUTDATED@ .
--
-- * 'csiDeploymentTargets' - [@Service-managed@ permissions] The AWS Organizations accounts for which to create stack instances in the specified Regions. You can specify @Accounts@ or @DeploymentTargets@ , but not both.
--
-- * 'csiParameterOverrides' - A list of stack set parameters whose values you want to override in the selected stack instances. Any overridden parameter values will be applied to all stack instances in the specified accounts and Regions. When specifying parameters and their values, be aware of how AWS CloudFormation sets parameter values during stack instance operations:     * To override the current value for a parameter, include the parameter and specify its value.     * To leave a parameter set to its present value, you can do one of the following:     * Do not include the parameter in the list.     * Include the parameter and specify @UsePreviousValue@ as @true@ . (You cannot specify both a value and set @UsePreviousValue@ to @true@ .)     * To set all overridden parameter back to the values specified in the stack set, specify a parameter list but do not include any parameters.     * To leave all parameters set to their present values, do not specify this property at all. During stack set updates, any parameter values overridden for a stack instance are not updated, but retain their overridden value. You can only override the parameter /values/ that are specified in the stack set; to add or delete a parameter itself, use <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_UpdateStackSet.html UpdateStackSet> to update the stack set template.
--
-- * 'csiStackSetName' - The name or unique ID of the stack set that you want to create stack instances from.
--
-- * 'csiRegions' - The names of one or more Regions where you want to create stack instances using the specified AWS account(s).
createStackInstances ::
  -- | 'csiStackSetName'
  Text ->
  CreateStackInstances
createStackInstances pStackSetName_ =
  CreateStackInstances'
    { _csiAccounts = Nothing,
      _csiOperationPreferences = Nothing,
      _csiOperationId = Nothing,
      _csiDeploymentTargets = Nothing,
      _csiParameterOverrides = Nothing,
      _csiStackSetName = pStackSetName_,
      _csiRegions = mempty
    }

-- | [@Self-managed@ permissions] The names of one or more AWS accounts that you want to create stack instances in the specified Region(s) for. You can specify @Accounts@ or @DeploymentTargets@ , but not both.
csiAccounts :: Lens' CreateStackInstances [Text]
csiAccounts = lens _csiAccounts (\s a -> s {_csiAccounts = a}) . _Default . _Coerce

-- | Preferences for how AWS CloudFormation performs this stack set operation.
csiOperationPreferences :: Lens' CreateStackInstances (Maybe StackSetOperationPreferences)
csiOperationPreferences = lens _csiOperationPreferences (\s a -> s {_csiOperationPreferences = a})

-- | The unique identifier for this stack set operation.  The operation ID also functions as an idempotency token, to ensure that AWS CloudFormation performs the stack set operation only once, even if you retry the request multiple times. You might retry stack set operation requests to ensure that AWS CloudFormation successfully received them. If you don't specify an operation ID, the SDK generates one automatically.  Repeating this stack set operation with a new operation ID retries all stack instances whose status is @OUTDATED@ .
csiOperationId :: Lens' CreateStackInstances (Maybe Text)
csiOperationId = lens _csiOperationId (\s a -> s {_csiOperationId = a})

-- | [@Service-managed@ permissions] The AWS Organizations accounts for which to create stack instances in the specified Regions. You can specify @Accounts@ or @DeploymentTargets@ , but not both.
csiDeploymentTargets :: Lens' CreateStackInstances (Maybe DeploymentTargets)
csiDeploymentTargets = lens _csiDeploymentTargets (\s a -> s {_csiDeploymentTargets = a})

-- | A list of stack set parameters whose values you want to override in the selected stack instances. Any overridden parameter values will be applied to all stack instances in the specified accounts and Regions. When specifying parameters and their values, be aware of how AWS CloudFormation sets parameter values during stack instance operations:     * To override the current value for a parameter, include the parameter and specify its value.     * To leave a parameter set to its present value, you can do one of the following:     * Do not include the parameter in the list.     * Include the parameter and specify @UsePreviousValue@ as @true@ . (You cannot specify both a value and set @UsePreviousValue@ to @true@ .)     * To set all overridden parameter back to the values specified in the stack set, specify a parameter list but do not include any parameters.     * To leave all parameters set to their present values, do not specify this property at all. During stack set updates, any parameter values overridden for a stack instance are not updated, but retain their overridden value. You can only override the parameter /values/ that are specified in the stack set; to add or delete a parameter itself, use <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_UpdateStackSet.html UpdateStackSet> to update the stack set template.
csiParameterOverrides :: Lens' CreateStackInstances [Parameter]
csiParameterOverrides = lens _csiParameterOverrides (\s a -> s {_csiParameterOverrides = a}) . _Default . _Coerce

-- | The name or unique ID of the stack set that you want to create stack instances from.
csiStackSetName :: Lens' CreateStackInstances Text
csiStackSetName = lens _csiStackSetName (\s a -> s {_csiStackSetName = a})

-- | The names of one or more Regions where you want to create stack instances using the specified AWS account(s).
csiRegions :: Lens' CreateStackInstances [Text]
csiRegions = lens _csiRegions (\s a -> s {_csiRegions = a}) . _Coerce

instance AWSRequest CreateStackInstances where
  type Rs CreateStackInstances = CreateStackInstancesResponse
  request = postQuery cloudFormation
  response =
    receiveXMLWrapper
      "CreateStackInstancesResult"
      ( \s h x ->
          CreateStackInstancesResponse'
            <$> (x .@? "OperationId") <*> (pure (fromEnum s))
      )

instance Hashable CreateStackInstances

instance NFData CreateStackInstances

instance ToHeaders CreateStackInstances where
  toHeaders = const mempty

instance ToPath CreateStackInstances where
  toPath = const "/"

instance ToQuery CreateStackInstances where
  toQuery CreateStackInstances' {..} =
    mconcat
      [ "Action" =: ("CreateStackInstances" :: ByteString),
        "Version" =: ("2010-05-15" :: ByteString),
        "Accounts" =: toQuery (toQueryList "member" <$> _csiAccounts),
        "OperationPreferences" =: _csiOperationPreferences,
        "OperationId" =: _csiOperationId,
        "DeploymentTargets" =: _csiDeploymentTargets,
        "ParameterOverrides"
          =: toQuery (toQueryList "member" <$> _csiParameterOverrides),
        "StackSetName" =: _csiStackSetName,
        "Regions" =: toQueryList "member" _csiRegions
      ]

-- | /See:/ 'createStackInstancesResponse' smart constructor.
data CreateStackInstancesResponse = CreateStackInstancesResponse'
  { _csirsOperationId ::
      !(Maybe Text),
    _csirsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateStackInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csirsOperationId' - The unique identifier for this stack set operation.
--
-- * 'csirsResponseStatus' - -- | The response status code.
createStackInstancesResponse ::
  -- | 'csirsResponseStatus'
  Int ->
  CreateStackInstancesResponse
createStackInstancesResponse pResponseStatus_ =
  CreateStackInstancesResponse'
    { _csirsOperationId = Nothing,
      _csirsResponseStatus = pResponseStatus_
    }

-- | The unique identifier for this stack set operation.
csirsOperationId :: Lens' CreateStackInstancesResponse (Maybe Text)
csirsOperationId = lens _csirsOperationId (\s a -> s {_csirsOperationId = a})

-- | -- | The response status code.
csirsResponseStatus :: Lens' CreateStackInstancesResponse Int
csirsResponseStatus = lens _csirsResponseStatus (\s a -> s {_csirsResponseStatus = a})

instance NFData CreateStackInstancesResponse
