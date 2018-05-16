{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.UpdateStackInstances
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the parameter values for stack instances for the specified accounts, within the specified regions. A stack instance refers to a stack in a specific account and region.
--
--
-- You can only update stack instances in regions and accounts where they already exist; to create additional stack instances, use <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_CreateStackInstances.html CreateStackInstances> .
--
-- During stack set updates, any parameters overridden for a stack instance are not updated, but retain their overridden value.
--
-- You can only update the parameter /values/ that are specified in the stack set; to add or delete a parameter itself, use <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_UpdateStackSet.html UpdateStackSet> to update the stack set template. If you add a parameter to a template, before you can override the parameter value specified in the stack set you must first use <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_UpdateStackSet.html UpdateStackSet> to update all stack instances with the updated template and parameter value specified in the stack set. Once a stack instance has been updated with the new parameter, you can then override the parameter value using @UpdateStackInstances@ .
--
module Network.AWS.CloudFormation.UpdateStackInstances
    (
    -- * Creating a Request
      updateStackInstances
    , UpdateStackInstances
    -- * Request Lenses
    , usiOperationPreferences
    , usiOperationId
    , usiParameterOverrides
    , usiStackSetName
    , usiAccounts
    , usiRegions

    -- * Destructuring the Response
    , updateStackInstancesResponse
    , UpdateStackInstancesResponse
    -- * Response Lenses
    , usirsOperationId
    , usirsResponseStatus
    ) where

import Network.AWS.CloudFormation.Types
import Network.AWS.CloudFormation.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateStackInstances' smart constructor.
data UpdateStackInstances = UpdateStackInstances'
  { _usiOperationPreferences :: !(Maybe StackSetOperationPreferences)
  , _usiOperationId          :: !(Maybe Text)
  , _usiParameterOverrides   :: !(Maybe [Parameter])
  , _usiStackSetName         :: !Text
  , _usiAccounts             :: ![Text]
  , _usiRegions              :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateStackInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usiOperationPreferences' - Preferences for how AWS CloudFormation performs this stack set operation.
--
-- * 'usiOperationId' - The unique identifier for this stack set operation.  The operation ID also functions as an idempotency token, to ensure that AWS CloudFormation performs the stack set operation only once, even if you retry the request multiple times. You might retry stack set operation requests to ensure that AWS CloudFormation successfully received them. If you don't specify an operation ID, the SDK generates one automatically.
--
-- * 'usiParameterOverrides' - A list of input parameters whose values you want to update for the specified stack instances.  Any overridden parameter values will be applied to all stack instances in the specified accounts and regions. When specifying parameters and their values, be aware of how AWS CloudFormation sets parameter values during stack instance update operations:     * To override the current value for a parameter, include the parameter and specify its value.     * To leave a parameter set to its present value, you can do one of the following:     * Do not include the parameter in the list.     * Include the parameter and specify @UsePreviousValue@ as @true@ . (You cannot specify both a value and set @UsePreviousValue@ to @true@ .)     * To set all overridden parameter back to the values specified in the stack set, specify a parameter list but do not include any parameters.     * To leave all parameters set to their present values, do not specify this property at all. During stack set updates, any parameter values overridden for a stack instance are not updated, but retain their overridden value. You can only override the parameter /values/ that are specified in the stack set; to add or delete a parameter itself, use @UpdateStackSet@ to update the stack set template. If you add a parameter to a template, before you can override the parameter value specified in the stack set you must first use <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_UpdateStackSet.html UpdateStackSet> to update all stack instances with the updated template and parameter value specified in the stack set. Once a stack instance has been updated with the new parameter, you can then override the parameter value using @UpdateStackInstances@ .
--
-- * 'usiStackSetName' - The name or unique ID of the stack set associated with the stack instances.
--
-- * 'usiAccounts' - The names of one or more AWS accounts for which you want to update parameter values for stack instances. The overridden parameter values will be applied to all stack instances in the specified accounts and regions.
--
-- * 'usiRegions' - The names of one or more regions in which you want to update parameter values for stack instances. The overridden parameter values will be applied to all stack instances in the specified accounts and regions.
updateStackInstances
    :: Text -- ^ 'usiStackSetName'
    -> UpdateStackInstances
updateStackInstances pStackSetName_ =
  UpdateStackInstances'
    { _usiOperationPreferences = Nothing
    , _usiOperationId = Nothing
    , _usiParameterOverrides = Nothing
    , _usiStackSetName = pStackSetName_
    , _usiAccounts = mempty
    , _usiRegions = mempty
    }


-- | Preferences for how AWS CloudFormation performs this stack set operation.
usiOperationPreferences :: Lens' UpdateStackInstances (Maybe StackSetOperationPreferences)
usiOperationPreferences = lens _usiOperationPreferences (\ s a -> s{_usiOperationPreferences = a})

-- | The unique identifier for this stack set operation.  The operation ID also functions as an idempotency token, to ensure that AWS CloudFormation performs the stack set operation only once, even if you retry the request multiple times. You might retry stack set operation requests to ensure that AWS CloudFormation successfully received them. If you don't specify an operation ID, the SDK generates one automatically.
usiOperationId :: Lens' UpdateStackInstances (Maybe Text)
usiOperationId = lens _usiOperationId (\ s a -> s{_usiOperationId = a})

-- | A list of input parameters whose values you want to update for the specified stack instances.  Any overridden parameter values will be applied to all stack instances in the specified accounts and regions. When specifying parameters and their values, be aware of how AWS CloudFormation sets parameter values during stack instance update operations:     * To override the current value for a parameter, include the parameter and specify its value.     * To leave a parameter set to its present value, you can do one of the following:     * Do not include the parameter in the list.     * Include the parameter and specify @UsePreviousValue@ as @true@ . (You cannot specify both a value and set @UsePreviousValue@ to @true@ .)     * To set all overridden parameter back to the values specified in the stack set, specify a parameter list but do not include any parameters.     * To leave all parameters set to their present values, do not specify this property at all. During stack set updates, any parameter values overridden for a stack instance are not updated, but retain their overridden value. You can only override the parameter /values/ that are specified in the stack set; to add or delete a parameter itself, use @UpdateStackSet@ to update the stack set template. If you add a parameter to a template, before you can override the parameter value specified in the stack set you must first use <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_UpdateStackSet.html UpdateStackSet> to update all stack instances with the updated template and parameter value specified in the stack set. Once a stack instance has been updated with the new parameter, you can then override the parameter value using @UpdateStackInstances@ .
usiParameterOverrides :: Lens' UpdateStackInstances [Parameter]
usiParameterOverrides = lens _usiParameterOverrides (\ s a -> s{_usiParameterOverrides = a}) . _Default . _Coerce

-- | The name or unique ID of the stack set associated with the stack instances.
usiStackSetName :: Lens' UpdateStackInstances Text
usiStackSetName = lens _usiStackSetName (\ s a -> s{_usiStackSetName = a})

-- | The names of one or more AWS accounts for which you want to update parameter values for stack instances. The overridden parameter values will be applied to all stack instances in the specified accounts and regions.
usiAccounts :: Lens' UpdateStackInstances [Text]
usiAccounts = lens _usiAccounts (\ s a -> s{_usiAccounts = a}) . _Coerce

-- | The names of one or more regions in which you want to update parameter values for stack instances. The overridden parameter values will be applied to all stack instances in the specified accounts and regions.
usiRegions :: Lens' UpdateStackInstances [Text]
usiRegions = lens _usiRegions (\ s a -> s{_usiRegions = a}) . _Coerce

instance AWSRequest UpdateStackInstances where
        type Rs UpdateStackInstances =
             UpdateStackInstancesResponse
        request = postQuery cloudFormation
        response
          = receiveXMLWrapper "UpdateStackInstancesResult"
              (\ s h x ->
                 UpdateStackInstancesResponse' <$>
                   (x .@? "OperationId") <*> (pure (fromEnum s)))

instance Hashable UpdateStackInstances where

instance NFData UpdateStackInstances where

instance ToHeaders UpdateStackInstances where
        toHeaders = const mempty

instance ToPath UpdateStackInstances where
        toPath = const "/"

instance ToQuery UpdateStackInstances where
        toQuery UpdateStackInstances'{..}
          = mconcat
              ["Action" =: ("UpdateStackInstances" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "OperationPreferences" =: _usiOperationPreferences,
               "OperationId" =: _usiOperationId,
               "ParameterOverrides" =:
                 toQuery
                   (toQueryList "member" <$> _usiParameterOverrides),
               "StackSetName" =: _usiStackSetName,
               "Accounts" =: toQueryList "member" _usiAccounts,
               "Regions" =: toQueryList "member" _usiRegions]

-- | /See:/ 'updateStackInstancesResponse' smart constructor.
data UpdateStackInstancesResponse = UpdateStackInstancesResponse'
  { _usirsOperationId    :: !(Maybe Text)
  , _usirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateStackInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usirsOperationId' - The unique identifier for this stack set operation.
--
-- * 'usirsResponseStatus' - -- | The response status code.
updateStackInstancesResponse
    :: Int -- ^ 'usirsResponseStatus'
    -> UpdateStackInstancesResponse
updateStackInstancesResponse pResponseStatus_ =
  UpdateStackInstancesResponse'
    {_usirsOperationId = Nothing, _usirsResponseStatus = pResponseStatus_}


-- | The unique identifier for this stack set operation.
usirsOperationId :: Lens' UpdateStackInstancesResponse (Maybe Text)
usirsOperationId = lens _usirsOperationId (\ s a -> s{_usirsOperationId = a})

-- | -- | The response status code.
usirsResponseStatus :: Lens' UpdateStackInstancesResponse Int
usirsResponseStatus = lens _usirsResponseStatus (\ s a -> s{_usirsResponseStatus = a})

instance NFData UpdateStackInstancesResponse where
