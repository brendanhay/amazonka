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
-- Module      : Network.AWS.CloudFormation.CreateStackInstances
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates stack instances for the specified accounts, within the specified regions. A stack instance refers to a stack in a specific account and region. @Accounts@ and @Regions@ are required parameters—you must specify at least one account and one region.
--
--
module Network.AWS.CloudFormation.CreateStackInstances
    (
    -- * Creating a Request
      createStackInstances
    , CreateStackInstances
    -- * Request Lenses
    , csiOperationPreferences
    , csiOperationId
    , csiStackSetName
    , csiAccounts
    , csiRegions

    -- * Destructuring the Response
    , createStackInstancesResponse
    , CreateStackInstancesResponse
    -- * Response Lenses
    , csirsOperationId
    , csirsResponseStatus
    ) where

import           Network.AWS.CloudFormation.Types
import           Network.AWS.CloudFormation.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createStackInstances' smart constructor.
data CreateStackInstances = CreateStackInstances'
    { _csiOperationPreferences :: !(Maybe StackSetOperationPreferences)
    , _csiOperationId          :: !(Maybe Text)
    , _csiStackSetName         :: !Text
    , _csiAccounts             :: ![Text]
    , _csiRegions              :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateStackInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csiOperationPreferences' - Preferences for how AWS CloudFormation performs this stack set operation.
--
-- * 'csiOperationId' - The unique identifier for this stack set operation.  The operation ID also functions as an idempotency token, to ensure that AWS CloudFormation performs the stack set operation only once, even if you retry the request multiple times. You might retry stack set operation requests to ensure that AWS CloudFormation successfully received them. If you don't specify an operation ID, the SDK generates one automatically.  Repeating this stack set operation with a new operation ID retries all stack instances whose status is @OUTDATED@ .
--
-- * 'csiStackSetName' - The name or unique ID of the stack set that you want to create stack instances from.
--
-- * 'csiAccounts' - The names of one or more AWS accounts that you want to create stack instances in the specified region(s) for.
--
-- * 'csiRegions' - The names of one or more regions where you want to create stack instances using the specified AWS account(s).
createStackInstances
    :: Text -- ^ 'csiStackSetName'
    -> CreateStackInstances
createStackInstances pStackSetName_ =
    CreateStackInstances'
    { _csiOperationPreferences = Nothing
    , _csiOperationId = Nothing
    , _csiStackSetName = pStackSetName_
    , _csiAccounts = mempty
    , _csiRegions = mempty
    }

-- | Preferences for how AWS CloudFormation performs this stack set operation.
csiOperationPreferences :: Lens' CreateStackInstances (Maybe StackSetOperationPreferences)
csiOperationPreferences = lens _csiOperationPreferences (\ s a -> s{_csiOperationPreferences = a});

-- | The unique identifier for this stack set operation.  The operation ID also functions as an idempotency token, to ensure that AWS CloudFormation performs the stack set operation only once, even if you retry the request multiple times. You might retry stack set operation requests to ensure that AWS CloudFormation successfully received them. If you don't specify an operation ID, the SDK generates one automatically.  Repeating this stack set operation with a new operation ID retries all stack instances whose status is @OUTDATED@ .
csiOperationId :: Lens' CreateStackInstances (Maybe Text)
csiOperationId = lens _csiOperationId (\ s a -> s{_csiOperationId = a});

-- | The name or unique ID of the stack set that you want to create stack instances from.
csiStackSetName :: Lens' CreateStackInstances Text
csiStackSetName = lens _csiStackSetName (\ s a -> s{_csiStackSetName = a});

-- | The names of one or more AWS accounts that you want to create stack instances in the specified region(s) for.
csiAccounts :: Lens' CreateStackInstances [Text]
csiAccounts = lens _csiAccounts (\ s a -> s{_csiAccounts = a}) . _Coerce;

-- | The names of one or more regions where you want to create stack instances using the specified AWS account(s).
csiRegions :: Lens' CreateStackInstances [Text]
csiRegions = lens _csiRegions (\ s a -> s{_csiRegions = a}) . _Coerce;

instance AWSRequest CreateStackInstances where
        type Rs CreateStackInstances =
             CreateStackInstancesResponse
        request = postQuery cloudFormation
        response
          = receiveXMLWrapper "CreateStackInstancesResult"
              (\ s h x ->
                 CreateStackInstancesResponse' <$>
                   (x .@? "OperationId") <*> (pure (fromEnum s)))

instance Hashable CreateStackInstances

instance NFData CreateStackInstances

instance ToHeaders CreateStackInstances where
        toHeaders = const mempty

instance ToPath CreateStackInstances where
        toPath = const "/"

instance ToQuery CreateStackInstances where
        toQuery CreateStackInstances'{..}
          = mconcat
              ["Action" =: ("CreateStackInstances" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "OperationPreferences" =: _csiOperationPreferences,
               "OperationId" =: _csiOperationId,
               "StackSetName" =: _csiStackSetName,
               "Accounts" =: toQueryList "member" _csiAccounts,
               "Regions" =: toQueryList "member" _csiRegions]

-- | /See:/ 'createStackInstancesResponse' smart constructor.
data CreateStackInstancesResponse = CreateStackInstancesResponse'
    { _csirsOperationId    :: !(Maybe Text)
    , _csirsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateStackInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csirsOperationId' - The unique identifier for this stack set operation.
--
-- * 'csirsResponseStatus' - -- | The response status code.
createStackInstancesResponse
    :: Int -- ^ 'csirsResponseStatus'
    -> CreateStackInstancesResponse
createStackInstancesResponse pResponseStatus_ =
    CreateStackInstancesResponse'
    { _csirsOperationId = Nothing
    , _csirsResponseStatus = pResponseStatus_
    }

-- | The unique identifier for this stack set operation.
csirsOperationId :: Lens' CreateStackInstancesResponse (Maybe Text)
csirsOperationId = lens _csirsOperationId (\ s a -> s{_csirsOperationId = a});

-- | -- | The response status code.
csirsResponseStatus :: Lens' CreateStackInstancesResponse Int
csirsResponseStatus = lens _csirsResponseStatus (\ s a -> s{_csirsResponseStatus = a});

instance NFData CreateStackInstancesResponse
