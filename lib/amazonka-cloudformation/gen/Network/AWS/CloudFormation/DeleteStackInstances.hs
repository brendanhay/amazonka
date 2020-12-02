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
-- Module      : Network.AWS.CloudFormation.DeleteStackInstances
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes stack instances for the specified accounts, in the specified regions.
--
--
module Network.AWS.CloudFormation.DeleteStackInstances
    (
    -- * Creating a Request
      deleteStackInstances
    , DeleteStackInstances
    -- * Request Lenses
    , dsiOperationPreferences
    , dsiOperationId
    , dsiStackSetName
    , dsiAccounts
    , dsiRegions
    , dsiRetainStacks

    -- * Destructuring the Response
    , deleteStackInstancesResponse
    , DeleteStackInstancesResponse
    -- * Response Lenses
    , dsirsOperationId
    , dsirsResponseStatus
    ) where

import Network.AWS.CloudFormation.Types
import Network.AWS.CloudFormation.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteStackInstances' smart constructor.
data DeleteStackInstances = DeleteStackInstances'
  { _dsiOperationPreferences :: !(Maybe StackSetOperationPreferences)
  , _dsiOperationId          :: !(Maybe Text)
  , _dsiStackSetName         :: !Text
  , _dsiAccounts             :: ![Text]
  , _dsiRegions              :: ![Text]
  , _dsiRetainStacks         :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteStackInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsiOperationPreferences' - Preferences for how AWS CloudFormation performs this stack set operation.
--
-- * 'dsiOperationId' - The unique identifier for this stack set operation.  If you don't specify an operation ID, the SDK generates one automatically.  The operation ID also functions as an idempotency token, to ensure that AWS CloudFormation performs the stack set operation only once, even if you retry the request multiple times. You can retry stack set operation requests to ensure that AWS CloudFormation successfully received them. Repeating this stack set operation with a new operation ID retries all stack instances whose status is @OUTDATED@ .
--
-- * 'dsiStackSetName' - The name or unique ID of the stack set that you want to delete stack instances for.
--
-- * 'dsiAccounts' - The names of the AWS accounts that you want to delete stack instances for.
--
-- * 'dsiRegions' - The regions where you want to delete stack set instances.
--
-- * 'dsiRetainStacks' - Removes the stack instances from the specified stack set, but doesn't delete the stacks. You can't reassociate a retained stack or add an existing, saved stack to a new stack set. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-ops-options Stack set operation options> .
deleteStackInstances
    :: Text -- ^ 'dsiStackSetName'
    -> Bool -- ^ 'dsiRetainStacks'
    -> DeleteStackInstances
deleteStackInstances pStackSetName_ pRetainStacks_ =
  DeleteStackInstances'
    { _dsiOperationPreferences = Nothing
    , _dsiOperationId = Nothing
    , _dsiStackSetName = pStackSetName_
    , _dsiAccounts = mempty
    , _dsiRegions = mempty
    , _dsiRetainStacks = pRetainStacks_
    }


-- | Preferences for how AWS CloudFormation performs this stack set operation.
dsiOperationPreferences :: Lens' DeleteStackInstances (Maybe StackSetOperationPreferences)
dsiOperationPreferences = lens _dsiOperationPreferences (\ s a -> s{_dsiOperationPreferences = a})

-- | The unique identifier for this stack set operation.  If you don't specify an operation ID, the SDK generates one automatically.  The operation ID also functions as an idempotency token, to ensure that AWS CloudFormation performs the stack set operation only once, even if you retry the request multiple times. You can retry stack set operation requests to ensure that AWS CloudFormation successfully received them. Repeating this stack set operation with a new operation ID retries all stack instances whose status is @OUTDATED@ .
dsiOperationId :: Lens' DeleteStackInstances (Maybe Text)
dsiOperationId = lens _dsiOperationId (\ s a -> s{_dsiOperationId = a})

-- | The name or unique ID of the stack set that you want to delete stack instances for.
dsiStackSetName :: Lens' DeleteStackInstances Text
dsiStackSetName = lens _dsiStackSetName (\ s a -> s{_dsiStackSetName = a})

-- | The names of the AWS accounts that you want to delete stack instances for.
dsiAccounts :: Lens' DeleteStackInstances [Text]
dsiAccounts = lens _dsiAccounts (\ s a -> s{_dsiAccounts = a}) . _Coerce

-- | The regions where you want to delete stack set instances.
dsiRegions :: Lens' DeleteStackInstances [Text]
dsiRegions = lens _dsiRegions (\ s a -> s{_dsiRegions = a}) . _Coerce

-- | Removes the stack instances from the specified stack set, but doesn't delete the stacks. You can't reassociate a retained stack or add an existing, saved stack to a new stack set. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-ops-options Stack set operation options> .
dsiRetainStacks :: Lens' DeleteStackInstances Bool
dsiRetainStacks = lens _dsiRetainStacks (\ s a -> s{_dsiRetainStacks = a})

instance AWSRequest DeleteStackInstances where
        type Rs DeleteStackInstances =
             DeleteStackInstancesResponse
        request = postQuery cloudFormation
        response
          = receiveXMLWrapper "DeleteStackInstancesResult"
              (\ s h x ->
                 DeleteStackInstancesResponse' <$>
                   (x .@? "OperationId") <*> (pure (fromEnum s)))

instance Hashable DeleteStackInstances where

instance NFData DeleteStackInstances where

instance ToHeaders DeleteStackInstances where
        toHeaders = const mempty

instance ToPath DeleteStackInstances where
        toPath = const "/"

instance ToQuery DeleteStackInstances where
        toQuery DeleteStackInstances'{..}
          = mconcat
              ["Action" =: ("DeleteStackInstances" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "OperationPreferences" =: _dsiOperationPreferences,
               "OperationId" =: _dsiOperationId,
               "StackSetName" =: _dsiStackSetName,
               "Accounts" =: toQueryList "member" _dsiAccounts,
               "Regions" =: toQueryList "member" _dsiRegions,
               "RetainStacks" =: _dsiRetainStacks]

-- | /See:/ 'deleteStackInstancesResponse' smart constructor.
data DeleteStackInstancesResponse = DeleteStackInstancesResponse'
  { _dsirsOperationId    :: !(Maybe Text)
  , _dsirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteStackInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsirsOperationId' - The unique identifier for this stack set operation.
--
-- * 'dsirsResponseStatus' - -- | The response status code.
deleteStackInstancesResponse
    :: Int -- ^ 'dsirsResponseStatus'
    -> DeleteStackInstancesResponse
deleteStackInstancesResponse pResponseStatus_ =
  DeleteStackInstancesResponse'
    {_dsirsOperationId = Nothing, _dsirsResponseStatus = pResponseStatus_}


-- | The unique identifier for this stack set operation.
dsirsOperationId :: Lens' DeleteStackInstancesResponse (Maybe Text)
dsirsOperationId = lens _dsirsOperationId (\ s a -> s{_dsirsOperationId = a})

-- | -- | The response status code.
dsirsResponseStatus :: Lens' DeleteStackInstancesResponse Int
dsirsResponseStatus = lens _dsirsResponseStatus (\ s a -> s{_dsirsResponseStatus = a})

instance NFData DeleteStackInstancesResponse where
