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
-- Module      : Network.AWS.CodeDeploy.ListDeploymentInstances
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the instance for a deployment associated with the applicable IAM user or AWS account.
--
--
--
-- This operation returns paginated results.
module Network.AWS.CodeDeploy.ListDeploymentInstances
    (
    -- * Creating a Request
      listDeploymentInstances
    , ListDeploymentInstances
    -- * Request Lenses
    , lInstanceStatusFilter
    , lNextToken
    , lInstanceTypeFilter
    , lDeploymentId

    -- * Destructuring the Response
    , listDeploymentInstancesResponse
    , ListDeploymentInstancesResponse
    -- * Response Lenses
    , ldirsNextToken
    , ldirsInstancesList
    , ldirsResponseStatus
    ) where

import Network.AWS.CodeDeploy.Types
import Network.AWS.CodeDeploy.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a ListDeploymentInstances operation.
--
--
--
-- /See:/ 'listDeploymentInstances' smart constructor.
data ListDeploymentInstances = ListDeploymentInstances'
  { _lInstanceStatusFilter :: !(Maybe [InstanceStatus])
  , _lNextToken            :: !(Maybe Text)
  , _lInstanceTypeFilter   :: !(Maybe [InstanceType])
  , _lDeploymentId         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDeploymentInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lInstanceStatusFilter' - A subset of instances to list by status:     * Pending: Include those instance with pending deployments.     * InProgress: Include those instance where deployments are still in progress.     * Succeeded: Include those instances with successful deployments.     * Failed: Include those instance with failed deployments.     * Skipped: Include those instance with skipped deployments.     * Unknown: Include those instance with deployments in an unknown state.
--
-- * 'lNextToken' - An identifier returned from the previous list deployment instances call. It can be used to return the next set of deployment instances in the list.
--
-- * 'lInstanceTypeFilter' - The set of instances in a blue/green deployment, either those in the original environment ("BLUE") or those in the replacement environment ("GREEN"), for which you want to view instance information.
--
-- * 'lDeploymentId' - The unique ID of a deployment.
listDeploymentInstances
    :: Text -- ^ 'lDeploymentId'
    -> ListDeploymentInstances
listDeploymentInstances pDeploymentId_ =
  ListDeploymentInstances'
    { _lInstanceStatusFilter = Nothing
    , _lNextToken = Nothing
    , _lInstanceTypeFilter = Nothing
    , _lDeploymentId = pDeploymentId_
    }


-- | A subset of instances to list by status:     * Pending: Include those instance with pending deployments.     * InProgress: Include those instance where deployments are still in progress.     * Succeeded: Include those instances with successful deployments.     * Failed: Include those instance with failed deployments.     * Skipped: Include those instance with skipped deployments.     * Unknown: Include those instance with deployments in an unknown state.
lInstanceStatusFilter :: Lens' ListDeploymentInstances [InstanceStatus]
lInstanceStatusFilter = lens _lInstanceStatusFilter (\ s a -> s{_lInstanceStatusFilter = a}) . _Default . _Coerce

-- | An identifier returned from the previous list deployment instances call. It can be used to return the next set of deployment instances in the list.
lNextToken :: Lens' ListDeploymentInstances (Maybe Text)
lNextToken = lens _lNextToken (\ s a -> s{_lNextToken = a})

-- | The set of instances in a blue/green deployment, either those in the original environment ("BLUE") or those in the replacement environment ("GREEN"), for which you want to view instance information.
lInstanceTypeFilter :: Lens' ListDeploymentInstances [InstanceType]
lInstanceTypeFilter = lens _lInstanceTypeFilter (\ s a -> s{_lInstanceTypeFilter = a}) . _Default . _Coerce

-- | The unique ID of a deployment.
lDeploymentId :: Lens' ListDeploymentInstances Text
lDeploymentId = lens _lDeploymentId (\ s a -> s{_lDeploymentId = a})

instance AWSPager ListDeploymentInstances where
        page rq rs
          | stop (rs ^. ldirsNextToken) = Nothing
          | stop (rs ^. ldirsInstancesList) = Nothing
          | otherwise =
            Just $ rq & lNextToken .~ rs ^. ldirsNextToken

instance AWSRequest ListDeploymentInstances where
        type Rs ListDeploymentInstances =
             ListDeploymentInstancesResponse
        request = postJSON codeDeploy
        response
          = receiveJSON
              (\ s h x ->
                 ListDeploymentInstancesResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "instancesList" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListDeploymentInstances where

instance NFData ListDeploymentInstances where

instance ToHeaders ListDeploymentInstances where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.ListDeploymentInstances" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListDeploymentInstances where
        toJSON ListDeploymentInstances'{..}
          = object
              (catMaybes
                 [("instanceStatusFilter" .=) <$>
                    _lInstanceStatusFilter,
                  ("nextToken" .=) <$> _lNextToken,
                  ("instanceTypeFilter" .=) <$> _lInstanceTypeFilter,
                  Just ("deploymentId" .= _lDeploymentId)])

instance ToPath ListDeploymentInstances where
        toPath = const "/"

instance ToQuery ListDeploymentInstances where
        toQuery = const mempty

-- | Represents the output of a ListDeploymentInstances operation.
--
--
--
-- /See:/ 'listDeploymentInstancesResponse' smart constructor.
data ListDeploymentInstancesResponse = ListDeploymentInstancesResponse'
  { _ldirsNextToken      :: !(Maybe Text)
  , _ldirsInstancesList  :: !(Maybe [Text])
  , _ldirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDeploymentInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldirsNextToken' - If a large amount of information is returned, an identifier is also returned. It can be used in a subsequent list deployment instances call to return the next set of deployment instances in the list.
--
-- * 'ldirsInstancesList' - A list of instance IDs.
--
-- * 'ldirsResponseStatus' - -- | The response status code.
listDeploymentInstancesResponse
    :: Int -- ^ 'ldirsResponseStatus'
    -> ListDeploymentInstancesResponse
listDeploymentInstancesResponse pResponseStatus_ =
  ListDeploymentInstancesResponse'
    { _ldirsNextToken = Nothing
    , _ldirsInstancesList = Nothing
    , _ldirsResponseStatus = pResponseStatus_
    }


-- | If a large amount of information is returned, an identifier is also returned. It can be used in a subsequent list deployment instances call to return the next set of deployment instances in the list.
ldirsNextToken :: Lens' ListDeploymentInstancesResponse (Maybe Text)
ldirsNextToken = lens _ldirsNextToken (\ s a -> s{_ldirsNextToken = a})

-- | A list of instance IDs.
ldirsInstancesList :: Lens' ListDeploymentInstancesResponse [Text]
ldirsInstancesList = lens _ldirsInstancesList (\ s a -> s{_ldirsInstancesList = a}) . _Default . _Coerce

-- | -- | The response status code.
ldirsResponseStatus :: Lens' ListDeploymentInstancesResponse Int
ldirsResponseStatus = lens _ldirsResponseStatus (\ s a -> s{_ldirsResponseStatus = a})

instance NFData ListDeploymentInstancesResponse where
