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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
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
    , ldiInstanceStatusFilter
    , ldiNextToken
    , ldiInstanceTypeFilter
    , ldiDeploymentId

    -- * Destructuring the Response
    , listDeploymentInstancesResponse
    , ListDeploymentInstancesResponse
    -- * Response Lenses
    , ldirsNextToken
    , ldirsInstancesList
    , ldirsResponseStatus
    ) where

import           Network.AWS.CodeDeploy.Types
import           Network.AWS.CodeDeploy.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a list deployment instances operation.
--
--
--
-- /See:/ 'listDeploymentInstances' smart constructor.
data ListDeploymentInstances = ListDeploymentInstances'
    { _ldiInstanceStatusFilter :: !(Maybe [InstanceStatus])
    , _ldiNextToken            :: !(Maybe Text)
    , _ldiInstanceTypeFilter   :: !(Maybe [InstanceType])
    , _ldiDeploymentId         :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListDeploymentInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldiInstanceStatusFilter' - A subset of instances to list by status:     * Pending: Include those instance with pending deployments.     * InProgress: Include those instance where deployments are still in progress.     * Succeeded: Include those instances with successful deployments.     * Failed: Include those instance with failed deployments.     * Skipped: Include those instance with skipped deployments.     * Unknown: Include those instance with deployments in an unknown state.
--
-- * 'ldiNextToken' - An identifier returned from the previous list deployment instances call. It can be used to return the next set of deployment instances in the list.
--
-- * 'ldiInstanceTypeFilter' - The set of instances in a blue/green deployment, either those in the original environment ("BLUE") or those in the replacement environment ("GREEN"), for which you want to view instance information.
--
-- * 'ldiDeploymentId' - The unique ID of a deployment.
listDeploymentInstances
    :: Text -- ^ 'ldiDeploymentId'
    -> ListDeploymentInstances
listDeploymentInstances pDeploymentId_ =
    ListDeploymentInstances'
    { _ldiInstanceStatusFilter = Nothing
    , _ldiNextToken = Nothing
    , _ldiInstanceTypeFilter = Nothing
    , _ldiDeploymentId = pDeploymentId_
    }

-- | A subset of instances to list by status:     * Pending: Include those instance with pending deployments.     * InProgress: Include those instance where deployments are still in progress.     * Succeeded: Include those instances with successful deployments.     * Failed: Include those instance with failed deployments.     * Skipped: Include those instance with skipped deployments.     * Unknown: Include those instance with deployments in an unknown state.
ldiInstanceStatusFilter :: Lens' ListDeploymentInstances [InstanceStatus]
ldiInstanceStatusFilter = lens _ldiInstanceStatusFilter (\ s a -> s{_ldiInstanceStatusFilter = a}) . _Default . _Coerce;

-- | An identifier returned from the previous list deployment instances call. It can be used to return the next set of deployment instances in the list.
ldiNextToken :: Lens' ListDeploymentInstances (Maybe Text)
ldiNextToken = lens _ldiNextToken (\ s a -> s{_ldiNextToken = a});

-- | The set of instances in a blue/green deployment, either those in the original environment ("BLUE") or those in the replacement environment ("GREEN"), for which you want to view instance information.
ldiInstanceTypeFilter :: Lens' ListDeploymentInstances [InstanceType]
ldiInstanceTypeFilter = lens _ldiInstanceTypeFilter (\ s a -> s{_ldiInstanceTypeFilter = a}) . _Default . _Coerce;

-- | The unique ID of a deployment.
ldiDeploymentId :: Lens' ListDeploymentInstances Text
ldiDeploymentId = lens _ldiDeploymentId (\ s a -> s{_ldiDeploymentId = a});

instance AWSPager ListDeploymentInstances where
        page rq rs
          | stop (rs ^. ldirsNextToken) = Nothing
          | stop (rs ^. ldirsInstancesList) = Nothing
          | otherwise =
            Just $ rq & ldiNextToken .~ rs ^. ldirsNextToken

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

instance Hashable ListDeploymentInstances

instance NFData ListDeploymentInstances

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
                    _ldiInstanceStatusFilter,
                  ("nextToken" .=) <$> _ldiNextToken,
                  ("instanceTypeFilter" .=) <$> _ldiInstanceTypeFilter,
                  Just ("deploymentId" .= _ldiDeploymentId)])

instance ToPath ListDeploymentInstances where
        toPath = const "/"

instance ToQuery ListDeploymentInstances where
        toQuery = const mempty

-- | Represents the output of a list deployment instances operation.
--
--
--
-- /See:/ 'listDeploymentInstancesResponse' smart constructor.
data ListDeploymentInstancesResponse = ListDeploymentInstancesResponse'
    { _ldirsNextToken      :: !(Maybe Text)
    , _ldirsInstancesList  :: !(Maybe [Text])
    , _ldirsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
ldirsNextToken = lens _ldirsNextToken (\ s a -> s{_ldirsNextToken = a});

-- | A list of instance IDs.
ldirsInstancesList :: Lens' ListDeploymentInstancesResponse [Text]
ldirsInstancesList = lens _ldirsInstancesList (\ s a -> s{_ldirsInstancesList = a}) . _Default . _Coerce;

-- | -- | The response status code.
ldirsResponseStatus :: Lens' ListDeploymentInstancesResponse Int
ldirsResponseStatus = lens _ldirsResponseStatus (\ s a -> s{_ldirsResponseStatus = a});

instance NFData ListDeploymentInstancesResponse
