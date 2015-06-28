{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CodeDeploy.ListDeploymentInstances
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Lists the instances for a deployment associated with the applicable IAM
-- user or AWS account.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_ListDeploymentInstances.html>
module Network.AWS.CodeDeploy.ListDeploymentInstances
    (
    -- * Request
      ListDeploymentInstances
    -- ** Request constructor
    , listDeploymentInstances
    -- ** Request lenses
    , ldiInstanceStatusFilter
    , ldiNextToken
    , ldiDeploymentId

    -- * Response
    , ListDeploymentInstancesResponse
    -- ** Response constructor
    , listDeploymentInstancesResponse
    -- ** Response lenses
    , ldirNextToken
    , ldirInstancesList
    , ldirStatus
    ) where

import           Network.AWS.CodeDeploy.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a list deployment instances operation.
--
-- /See:/ 'listDeploymentInstances' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldiInstanceStatusFilter'
--
-- * 'ldiNextToken'
--
-- * 'ldiDeploymentId'
data ListDeploymentInstances = ListDeploymentInstances'
    { _ldiInstanceStatusFilter :: !(Maybe [InstanceStatus])
    , _ldiNextToken            :: !(Maybe Text)
    , _ldiDeploymentId         :: !Text
    } deriving (Eq,Read,Show)

-- | 'ListDeploymentInstances' smart constructor.
listDeploymentInstances :: Text -> ListDeploymentInstances
listDeploymentInstances pDeploymentId =
    ListDeploymentInstances'
    { _ldiInstanceStatusFilter = Nothing
    , _ldiNextToken = Nothing
    , _ldiDeploymentId = pDeploymentId
    }

-- | A subset of instances to list, by status:
--
-- -   Pending: Include in the resulting list those instances with pending
--     deployments.
-- -   InProgress: Include in the resulting list those instances with
--     in-progress deployments.
-- -   Succeeded: Include in the resulting list those instances with
--     succeeded deployments.
-- -   Failed: Include in the resulting list those instances with failed
--     deployments.
-- -   Skipped: Include in the resulting list those instances with skipped
--     deployments.
-- -   Unknown: Include in the resulting list those instances with
--     deployments in an unknown state.
ldiInstanceStatusFilter :: Lens' ListDeploymentInstances [InstanceStatus]
ldiInstanceStatusFilter = lens _ldiInstanceStatusFilter (\ s a -> s{_ldiInstanceStatusFilter = a}) . _Default;

-- | An identifier that was returned from the previous list deployment
-- instances call, which can be used to return the next set of deployment
-- instances in the list.
ldiNextToken :: Lens' ListDeploymentInstances (Maybe Text)
ldiNextToken = lens _ldiNextToken (\ s a -> s{_ldiNextToken = a});

-- | The unique ID of a deployment.
ldiDeploymentId :: Lens' ListDeploymentInstances Text
ldiDeploymentId = lens _ldiDeploymentId (\ s a -> s{_ldiDeploymentId = a});

instance AWSRequest ListDeploymentInstances where
        type Sv ListDeploymentInstances = CodeDeploy
        type Rs ListDeploymentInstances =
             ListDeploymentInstancesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListDeploymentInstancesResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "instancesList" .!@ mempty)
                     <*> (pure s))

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
              ["instanceStatusFilter" .= _ldiInstanceStatusFilter,
               "nextToken" .= _ldiNextToken,
               "deploymentId" .= _ldiDeploymentId]

instance ToPath ListDeploymentInstances where
        toPath = const "/"

instance ToQuery ListDeploymentInstances where
        toQuery = const mempty

-- | Represents the output of a list deployment instances operation.
--
-- /See:/ 'listDeploymentInstancesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldirNextToken'
--
-- * 'ldirInstancesList'
--
-- * 'ldirStatus'
data ListDeploymentInstancesResponse = ListDeploymentInstancesResponse'
    { _ldirNextToken     :: !(Maybe Text)
    , _ldirInstancesList :: !(Maybe [Text])
    , _ldirStatus        :: !Status
    } deriving (Eq,Show)

-- | 'ListDeploymentInstancesResponse' smart constructor.
listDeploymentInstancesResponse :: Status -> ListDeploymentInstancesResponse
listDeploymentInstancesResponse pStatus =
    ListDeploymentInstancesResponse'
    { _ldirNextToken = Nothing
    , _ldirInstancesList = Nothing
    , _ldirStatus = pStatus
    }

-- | If the amount of information that is returned is significantly large, an
-- identifier will also be returned, which can be used in a subsequent list
-- deployment instances call to return the next set of deployment instances
-- in the list.
ldirNextToken :: Lens' ListDeploymentInstancesResponse (Maybe Text)
ldirNextToken = lens _ldirNextToken (\ s a -> s{_ldirNextToken = a});

-- | A list of instances IDs.
ldirInstancesList :: Lens' ListDeploymentInstancesResponse [Text]
ldirInstancesList = lens _ldirInstancesList (\ s a -> s{_ldirInstancesList = a}) . _Default;

-- | FIXME: Undocumented member.
ldirStatus :: Lens' ListDeploymentInstancesResponse Status
ldirStatus = lens _ldirStatus (\ s a -> s{_ldirStatus = a});
