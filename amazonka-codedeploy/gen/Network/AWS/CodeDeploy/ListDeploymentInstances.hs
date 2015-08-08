{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.ListDeploymentInstances
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Lists the instances for a deployment associated with the applicable IAM
-- user or AWS account.
--
-- /See:/ <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_ListDeploymentInstances.html AWS API Reference> for ListDeploymentInstances.
module Network.AWS.CodeDeploy.ListDeploymentInstances
    (
    -- * Creating a Request
      ListDeploymentInstances
    , listDeploymentInstances
    -- * Request Lenses
    , ldiInstanceStatusFilter
    , ldiNextToken
    , ldiDeploymentId

    -- * Destructuring the Response
    , ListDeploymentInstancesResponse
    , listDeploymentInstancesResponse
    -- * Response Lenses
    , ldirsNextToken
    , ldirsInstancesList
    , ldirsStatus
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListDeploymentInstances' smart constructor.
listDeploymentInstances :: Text -> ListDeploymentInstances
listDeploymentInstances pDeploymentId_ =
    ListDeploymentInstances'
    { _ldiInstanceStatusFilter = Nothing
    , _ldiNextToken = Nothing
    , _ldiDeploymentId = pDeploymentId_
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
ldiInstanceStatusFilter = lens _ldiInstanceStatusFilter (\ s a -> s{_ldiInstanceStatusFilter = a}) . _Default . _Coerce;

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
                     <*> (pure (fromEnum s)))

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
-- * 'ldirsNextToken'
--
-- * 'ldirsInstancesList'
--
-- * 'ldirsStatus'
data ListDeploymentInstancesResponse = ListDeploymentInstancesResponse'
    { _ldirsNextToken     :: !(Maybe Text)
    , _ldirsInstancesList :: !(Maybe [Text])
    , _ldirsStatus        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListDeploymentInstancesResponse' smart constructor.
listDeploymentInstancesResponse :: Int -> ListDeploymentInstancesResponse
listDeploymentInstancesResponse pStatus_ =
    ListDeploymentInstancesResponse'
    { _ldirsNextToken = Nothing
    , _ldirsInstancesList = Nothing
    , _ldirsStatus = pStatus_
    }

-- | If the amount of information that is returned is significantly large, an
-- identifier will also be returned, which can be used in a subsequent list
-- deployment instances call to return the next set of deployment instances
-- in the list.
ldirsNextToken :: Lens' ListDeploymentInstancesResponse (Maybe Text)
ldirsNextToken = lens _ldirsNextToken (\ s a -> s{_ldirsNextToken = a});

-- | A list of instances IDs.
ldirsInstancesList :: Lens' ListDeploymentInstancesResponse [Text]
ldirsInstancesList = lens _ldirsInstancesList (\ s a -> s{_ldirsInstancesList = a}) . _Default . _Coerce;

-- | Undocumented member.
ldirsStatus :: Lens' ListDeploymentInstancesResponse Int
ldirsStatus = lens _ldirsStatus (\ s a -> s{_ldirsStatus = a});
