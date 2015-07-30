{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.ListDeployments
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Lists the deployments within a deployment group for an application
-- registered with the applicable IAM user or AWS account.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_ListDeployments.html>
module Network.AWS.CodeDeploy.ListDeployments
    (
    -- * Request
      ListDeployments
    -- ** Request constructor
    , listDeployments
    -- ** Request lenses
    , ldCreateTimeRange
    , ldNextToken
    , ldIncludeOnlyStatuses
    , ldApplicationName
    , ldDeploymentGroupName

    -- * Response
    , ListDeploymentsResponse
    -- ** Response constructor
    , listDeploymentsResponse
    -- ** Response lenses
    , ldrsNextToken
    , ldrsDeployments
    , ldrsStatus
    ) where

import           Network.AWS.CodeDeploy.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a list deployments operation.
--
-- /See:/ 'listDeployments' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldCreateTimeRange'
--
-- * 'ldNextToken'
--
-- * 'ldIncludeOnlyStatuses'
--
-- * 'ldApplicationName'
--
-- * 'ldDeploymentGroupName'
data ListDeployments = ListDeployments'
    { _ldCreateTimeRange     :: !(Maybe TimeRange)
    , _ldNextToken           :: !(Maybe Text)
    , _ldIncludeOnlyStatuses :: !(Maybe [DeploymentStatus])
    , _ldApplicationName     :: !(Maybe Text)
    , _ldDeploymentGroupName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListDeployments' smart constructor.
listDeployments :: ListDeployments
listDeployments =
    ListDeployments'
    { _ldCreateTimeRange = Nothing
    , _ldNextToken = Nothing
    , _ldIncludeOnlyStatuses = Nothing
    , _ldApplicationName = Nothing
    , _ldDeploymentGroupName = Nothing
    }

-- | A deployment creation start- and end-time range for returning a subset
-- of the list of deployments.
ldCreateTimeRange :: Lens' ListDeployments (Maybe TimeRange)
ldCreateTimeRange = lens _ldCreateTimeRange (\ s a -> s{_ldCreateTimeRange = a});

-- | An identifier that was returned from the previous list deployments call,
-- which can be used to return the next set of deployments in the list.
ldNextToken :: Lens' ListDeployments (Maybe Text)
ldNextToken = lens _ldNextToken (\ s a -> s{_ldNextToken = a});

-- | A subset of deployments to list, by status:
--
-- -   Created: Include in the resulting list created deployments.
-- -   Queued: Include in the resulting list queued deployments.
-- -   In Progress: Include in the resulting list in-progress deployments.
-- -   Succeeded: Include in the resulting list succeeded deployments.
-- -   Failed: Include in the resulting list failed deployments.
-- -   Aborted: Include in the resulting list aborted deployments.
ldIncludeOnlyStatuses :: Lens' ListDeployments [DeploymentStatus]
ldIncludeOnlyStatuses = lens _ldIncludeOnlyStatuses (\ s a -> s{_ldIncludeOnlyStatuses = a}) . _Default . _Coerce;

-- | The name of an existing AWS CodeDeploy application associated with the
-- applicable IAM user or AWS account.
ldApplicationName :: Lens' ListDeployments (Maybe Text)
ldApplicationName = lens _ldApplicationName (\ s a -> s{_ldApplicationName = a});

-- | The name of an existing deployment group for the specified application.
ldDeploymentGroupName :: Lens' ListDeployments (Maybe Text)
ldDeploymentGroupName = lens _ldDeploymentGroupName (\ s a -> s{_ldDeploymentGroupName = a});

instance AWSRequest ListDeployments where
        type Sv ListDeployments = CodeDeploy
        type Rs ListDeployments = ListDeploymentsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListDeploymentsResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "deployments" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance ToHeaders ListDeployments where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.ListDeployments" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListDeployments where
        toJSON ListDeployments'{..}
          = object
              ["createTimeRange" .= _ldCreateTimeRange,
               "nextToken" .= _ldNextToken,
               "includeOnlyStatuses" .= _ldIncludeOnlyStatuses,
               "applicationName" .= _ldApplicationName,
               "deploymentGroupName" .= _ldDeploymentGroupName]

instance ToPath ListDeployments where
        toPath = const mempty

instance ToQuery ListDeployments where
        toQuery = const mempty

-- | Represents the output of a list deployments operation.
--
-- /See:/ 'listDeploymentsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldrsNextToken'
--
-- * 'ldrsDeployments'
--
-- * 'ldrsStatus'
data ListDeploymentsResponse = ListDeploymentsResponse'
    { _ldrsNextToken   :: !(Maybe Text)
    , _ldrsDeployments :: !(Maybe [Text])
    , _ldrsStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListDeploymentsResponse' smart constructor.
listDeploymentsResponse :: Int -> ListDeploymentsResponse
listDeploymentsResponse pStatus_ =
    ListDeploymentsResponse'
    { _ldrsNextToken = Nothing
    , _ldrsDeployments = Nothing
    , _ldrsStatus = pStatus_
    }

-- | If the amount of information that is returned is significantly large, an
-- identifier will also be returned, which can be used in a subsequent list
-- deployments call to return the next set of deployments in the list.
ldrsNextToken :: Lens' ListDeploymentsResponse (Maybe Text)
ldrsNextToken = lens _ldrsNextToken (\ s a -> s{_ldrsNextToken = a});

-- | A list of deployment IDs.
ldrsDeployments :: Lens' ListDeploymentsResponse [Text]
ldrsDeployments = lens _ldrsDeployments (\ s a -> s{_ldrsDeployments = a}) . _Default . _Coerce;

-- | FIXME: Undocumented member.
ldrsStatus :: Lens' ListDeploymentsResponse Int
ldrsStatus = lens _ldrsStatus (\ s a -> s{_ldrsStatus = a});
