{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CodeDeploy.ListDeployments
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

-- | Lists the deployments within a deployment group for an application
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
    , ldrNextToken
    , ldrDeployments
    ) where

import Network.AWS.CodeDeploy.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listDeployments' smart constructor.
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
data ListDeployments = ListDeployments'{_ldCreateTimeRange :: Maybe TimeRange, _ldNextToken :: Maybe Text, _ldIncludeOnlyStatuses :: Maybe [DeploymentStatus], _ldApplicationName :: Maybe Text, _ldDeploymentGroupName :: Maybe Text} deriving (Eq, Read, Show)

-- | 'ListDeployments' smart constructor.
listDeployments :: ListDeployments
listDeployments = ListDeployments'{_ldCreateTimeRange = Nothing, _ldNextToken = Nothing, _ldIncludeOnlyStatuses = Nothing, _ldApplicationName = Nothing, _ldDeploymentGroupName = Nothing};

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
ldIncludeOnlyStatuses = lens _ldIncludeOnlyStatuses (\ s a -> s{_ldIncludeOnlyStatuses = a}) . _Default;

-- | The name of an existing AWS CodeDeploy application associated with the
-- applicable IAM user or AWS account.
ldApplicationName :: Lens' ListDeployments (Maybe Text)
ldApplicationName = lens _ldApplicationName (\ s a -> s{_ldApplicationName = a});

-- | The name of an existing deployment group for the specified application.
ldDeploymentGroupName :: Lens' ListDeployments (Maybe Text)
ldDeploymentGroupName = lens _ldDeploymentGroupName (\ s a -> s{_ldDeploymentGroupName = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest ListDeployments where
        type Sv ListDeployments = CodeDeploy
        type Rs ListDeployments = ListDeploymentsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListDeploymentsResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "deployments" .!@ mempty))

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
        toPath = const "/"

instance ToQuery ListDeployments where
        toQuery = const mempty

-- | /See:/ 'listDeploymentsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldrNextToken'
--
-- * 'ldrDeployments'
data ListDeploymentsResponse = ListDeploymentsResponse'{_ldrNextToken :: Maybe Text, _ldrDeployments :: Maybe [Text]} deriving (Eq, Read, Show)

-- | 'ListDeploymentsResponse' smart constructor.
listDeploymentsResponse :: ListDeploymentsResponse
listDeploymentsResponse = ListDeploymentsResponse'{_ldrNextToken = Nothing, _ldrDeployments = Nothing};

-- | If the amount of information that is returned is significantly large, an
-- identifier will also be returned, which can be used in a subsequent list
-- deployments call to return the next set of deployments in the list.
ldrNextToken :: Lens' ListDeploymentsResponse (Maybe Text)
ldrNextToken = lens _ldrNextToken (\ s a -> s{_ldrNextToken = a});

-- | A list of deployment IDs.
ldrDeployments :: Lens' ListDeploymentsResponse [Text]
ldrDeployments = lens _ldrDeployments (\ s a -> s{_ldrDeployments = a}) . _Default;
