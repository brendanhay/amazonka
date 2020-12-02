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
-- Module      : Network.AWS.CodeDeploy.ListDeployments
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the deployments in a deployment group for an application registered with the applicable IAM user or AWS account.
--
--
--
-- This operation returns paginated results.
module Network.AWS.CodeDeploy.ListDeployments
    (
    -- * Creating a Request
      listDeployments
    , ListDeployments
    -- * Request Lenses
    , ldCreateTimeRange
    , ldNextToken
    , ldIncludeOnlyStatuses
    , ldApplicationName
    , ldDeploymentGroupName

    -- * Destructuring the Response
    , listDeploymentsResponse
    , ListDeploymentsResponse
    -- * Response Lenses
    , ldrsNextToken
    , ldrsDeployments
    , ldrsResponseStatus
    ) where

import Network.AWS.CodeDeploy.Types
import Network.AWS.CodeDeploy.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a ListDeployments operation.
--
--
--
-- /See:/ 'listDeployments' smart constructor.
data ListDeployments = ListDeployments'
  { _ldCreateTimeRange     :: !(Maybe TimeRange)
  , _ldNextToken           :: !(Maybe Text)
  , _ldIncludeOnlyStatuses :: !(Maybe [DeploymentStatus])
  , _ldApplicationName     :: !(Maybe Text)
  , _ldDeploymentGroupName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDeployments' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldCreateTimeRange' - A time range (start and end) for returning a subset of the list of deployments.
--
-- * 'ldNextToken' - An identifier returned from the previous list deployments call. It can be used to return the next set of deployments in the list.
--
-- * 'ldIncludeOnlyStatuses' - A subset of deployments to list by status:     * Created: Include created deployments in the resulting list.     * Queued: Include queued deployments in the resulting list.     * In Progress: Include in-progress deployments in the resulting list.     * Succeeded: Include successful deployments in the resulting list.     * Failed: Include failed deployments in the resulting list.     * Stopped: Include stopped deployments in the resulting list.
--
-- * 'ldApplicationName' - The name of an AWS CodeDeploy application associated with the applicable IAM user or AWS account.
--
-- * 'ldDeploymentGroupName' - The name of an existing deployment group for the specified application.
listDeployments
    :: ListDeployments
listDeployments =
  ListDeployments'
    { _ldCreateTimeRange = Nothing
    , _ldNextToken = Nothing
    , _ldIncludeOnlyStatuses = Nothing
    , _ldApplicationName = Nothing
    , _ldDeploymentGroupName = Nothing
    }


-- | A time range (start and end) for returning a subset of the list of deployments.
ldCreateTimeRange :: Lens' ListDeployments (Maybe TimeRange)
ldCreateTimeRange = lens _ldCreateTimeRange (\ s a -> s{_ldCreateTimeRange = a})

-- | An identifier returned from the previous list deployments call. It can be used to return the next set of deployments in the list.
ldNextToken :: Lens' ListDeployments (Maybe Text)
ldNextToken = lens _ldNextToken (\ s a -> s{_ldNextToken = a})

-- | A subset of deployments to list by status:     * Created: Include created deployments in the resulting list.     * Queued: Include queued deployments in the resulting list.     * In Progress: Include in-progress deployments in the resulting list.     * Succeeded: Include successful deployments in the resulting list.     * Failed: Include failed deployments in the resulting list.     * Stopped: Include stopped deployments in the resulting list.
ldIncludeOnlyStatuses :: Lens' ListDeployments [DeploymentStatus]
ldIncludeOnlyStatuses = lens _ldIncludeOnlyStatuses (\ s a -> s{_ldIncludeOnlyStatuses = a}) . _Default . _Coerce

-- | The name of an AWS CodeDeploy application associated with the applicable IAM user or AWS account.
ldApplicationName :: Lens' ListDeployments (Maybe Text)
ldApplicationName = lens _ldApplicationName (\ s a -> s{_ldApplicationName = a})

-- | The name of an existing deployment group for the specified application.
ldDeploymentGroupName :: Lens' ListDeployments (Maybe Text)
ldDeploymentGroupName = lens _ldDeploymentGroupName (\ s a -> s{_ldDeploymentGroupName = a})

instance AWSPager ListDeployments where
        page rq rs
          | stop (rs ^. ldrsNextToken) = Nothing
          | stop (rs ^. ldrsDeployments) = Nothing
          | otherwise =
            Just $ rq & ldNextToken .~ rs ^. ldrsNextToken

instance AWSRequest ListDeployments where
        type Rs ListDeployments = ListDeploymentsResponse
        request = postJSON codeDeploy
        response
          = receiveJSON
              (\ s h x ->
                 ListDeploymentsResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "deployments" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListDeployments where

instance NFData ListDeployments where

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
              (catMaybes
                 [("createTimeRange" .=) <$> _ldCreateTimeRange,
                  ("nextToken" .=) <$> _ldNextToken,
                  ("includeOnlyStatuses" .=) <$>
                    _ldIncludeOnlyStatuses,
                  ("applicationName" .=) <$> _ldApplicationName,
                  ("deploymentGroupName" .=) <$>
                    _ldDeploymentGroupName])

instance ToPath ListDeployments where
        toPath = const "/"

instance ToQuery ListDeployments where
        toQuery = const mempty

-- | Represents the output of a ListDeployments operation.
--
--
--
-- /See:/ 'listDeploymentsResponse' smart constructor.
data ListDeploymentsResponse = ListDeploymentsResponse'
  { _ldrsNextToken      :: !(Maybe Text)
  , _ldrsDeployments    :: !(Maybe [Text])
  , _ldrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDeploymentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldrsNextToken' - If a large amount of information is returned, an identifier is also returned. It can be used in a subsequent list deployments call to return the next set of deployments in the list.
--
-- * 'ldrsDeployments' - A list of deployment IDs.
--
-- * 'ldrsResponseStatus' - -- | The response status code.
listDeploymentsResponse
    :: Int -- ^ 'ldrsResponseStatus'
    -> ListDeploymentsResponse
listDeploymentsResponse pResponseStatus_ =
  ListDeploymentsResponse'
    { _ldrsNextToken = Nothing
    , _ldrsDeployments = Nothing
    , _ldrsResponseStatus = pResponseStatus_
    }


-- | If a large amount of information is returned, an identifier is also returned. It can be used in a subsequent list deployments call to return the next set of deployments in the list.
ldrsNextToken :: Lens' ListDeploymentsResponse (Maybe Text)
ldrsNextToken = lens _ldrsNextToken (\ s a -> s{_ldrsNextToken = a})

-- | A list of deployment IDs.
ldrsDeployments :: Lens' ListDeploymentsResponse [Text]
ldrsDeployments = lens _ldrsDeployments (\ s a -> s{_ldrsDeployments = a}) . _Default . _Coerce

-- | -- | The response status code.
ldrsResponseStatus :: Lens' ListDeploymentsResponse Int
ldrsResponseStatus = lens _ldrsResponseStatus (\ s a -> s{_ldrsResponseStatus = a})

instance NFData ListDeploymentsResponse where
