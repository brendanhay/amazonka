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
-- Module      : Network.AWS.CodeDeploy.ListDeploymentTargets
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of target IDs that are associated a deployment.
--
--
--
-- This operation returns paginated results.
module Network.AWS.CodeDeploy.ListDeploymentTargets
    (
    -- * Creating a Request
      listDeploymentTargets
    , ListDeploymentTargets
    -- * Request Lenses
    , ldtDeploymentId
    , ldtTargetFilters
    , ldtNextToken

    -- * Destructuring the Response
    , listDeploymentTargetsResponse
    , ListDeploymentTargetsResponse
    -- * Response Lenses
    , ldtrsNextToken
    , ldtrsTargetIds
    , ldtrsResponseStatus
    ) where

import Network.AWS.CodeDeploy.Types
import Network.AWS.CodeDeploy.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listDeploymentTargets' smart constructor.
data ListDeploymentTargets = ListDeploymentTargets'
  { _ldtDeploymentId  :: !(Maybe Text)
  , _ldtTargetFilters :: !(Maybe (Map TargetFilterName [Text]))
  , _ldtNextToken     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDeploymentTargets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldtDeploymentId' - The unique ID of a deployment.
--
-- * 'ldtTargetFilters' - A key used to filter the returned targets.
--
-- * 'ldtNextToken' - A token identifier returned from the previous @ListDeploymentTargets@ call. It can be used to return the next set of deployment targets in the list.
listDeploymentTargets
    :: ListDeploymentTargets
listDeploymentTargets =
  ListDeploymentTargets'
    { _ldtDeploymentId = Nothing
    , _ldtTargetFilters = Nothing
    , _ldtNextToken = Nothing
    }


-- | The unique ID of a deployment.
ldtDeploymentId :: Lens' ListDeploymentTargets (Maybe Text)
ldtDeploymentId = lens _ldtDeploymentId (\ s a -> s{_ldtDeploymentId = a})

-- | A key used to filter the returned targets.
ldtTargetFilters :: Lens' ListDeploymentTargets (HashMap TargetFilterName [Text])
ldtTargetFilters = lens _ldtTargetFilters (\ s a -> s{_ldtTargetFilters = a}) . _Default . _Map

-- | A token identifier returned from the previous @ListDeploymentTargets@ call. It can be used to return the next set of deployment targets in the list.
ldtNextToken :: Lens' ListDeploymentTargets (Maybe Text)
ldtNextToken = lens _ldtNextToken (\ s a -> s{_ldtNextToken = a})

instance AWSPager ListDeploymentTargets where
        page rq rs
          | stop (rs ^. ldtrsNextToken) = Nothing
          | stop (rs ^. ldtrsTargetIds) = Nothing
          | otherwise =
            Just $ rq & ldtNextToken .~ rs ^. ldtrsNextToken

instance AWSRequest ListDeploymentTargets where
        type Rs ListDeploymentTargets =
             ListDeploymentTargetsResponse
        request = postJSON codeDeploy
        response
          = receiveJSON
              (\ s h x ->
                 ListDeploymentTargetsResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "targetIds" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListDeploymentTargets where

instance NFData ListDeploymentTargets where

instance ToHeaders ListDeploymentTargets where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.ListDeploymentTargets" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListDeploymentTargets where
        toJSON ListDeploymentTargets'{..}
          = object
              (catMaybes
                 [("deploymentId" .=) <$> _ldtDeploymentId,
                  ("targetFilters" .=) <$> _ldtTargetFilters,
                  ("nextToken" .=) <$> _ldtNextToken])

instance ToPath ListDeploymentTargets where
        toPath = const "/"

instance ToQuery ListDeploymentTargets where
        toQuery = const mempty

-- | /See:/ 'listDeploymentTargetsResponse' smart constructor.
data ListDeploymentTargetsResponse = ListDeploymentTargetsResponse'
  { _ldtrsNextToken      :: !(Maybe Text)
  , _ldtrsTargetIds      :: !(Maybe [Text])
  , _ldtrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDeploymentTargetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldtrsNextToken' - If a large amount of information is returned, a token identifier is also returned. It can be used in a subsequent @ListDeploymentTargets@ call to return the next set of deployment targets in the list.
--
-- * 'ldtrsTargetIds' - The unique IDs of deployment targets.
--
-- * 'ldtrsResponseStatus' - -- | The response status code.
listDeploymentTargetsResponse
    :: Int -- ^ 'ldtrsResponseStatus'
    -> ListDeploymentTargetsResponse
listDeploymentTargetsResponse pResponseStatus_ =
  ListDeploymentTargetsResponse'
    { _ldtrsNextToken = Nothing
    , _ldtrsTargetIds = Nothing
    , _ldtrsResponseStatus = pResponseStatus_
    }


-- | If a large amount of information is returned, a token identifier is also returned. It can be used in a subsequent @ListDeploymentTargets@ call to return the next set of deployment targets in the list.
ldtrsNextToken :: Lens' ListDeploymentTargetsResponse (Maybe Text)
ldtrsNextToken = lens _ldtrsNextToken (\ s a -> s{_ldtrsNextToken = a})

-- | The unique IDs of deployment targets.
ldtrsTargetIds :: Lens' ListDeploymentTargetsResponse [Text]
ldtrsTargetIds = lens _ldtrsTargetIds (\ s a -> s{_ldtrsTargetIds = a}) . _Default . _Coerce

-- | -- | The response status code.
ldtrsResponseStatus :: Lens' ListDeploymentTargetsResponse Int
ldtrsResponseStatus = lens _ldtrsResponseStatus (\ s a -> s{_ldtrsResponseStatus = a})

instance NFData ListDeploymentTargetsResponse where
