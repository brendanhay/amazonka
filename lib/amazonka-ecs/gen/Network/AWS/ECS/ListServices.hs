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
-- Module      : Network.AWS.ECS.ListServices
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the services that are running in a specified cluster.
--
--
--
-- This operation returns paginated results.
module Network.AWS.ECS.ListServices
    (
    -- * Creating a Request
      listServices
    , ListServices
    -- * Request Lenses
    , lsCluster
    , lsNextToken
    , lsLaunchType
    , lsMaxResults

    -- * Destructuring the Response
    , listServicesResponse
    , ListServicesResponse
    -- * Response Lenses
    , lsrsServiceARNs
    , lsrsNextToken
    , lsrsResponseStatus
    ) where

import Network.AWS.ECS.Types
import Network.AWS.ECS.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listServices' smart constructor.
data ListServices = ListServices'
  { _lsCluster    :: !(Maybe Text)
  , _lsNextToken  :: !(Maybe Text)
  , _lsLaunchType :: !(Maybe LaunchType)
  , _lsMaxResults :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListServices' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsCluster' - The short name or full Amazon Resource Name (ARN) of the cluster that hosts the services to list. If you do not specify a cluster, the default cluster is assumed.
--
-- * 'lsNextToken' - The @nextToken@ value returned from a previous paginated @ListServices@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value.
--
-- * 'lsLaunchType' - The launch type for services you want to list.
--
-- * 'lsMaxResults' - The maximum number of service results returned by @ListServices@ in paginated output. When this parameter is used, @ListServices@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListServices@ request with the returned @nextToken@ value. This value can be between 1 and 10. If this parameter is not used, then @ListServices@ returns up to 10 results and a @nextToken@ value if applicable.
listServices
    :: ListServices
listServices =
  ListServices'
    { _lsCluster = Nothing
    , _lsNextToken = Nothing
    , _lsLaunchType = Nothing
    , _lsMaxResults = Nothing
    }


-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the services to list. If you do not specify a cluster, the default cluster is assumed.
lsCluster :: Lens' ListServices (Maybe Text)
lsCluster = lens _lsCluster (\ s a -> s{_lsCluster = a})

-- | The @nextToken@ value returned from a previous paginated @ListServices@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value.
lsNextToken :: Lens' ListServices (Maybe Text)
lsNextToken = lens _lsNextToken (\ s a -> s{_lsNextToken = a})

-- | The launch type for services you want to list.
lsLaunchType :: Lens' ListServices (Maybe LaunchType)
lsLaunchType = lens _lsLaunchType (\ s a -> s{_lsLaunchType = a})

-- | The maximum number of service results returned by @ListServices@ in paginated output. When this parameter is used, @ListServices@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListServices@ request with the returned @nextToken@ value. This value can be between 1 and 10. If this parameter is not used, then @ListServices@ returns up to 10 results and a @nextToken@ value if applicable.
lsMaxResults :: Lens' ListServices (Maybe Int)
lsMaxResults = lens _lsMaxResults (\ s a -> s{_lsMaxResults = a})

instance AWSPager ListServices where
        page rq rs
          | stop (rs ^. lsrsNextToken) = Nothing
          | stop (rs ^. lsrsServiceARNs) = Nothing
          | otherwise =
            Just $ rq & lsNextToken .~ rs ^. lsrsNextToken

instance AWSRequest ListServices where
        type Rs ListServices = ListServicesResponse
        request = postJSON ecs
        response
          = receiveJSON
              (\ s h x ->
                 ListServicesResponse' <$>
                   (x .?> "serviceArns" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListServices where

instance NFData ListServices where

instance ToHeaders ListServices where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerServiceV20141113.ListServices" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListServices where
        toJSON ListServices'{..}
          = object
              (catMaybes
                 [("cluster" .=) <$> _lsCluster,
                  ("nextToken" .=) <$> _lsNextToken,
                  ("launchType" .=) <$> _lsLaunchType,
                  ("maxResults" .=) <$> _lsMaxResults])

instance ToPath ListServices where
        toPath = const "/"

instance ToQuery ListServices where
        toQuery = const mempty

-- | /See:/ 'listServicesResponse' smart constructor.
data ListServicesResponse = ListServicesResponse'
  { _lsrsServiceARNs    :: !(Maybe [Text])
  , _lsrsNextToken      :: !(Maybe Text)
  , _lsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListServicesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsrsServiceARNs' - The list of full ARN entries for each service associated with the specified cluster.
--
-- * 'lsrsNextToken' - The @nextToken@ value to include in a future @ListServices@ request. When the results of a @ListServices@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'lsrsResponseStatus' - -- | The response status code.
listServicesResponse
    :: Int -- ^ 'lsrsResponseStatus'
    -> ListServicesResponse
listServicesResponse pResponseStatus_ =
  ListServicesResponse'
    { _lsrsServiceARNs = Nothing
    , _lsrsNextToken = Nothing
    , _lsrsResponseStatus = pResponseStatus_
    }


-- | The list of full ARN entries for each service associated with the specified cluster.
lsrsServiceARNs :: Lens' ListServicesResponse [Text]
lsrsServiceARNs = lens _lsrsServiceARNs (\ s a -> s{_lsrsServiceARNs = a}) . _Default . _Coerce

-- | The @nextToken@ value to include in a future @ListServices@ request. When the results of a @ListServices@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
lsrsNextToken :: Lens' ListServicesResponse (Maybe Text)
lsrsNextToken = lens _lsrsNextToken (\ s a -> s{_lsrsNextToken = a})

-- | -- | The response status code.
lsrsResponseStatus :: Lens' ListServicesResponse Int
lsrsResponseStatus = lens _lsrsResponseStatus (\ s a -> s{_lsrsResponseStatus = a})

instance NFData ListServicesResponse where
