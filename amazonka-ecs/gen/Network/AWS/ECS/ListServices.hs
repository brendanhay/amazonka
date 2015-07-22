{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.ListServices
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Lists the services that are running in a specified cluster.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_ListServices.html>
module Network.AWS.ECS.ListServices
    (
    -- * Request
      ListServices
    -- ** Request constructor
    , listServices
    -- ** Request lenses
    , lsrqCluster
    , lsrqNextToken
    , lsrqMaxResults

    -- * Response
    , ListServicesResponse
    -- ** Response constructor
    , listServicesResponse
    -- ** Response lenses
    , lsrsServiceARNs
    , lsrsNextToken
    , lsrsStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listServices' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsrqCluster'
--
-- * 'lsrqNextToken'
--
-- * 'lsrqMaxResults'
data ListServices = ListServices'
    { _lsrqCluster    :: !(Maybe Text)
    , _lsrqNextToken  :: !(Maybe Text)
    , _lsrqMaxResults :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListServices' smart constructor.
listServices :: ListServices
listServices =
    ListServices'
    { _lsrqCluster = Nothing
    , _lsrqNextToken = Nothing
    , _lsrqMaxResults = Nothing
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the services you want to list. If you do not specify a cluster,
-- the default cluster is assumed..
lsrqCluster :: Lens' ListServices (Maybe Text)
lsrqCluster = lens _lsrqCluster (\ s a -> s{_lsrqCluster = a});

-- | The @nextToken@ value returned from a previous paginated @ListServices@
-- request where @maxResults@ was used and the results exceeded the value
-- of that parameter. Pagination continues from the end of the previous
-- results that returned the @nextToken@ value. This value is @null@ when
-- there are no more results to return.
lsrqNextToken :: Lens' ListServices (Maybe Text)
lsrqNextToken = lens _lsrqNextToken (\ s a -> s{_lsrqNextToken = a});

-- | The maximum number of container instance results returned by
-- @ListServices@ in paginated output. When this parameter is used,
-- @ListServices@ only returns @maxResults@ results in a single page along
-- with a @nextToken@ response element. The remaining results of the
-- initial request can be seen by sending another @ListServices@ request
-- with the returned @nextToken@ value. This value can be between 1 and
-- 100. If this parameter is not used, then @ListServices@ returns up to
-- 100 results and a @nextToken@ value if applicable.
lsrqMaxResults :: Lens' ListServices (Maybe Int)
lsrqMaxResults = lens _lsrqMaxResults (\ s a -> s{_lsrqMaxResults = a});

instance AWSPager ListServices where
        page rq rs
          | stop (rs ^. lsrsNextToken) = Nothing
          | stop (rs ^. lsrsServiceARNs) = Nothing
          | otherwise =
            Just $ rq & lsrqNextToken .~ rs ^. lsrsNextToken

instance AWSRequest ListServices where
        type Sv ListServices = ECS
        type Rs ListServices = ListServicesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListServicesResponse' <$>
                   (x .?> "serviceArns" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

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
              ["cluster" .= _lsrqCluster,
               "nextToken" .= _lsrqNextToken,
               "maxResults" .= _lsrqMaxResults]

instance ToPath ListServices where
        toPath = const "/"

instance ToQuery ListServices where
        toQuery = const mempty

-- | /See:/ 'listServicesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsrsServiceARNs'
--
-- * 'lsrsNextToken'
--
-- * 'lsrsStatus'
data ListServicesResponse = ListServicesResponse'
    { _lsrsServiceARNs :: !(Maybe [Text])
    , _lsrsNextToken   :: !(Maybe Text)
    , _lsrsStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListServicesResponse' smart constructor.
listServicesResponse :: Int -> ListServicesResponse
listServicesResponse pStatus =
    ListServicesResponse'
    { _lsrsServiceARNs = Nothing
    , _lsrsNextToken = Nothing
    , _lsrsStatus = pStatus
    }

-- | The list of full Amazon Resource Name (ARN) entries for each service
-- associated with the specified cluster.
lsrsServiceARNs :: Lens' ListServicesResponse [Text]
lsrsServiceARNs = lens _lsrsServiceARNs (\ s a -> s{_lsrsServiceARNs = a}) . _Default;

-- | The @nextToken@ value to include in a future @ListServices@ request.
-- When the results of a @ListServices@ request exceed @maxResults@, this
-- value can be used to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
lsrsNextToken :: Lens' ListServicesResponse (Maybe Text)
lsrsNextToken = lens _lsrsNextToken (\ s a -> s{_lsrsNextToken = a});

-- | FIXME: Undocumented member.
lsrsStatus :: Lens' ListServicesResponse Int
lsrsStatus = lens _lsrsStatus (\ s a -> s{_lsrsStatus = a});
