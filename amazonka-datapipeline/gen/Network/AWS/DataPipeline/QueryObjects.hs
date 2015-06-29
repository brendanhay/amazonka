{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.DataPipeline.QueryObjects
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

-- | Queries the specified pipeline for the names of objects that match the
-- specified set of conditions.
--
-- <http://docs.aws.amazon.com/datapipeline/latest/APIReference/API_QueryObjects.html>
module Network.AWS.DataPipeline.QueryObjects
    (
    -- * Request
      QueryObjects
    -- ** Request constructor
    , queryObjects
    -- ** Request lenses
    , qoQuery
    , qoMarker
    , qoLimit
    , qoPipelineId
    , qoSphere

    -- * Response
    , QueryObjectsResponse
    -- ** Response constructor
    , queryObjectsResponse
    -- ** Response lenses
    , qorHasMoreResults
    , qorIds
    , qorMarker
    , qorStatus
    ) where

import           Network.AWS.DataPipeline.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for QueryObjects.
--
-- /See:/ 'queryObjects' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'qoQuery'
--
-- * 'qoMarker'
--
-- * 'qoLimit'
--
-- * 'qoPipelineId'
--
-- * 'qoSphere'
data QueryObjects = QueryObjects'
    { _qoQuery      :: !(Maybe Query)
    , _qoMarker     :: !(Maybe Text)
    , _qoLimit      :: !(Maybe Int)
    , _qoPipelineId :: !Text
    , _qoSphere     :: !Text
    } deriving (Eq,Read,Show)

-- | 'QueryObjects' smart constructor.
queryObjects :: Text -> Text -> QueryObjects
queryObjects pPipelineId pSphere =
    QueryObjects'
    { _qoQuery = Nothing
    , _qoMarker = Nothing
    , _qoLimit = Nothing
    , _qoPipelineId = pPipelineId
    , _qoSphere = pSphere
    }

-- | The query that defines the objects to be returned. The @Query@ object
-- can contain a maximum of ten selectors. The conditions in the query are
-- limited to top-level String fields in the object. These filters can be
-- applied to components, instances, and attempts.
qoQuery :: Lens' QueryObjects (Maybe Query)
qoQuery = lens _qoQuery (\ s a -> s{_qoQuery = a});

-- | The starting point for the results to be returned. For the first call,
-- this value should be empty. As long as there are more results, continue
-- to call @QueryObjects@ with the marker value from the previous call to
-- retrieve the next set of results.
qoMarker :: Lens' QueryObjects (Maybe Text)
qoMarker = lens _qoMarker (\ s a -> s{_qoMarker = a});

-- | The maximum number of object names that @QueryObjects@ will return in a
-- single call. The default value is 100.
qoLimit :: Lens' QueryObjects (Maybe Int)
qoLimit = lens _qoLimit (\ s a -> s{_qoLimit = a});

-- | The ID of the pipeline.
qoPipelineId :: Lens' QueryObjects Text
qoPipelineId = lens _qoPipelineId (\ s a -> s{_qoPipelineId = a});

-- | Indicates whether the query applies to components or instances. The
-- possible values are: @COMPONENT@, @INSTANCE@, and @ATTEMPT@.
qoSphere :: Lens' QueryObjects Text
qoSphere = lens _qoSphere (\ s a -> s{_qoSphere = a});

instance AWSPager QueryObjects where
        page rq rs
          | stop (rs ^. qorHasMoreResults) = Nothing
          | isNothing (rs ^. qorMarker) = Nothing
          | otherwise = Just $ rq & qoMarker .~ rs ^. qorMarker

instance AWSRequest QueryObjects where
        type Sv QueryObjects = DataPipeline
        type Rs QueryObjects = QueryObjectsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 QueryObjectsResponse' <$>
                   (x .?> "hasMoreResults") <*> (x .?> "ids" .!@ mempty)
                     <*> (x .?> "marker")
                     <*> (pure (fromEnum s)))

instance ToHeaders QueryObjects where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DataPipeline.QueryObjects" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON QueryObjects where
        toJSON QueryObjects'{..}
          = object
              ["query" .= _qoQuery, "marker" .= _qoMarker,
               "limit" .= _qoLimit, "pipelineId" .= _qoPipelineId,
               "sphere" .= _qoSphere]

instance ToPath QueryObjects where
        toPath = const "/"

instance ToQuery QueryObjects where
        toQuery = const mempty

-- | Contains the output of QueryObjects.
--
-- /See:/ 'queryObjectsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'qorHasMoreResults'
--
-- * 'qorIds'
--
-- * 'qorMarker'
--
-- * 'qorStatus'
data QueryObjectsResponse = QueryObjectsResponse'
    { _qorHasMoreResults :: !(Maybe Bool)
    , _qorIds            :: !(Maybe [Text])
    , _qorMarker         :: !(Maybe Text)
    , _qorStatus         :: !Int
    } deriving (Eq,Read,Show)

-- | 'QueryObjectsResponse' smart constructor.
queryObjectsResponse :: Int -> QueryObjectsResponse
queryObjectsResponse pStatus =
    QueryObjectsResponse'
    { _qorHasMoreResults = Nothing
    , _qorIds = Nothing
    , _qorMarker = Nothing
    , _qorStatus = pStatus
    }

-- | Indicates whether there are more results that can be obtained by a
-- subsequent call.
qorHasMoreResults :: Lens' QueryObjectsResponse (Maybe Bool)
qorHasMoreResults = lens _qorHasMoreResults (\ s a -> s{_qorHasMoreResults = a});

-- | The identifiers that match the query selectors.
qorIds :: Lens' QueryObjectsResponse [Text]
qorIds = lens _qorIds (\ s a -> s{_qorIds = a}) . _Default;

-- | The starting point for the next page of results. To view the next page
-- of results, call @QueryObjects@ again with this marker value. If the
-- value is null, there are no more results.
qorMarker :: Lens' QueryObjectsResponse (Maybe Text)
qorMarker = lens _qorMarker (\ s a -> s{_qorMarker = a});

-- | FIXME: Undocumented member.
qorStatus :: Lens' QueryObjectsResponse Int
qorStatus = lens _qorStatus (\ s a -> s{_qorStatus = a});
