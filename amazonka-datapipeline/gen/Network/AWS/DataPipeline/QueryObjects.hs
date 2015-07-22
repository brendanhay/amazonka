{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.QueryObjects
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Queries the specified pipeline for the names of objects that match the
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
    , qorqQuery
    , qorqMarker
    , qorqLimit
    , qorqPipelineId
    , qorqSphere

    -- * Response
    , QueryObjectsResponse
    -- ** Response constructor
    , queryObjectsResponse
    -- ** Response lenses
    , qorsHasMoreResults
    , qorsIds
    , qorsMarker
    , qorsStatus
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
-- * 'qorqQuery'
--
-- * 'qorqMarker'
--
-- * 'qorqLimit'
--
-- * 'qorqPipelineId'
--
-- * 'qorqSphere'
data QueryObjects = QueryObjects'
    { _qorqQuery      :: !(Maybe Query)
    , _qorqMarker     :: !(Maybe Text)
    , _qorqLimit      :: !(Maybe Int)
    , _qorqPipelineId :: !Text
    , _qorqSphere     :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'QueryObjects' smart constructor.
queryObjects :: Text -> Text -> QueryObjects
queryObjects pPipelineId pSphere =
    QueryObjects'
    { _qorqQuery = Nothing
    , _qorqMarker = Nothing
    , _qorqLimit = Nothing
    , _qorqPipelineId = pPipelineId
    , _qorqSphere = pSphere
    }

-- | The query that defines the objects to be returned. The @Query@ object
-- can contain a maximum of ten selectors. The conditions in the query are
-- limited to top-level String fields in the object. These filters can be
-- applied to components, instances, and attempts.
qorqQuery :: Lens' QueryObjects (Maybe Query)
qorqQuery = lens _qorqQuery (\ s a -> s{_qorqQuery = a});

-- | The starting point for the results to be returned. For the first call,
-- this value should be empty. As long as there are more results, continue
-- to call @QueryObjects@ with the marker value from the previous call to
-- retrieve the next set of results.
qorqMarker :: Lens' QueryObjects (Maybe Text)
qorqMarker = lens _qorqMarker (\ s a -> s{_qorqMarker = a});

-- | The maximum number of object names that @QueryObjects@ will return in a
-- single call. The default value is 100.
qorqLimit :: Lens' QueryObjects (Maybe Int)
qorqLimit = lens _qorqLimit (\ s a -> s{_qorqLimit = a});

-- | The ID of the pipeline.
qorqPipelineId :: Lens' QueryObjects Text
qorqPipelineId = lens _qorqPipelineId (\ s a -> s{_qorqPipelineId = a});

-- | Indicates whether the query applies to components or instances. The
-- possible values are: @COMPONENT@, @INSTANCE@, and @ATTEMPT@.
qorqSphere :: Lens' QueryObjects Text
qorqSphere = lens _qorqSphere (\ s a -> s{_qorqSphere = a});

instance AWSPager QueryObjects where
        page rq rs
          | stop (rs ^. qorsHasMoreResults) = Nothing
          | isNothing (rs ^. qorsMarker) = Nothing
          | otherwise =
            Just $ rq & qorqMarker .~ rs ^. qorsMarker

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
              ["query" .= _qorqQuery, "marker" .= _qorqMarker,
               "limit" .= _qorqLimit,
               "pipelineId" .= _qorqPipelineId,
               "sphere" .= _qorqSphere]

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
-- * 'qorsHasMoreResults'
--
-- * 'qorsIds'
--
-- * 'qorsMarker'
--
-- * 'qorsStatus'
data QueryObjectsResponse = QueryObjectsResponse'
    { _qorsHasMoreResults :: !(Maybe Bool)
    , _qorsIds            :: !(Maybe [Text])
    , _qorsMarker         :: !(Maybe Text)
    , _qorsStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'QueryObjectsResponse' smart constructor.
queryObjectsResponse :: Int -> QueryObjectsResponse
queryObjectsResponse pStatus =
    QueryObjectsResponse'
    { _qorsHasMoreResults = Nothing
    , _qorsIds = Nothing
    , _qorsMarker = Nothing
    , _qorsStatus = pStatus
    }

-- | Indicates whether there are more results that can be obtained by a
-- subsequent call.
qorsHasMoreResults :: Lens' QueryObjectsResponse (Maybe Bool)
qorsHasMoreResults = lens _qorsHasMoreResults (\ s a -> s{_qorsHasMoreResults = a});

-- | The identifiers that match the query selectors.
qorsIds :: Lens' QueryObjectsResponse [Text]
qorsIds = lens _qorsIds (\ s a -> s{_qorsIds = a}) . _Default;

-- | The starting point for the next page of results. To view the next page
-- of results, call @QueryObjects@ again with this marker value. If the
-- value is null, there are no more results.
qorsMarker :: Lens' QueryObjectsResponse (Maybe Text)
qorsMarker = lens _qorsMarker (\ s a -> s{_qorsMarker = a});

-- | FIXME: Undocumented member.
qorsStatus :: Lens' QueryObjectsResponse Int
qorsStatus = lens _qorsStatus (\ s a -> s{_qorsStatus = a});
