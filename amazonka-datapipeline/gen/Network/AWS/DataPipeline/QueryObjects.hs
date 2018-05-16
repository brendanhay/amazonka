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
-- Module      : Network.AWS.DataPipeline.QueryObjects
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Queries the specified pipeline for the names of objects that match the specified set of conditions.
--
--
--
-- This operation returns paginated results.
module Network.AWS.DataPipeline.QueryObjects
    (
    -- * Creating a Request
      queryObjects
    , QueryObjects
    -- * Request Lenses
    , qoQuery
    , qoMarker
    , qoLimit
    , qoPipelineId
    , qoSphere

    -- * Destructuring the Response
    , queryObjectsResponse
    , QueryObjectsResponse
    -- * Response Lenses
    , qorsHasMoreResults
    , qorsIds
    , qorsMarker
    , qorsResponseStatus
    ) where

import Network.AWS.DataPipeline.Types
import Network.AWS.DataPipeline.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for QueryObjects.
--
--
--
-- /See:/ 'queryObjects' smart constructor.
data QueryObjects = QueryObjects'
  { _qoQuery      :: !(Maybe Query)
  , _qoMarker     :: !(Maybe Text)
  , _qoLimit      :: !(Maybe Int)
  , _qoPipelineId :: !Text
  , _qoSphere     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'QueryObjects' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qoQuery' - The query that defines the objects to be returned. The @Query@ object can contain a maximum of ten selectors. The conditions in the query are limited to top-level String fields in the object. These filters can be applied to components, instances, and attempts.
--
-- * 'qoMarker' - The starting point for the results to be returned. For the first call, this value should be empty. As long as there are more results, continue to call @QueryObjects@ with the marker value from the previous call to retrieve the next set of results.
--
-- * 'qoLimit' - The maximum number of object names that @QueryObjects@ will return in a single call. The default value is 100.
--
-- * 'qoPipelineId' - The ID of the pipeline.
--
-- * 'qoSphere' - Indicates whether the query applies to components or instances. The possible values are: @COMPONENT@ , @INSTANCE@ , and @ATTEMPT@ .
queryObjects
    :: Text -- ^ 'qoPipelineId'
    -> Text -- ^ 'qoSphere'
    -> QueryObjects
queryObjects pPipelineId_ pSphere_ =
  QueryObjects'
    { _qoQuery = Nothing
    , _qoMarker = Nothing
    , _qoLimit = Nothing
    , _qoPipelineId = pPipelineId_
    , _qoSphere = pSphere_
    }


-- | The query that defines the objects to be returned. The @Query@ object can contain a maximum of ten selectors. The conditions in the query are limited to top-level String fields in the object. These filters can be applied to components, instances, and attempts.
qoQuery :: Lens' QueryObjects (Maybe Query)
qoQuery = lens _qoQuery (\ s a -> s{_qoQuery = a})

-- | The starting point for the results to be returned. For the first call, this value should be empty. As long as there are more results, continue to call @QueryObjects@ with the marker value from the previous call to retrieve the next set of results.
qoMarker :: Lens' QueryObjects (Maybe Text)
qoMarker = lens _qoMarker (\ s a -> s{_qoMarker = a})

-- | The maximum number of object names that @QueryObjects@ will return in a single call. The default value is 100.
qoLimit :: Lens' QueryObjects (Maybe Int)
qoLimit = lens _qoLimit (\ s a -> s{_qoLimit = a})

-- | The ID of the pipeline.
qoPipelineId :: Lens' QueryObjects Text
qoPipelineId = lens _qoPipelineId (\ s a -> s{_qoPipelineId = a})

-- | Indicates whether the query applies to components or instances. The possible values are: @COMPONENT@ , @INSTANCE@ , and @ATTEMPT@ .
qoSphere :: Lens' QueryObjects Text
qoSphere = lens _qoSphere (\ s a -> s{_qoSphere = a})

instance AWSPager QueryObjects where
        page rq rs
          | stop (rs ^. qorsHasMoreResults) = Nothing
          | isNothing (rs ^. qorsMarker) = Nothing
          | otherwise =
            Just $ rq & qoMarker .~ rs ^. qorsMarker

instance AWSRequest QueryObjects where
        type Rs QueryObjects = QueryObjectsResponse
        request = postJSON dataPipeline
        response
          = receiveJSON
              (\ s h x ->
                 QueryObjectsResponse' <$>
                   (x .?> "hasMoreResults") <*> (x .?> "ids" .!@ mempty)
                     <*> (x .?> "marker")
                     <*> (pure (fromEnum s)))

instance Hashable QueryObjects where

instance NFData QueryObjects where

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
              (catMaybes
                 [("query" .=) <$> _qoQuery,
                  ("marker" .=) <$> _qoMarker,
                  ("limit" .=) <$> _qoLimit,
                  Just ("pipelineId" .= _qoPipelineId),
                  Just ("sphere" .= _qoSphere)])

instance ToPath QueryObjects where
        toPath = const "/"

instance ToQuery QueryObjects where
        toQuery = const mempty

-- | Contains the output of QueryObjects.
--
--
--
-- /See:/ 'queryObjectsResponse' smart constructor.
data QueryObjectsResponse = QueryObjectsResponse'
  { _qorsHasMoreResults :: !(Maybe Bool)
  , _qorsIds            :: !(Maybe [Text])
  , _qorsMarker         :: !(Maybe Text)
  , _qorsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'QueryObjectsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qorsHasMoreResults' - Indicates whether there are more results that can be obtained by a subsequent call.
--
-- * 'qorsIds' - The identifiers that match the query selectors.
--
-- * 'qorsMarker' - The starting point for the next page of results. To view the next page of results, call @QueryObjects@ again with this marker value. If the value is null, there are no more results.
--
-- * 'qorsResponseStatus' - -- | The response status code.
queryObjectsResponse
    :: Int -- ^ 'qorsResponseStatus'
    -> QueryObjectsResponse
queryObjectsResponse pResponseStatus_ =
  QueryObjectsResponse'
    { _qorsHasMoreResults = Nothing
    , _qorsIds = Nothing
    , _qorsMarker = Nothing
    , _qorsResponseStatus = pResponseStatus_
    }


-- | Indicates whether there are more results that can be obtained by a subsequent call.
qorsHasMoreResults :: Lens' QueryObjectsResponse (Maybe Bool)
qorsHasMoreResults = lens _qorsHasMoreResults (\ s a -> s{_qorsHasMoreResults = a})

-- | The identifiers that match the query selectors.
qorsIds :: Lens' QueryObjectsResponse [Text]
qorsIds = lens _qorsIds (\ s a -> s{_qorsIds = a}) . _Default . _Coerce

-- | The starting point for the next page of results. To view the next page of results, call @QueryObjects@ again with this marker value. If the value is null, there are no more results.
qorsMarker :: Lens' QueryObjectsResponse (Maybe Text)
qorsMarker = lens _qorsMarker (\ s a -> s{_qorsMarker = a})

-- | -- | The response status code.
qorsResponseStatus :: Lens' QueryObjectsResponse Int
qorsResponseStatus = lens _qorsResponseStatus (\ s a -> s{_qorsResponseStatus = a})

instance NFData QueryObjectsResponse where
