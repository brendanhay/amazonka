{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.DataPipeline.DescribeObjects
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

-- | Gets the object definitions for a set of objects associated with the
-- pipeline. Object definitions are composed of a set of fields that define
-- the properties of the object.
--
-- <http://docs.aws.amazon.com/datapipeline/latest/APIReference/API_DescribeObjects.html>
module Network.AWS.DataPipeline.DescribeObjects
    (
    -- * Request
      DescribeObjects
    -- ** Request constructor
    , describeObjects
    -- ** Request lenses
    , doEvaluateExpressions
    , doMarker
    , doPipelineId
    , doObjectIds

    -- * Response
    , DescribeObjectsResponse
    -- ** Response constructor
    , describeObjectsResponse
    -- ** Response lenses
    , dorHasMoreResults
    , dorMarker
    , dorPipelineObjects
    ) where

import Network.AWS.DataPipeline.Types
import Network.AWS.Pagers
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeObjects' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'doEvaluateExpressions'
--
-- * 'doMarker'
--
-- * 'doPipelineId'
--
-- * 'doObjectIds'
data DescribeObjects = DescribeObjects'{_doEvaluateExpressions :: Maybe Bool, _doMarker :: Maybe Text, _doPipelineId :: Text, _doObjectIds :: [Text]} deriving (Eq, Read, Show)

-- | 'DescribeObjects' smart constructor.
describeObjects :: Text -> DescribeObjects
describeObjects pPipelineId = DescribeObjects'{_doEvaluateExpressions = Nothing, _doMarker = Nothing, _doPipelineId = pPipelineId, _doObjectIds = mempty};

-- | Indicates whether any expressions in the object should be evaluated when
-- the object descriptions are returned.
doEvaluateExpressions :: Lens' DescribeObjects (Maybe Bool)
doEvaluateExpressions = lens _doEvaluateExpressions (\ s a -> s{_doEvaluateExpressions = a});

-- | The starting point for the results to be returned. For the first call,
-- this value should be empty. As long as there are more results, continue
-- to call @DescribeObjects@ with the marker value from the previous call
-- to retrieve the next set of results.
doMarker :: Lens' DescribeObjects (Maybe Text)
doMarker = lens _doMarker (\ s a -> s{_doMarker = a});

-- | The ID of the pipeline that contains the object definitions.
doPipelineId :: Lens' DescribeObjects Text
doPipelineId = lens _doPipelineId (\ s a -> s{_doPipelineId = a});

-- | The IDs of the pipeline objects that contain the definitions to be
-- described. You can pass as many as 25 identifiers in a single call to
-- @DescribeObjects@.
doObjectIds :: Lens' DescribeObjects [Text]
doObjectIds = lens _doObjectIds (\ s a -> s{_doObjectIds = a});

instance AWSPager DescribeObjects where
        page rq rs
          | stop (rs ^. dorHasMoreResults) = Nothing
          | isNothing (rs ^. dorMarker) = Nothing
          | otherwise = Just $ rq & doMarker .~ rs ^. dorMarker

instance AWSRequest DescribeObjects where
        type Sv DescribeObjects = DataPipeline
        type Rs DescribeObjects = DescribeObjectsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeObjectsResponse' <$>
                   (x .?> "hasMoreResults") <*> (x .?> "marker") <*>
                     (x .?> "pipelineObjects" .!@ mempty))

instance ToHeaders DescribeObjects where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DataPipeline.DescribeObjects" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeObjects where
        toJSON DescribeObjects'{..}
          = object
              ["evaluateExpressions" .= _doEvaluateExpressions,
               "marker" .= _doMarker, "pipelineId" .= _doPipelineId,
               "objectIds" .= _doObjectIds]

instance ToPath DescribeObjects where
        toPath = const "/"

instance ToQuery DescribeObjects where
        toQuery = const mempty

-- | /See:/ 'describeObjectsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dorHasMoreResults'
--
-- * 'dorMarker'
--
-- * 'dorPipelineObjects'
data DescribeObjectsResponse = DescribeObjectsResponse'{_dorHasMoreResults :: Maybe Bool, _dorMarker :: Maybe Text, _dorPipelineObjects :: [PipelineObject]} deriving (Eq, Read, Show)

-- | 'DescribeObjectsResponse' smart constructor.
describeObjectsResponse :: DescribeObjectsResponse
describeObjectsResponse = DescribeObjectsResponse'{_dorHasMoreResults = Nothing, _dorMarker = Nothing, _dorPipelineObjects = mempty};

-- | Indicates whether there are more results to return.
dorHasMoreResults :: Lens' DescribeObjectsResponse (Maybe Bool)
dorHasMoreResults = lens _dorHasMoreResults (\ s a -> s{_dorHasMoreResults = a});

-- | The starting point for the next page of results. To view the next page
-- of results, call @DescribeObjects@ again with this marker value. If the
-- value is null, there are no more results.
dorMarker :: Lens' DescribeObjectsResponse (Maybe Text)
dorMarker = lens _dorMarker (\ s a -> s{_dorMarker = a});

-- | An array of object definitions.
dorPipelineObjects :: Lens' DescribeObjectsResponse [PipelineObject]
dorPipelineObjects = lens _dorPipelineObjects (\ s a -> s{_dorPipelineObjects = a});
