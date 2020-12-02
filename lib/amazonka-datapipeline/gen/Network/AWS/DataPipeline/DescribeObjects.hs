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
-- Module      : Network.AWS.DataPipeline.DescribeObjects
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the object definitions for a set of objects associated with the pipeline. Object definitions are composed of a set of fields that define the properties of the object.
--
--
--
-- This operation returns paginated results.
module Network.AWS.DataPipeline.DescribeObjects
    (
    -- * Creating a Request
      describeObjects
    , DescribeObjects
    -- * Request Lenses
    , doEvaluateExpressions
    , doMarker
    , doPipelineId
    , doObjectIds

    -- * Destructuring the Response
    , describeObjectsResponse
    , DescribeObjectsResponse
    -- * Response Lenses
    , dorsHasMoreResults
    , dorsMarker
    , dorsResponseStatus
    , dorsPipelineObjects
    ) where

import Network.AWS.DataPipeline.Types
import Network.AWS.DataPipeline.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DescribeObjects.
--
--
--
-- /See:/ 'describeObjects' smart constructor.
data DescribeObjects = DescribeObjects'
  { _doEvaluateExpressions :: !(Maybe Bool)
  , _doMarker              :: !(Maybe Text)
  , _doPipelineId          :: !Text
  , _doObjectIds           :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeObjects' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'doEvaluateExpressions' - Indicates whether any expressions in the object should be evaluated when the object descriptions are returned.
--
-- * 'doMarker' - The starting point for the results to be returned. For the first call, this value should be empty. As long as there are more results, continue to call @DescribeObjects@ with the marker value from the previous call to retrieve the next set of results.
--
-- * 'doPipelineId' - The ID of the pipeline that contains the object definitions.
--
-- * 'doObjectIds' - The IDs of the pipeline objects that contain the definitions to be described. You can pass as many as 25 identifiers in a single call to @DescribeObjects@ .
describeObjects
    :: Text -- ^ 'doPipelineId'
    -> DescribeObjects
describeObjects pPipelineId_ =
  DescribeObjects'
    { _doEvaluateExpressions = Nothing
    , _doMarker = Nothing
    , _doPipelineId = pPipelineId_
    , _doObjectIds = mempty
    }


-- | Indicates whether any expressions in the object should be evaluated when the object descriptions are returned.
doEvaluateExpressions :: Lens' DescribeObjects (Maybe Bool)
doEvaluateExpressions = lens _doEvaluateExpressions (\ s a -> s{_doEvaluateExpressions = a})

-- | The starting point for the results to be returned. For the first call, this value should be empty. As long as there are more results, continue to call @DescribeObjects@ with the marker value from the previous call to retrieve the next set of results.
doMarker :: Lens' DescribeObjects (Maybe Text)
doMarker = lens _doMarker (\ s a -> s{_doMarker = a})

-- | The ID of the pipeline that contains the object definitions.
doPipelineId :: Lens' DescribeObjects Text
doPipelineId = lens _doPipelineId (\ s a -> s{_doPipelineId = a})

-- | The IDs of the pipeline objects that contain the definitions to be described. You can pass as many as 25 identifiers in a single call to @DescribeObjects@ .
doObjectIds :: Lens' DescribeObjects [Text]
doObjectIds = lens _doObjectIds (\ s a -> s{_doObjectIds = a}) . _Coerce

instance AWSPager DescribeObjects where
        page rq rs
          | stop (rs ^. dorsHasMoreResults) = Nothing
          | isNothing (rs ^. dorsMarker) = Nothing
          | otherwise =
            Just $ rq & doMarker .~ rs ^. dorsMarker

instance AWSRequest DescribeObjects where
        type Rs DescribeObjects = DescribeObjectsResponse
        request = postJSON dataPipeline
        response
          = receiveJSON
              (\ s h x ->
                 DescribeObjectsResponse' <$>
                   (x .?> "hasMoreResults") <*> (x .?> "marker") <*>
                     (pure (fromEnum s))
                     <*> (x .?> "pipelineObjects" .!@ mempty))

instance Hashable DescribeObjects where

instance NFData DescribeObjects where

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
              (catMaybes
                 [("evaluateExpressions" .=) <$>
                    _doEvaluateExpressions,
                  ("marker" .=) <$> _doMarker,
                  Just ("pipelineId" .= _doPipelineId),
                  Just ("objectIds" .= _doObjectIds)])

instance ToPath DescribeObjects where
        toPath = const "/"

instance ToQuery DescribeObjects where
        toQuery = const mempty

-- | Contains the output of DescribeObjects.
--
--
--
-- /See:/ 'describeObjectsResponse' smart constructor.
data DescribeObjectsResponse = DescribeObjectsResponse'
  { _dorsHasMoreResults  :: !(Maybe Bool)
  , _dorsMarker          :: !(Maybe Text)
  , _dorsResponseStatus  :: !Int
  , _dorsPipelineObjects :: ![PipelineObject]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeObjectsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dorsHasMoreResults' - Indicates whether there are more results to return.
--
-- * 'dorsMarker' - The starting point for the next page of results. To view the next page of results, call @DescribeObjects@ again with this marker value. If the value is null, there are no more results.
--
-- * 'dorsResponseStatus' - -- | The response status code.
--
-- * 'dorsPipelineObjects' - An array of object definitions.
describeObjectsResponse
    :: Int -- ^ 'dorsResponseStatus'
    -> DescribeObjectsResponse
describeObjectsResponse pResponseStatus_ =
  DescribeObjectsResponse'
    { _dorsHasMoreResults = Nothing
    , _dorsMarker = Nothing
    , _dorsResponseStatus = pResponseStatus_
    , _dorsPipelineObjects = mempty
    }


-- | Indicates whether there are more results to return.
dorsHasMoreResults :: Lens' DescribeObjectsResponse (Maybe Bool)
dorsHasMoreResults = lens _dorsHasMoreResults (\ s a -> s{_dorsHasMoreResults = a})

-- | The starting point for the next page of results. To view the next page of results, call @DescribeObjects@ again with this marker value. If the value is null, there are no more results.
dorsMarker :: Lens' DescribeObjectsResponse (Maybe Text)
dorsMarker = lens _dorsMarker (\ s a -> s{_dorsMarker = a})

-- | -- | The response status code.
dorsResponseStatus :: Lens' DescribeObjectsResponse Int
dorsResponseStatus = lens _dorsResponseStatus (\ s a -> s{_dorsResponseStatus = a})

-- | An array of object definitions.
dorsPipelineObjects :: Lens' DescribeObjectsResponse [PipelineObject]
dorsPipelineObjects = lens _dorsPipelineObjects (\ s a -> s{_dorsPipelineObjects = a}) . _Coerce

instance NFData DescribeObjectsResponse where
