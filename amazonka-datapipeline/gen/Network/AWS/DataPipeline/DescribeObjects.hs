{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.DescribeObjects
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets the object definitions for a set of objects associated with the
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
    , dorsHasMoreResults
    , dorsMarker
    , dorsStatus
    , dorsPipelineObjects
    ) where

import           Network.AWS.DataPipeline.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for DescribeObjects.
--
-- /See:/ 'describeObjects' smart constructor.
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
data DescribeObjects = DescribeObjects'
    { _doEvaluateExpressions :: !(Maybe Bool)
    , _doMarker              :: !(Maybe Text)
    , _doPipelineId          :: !Text
    , _doObjectIds           :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeObjects' smart constructor.
describeObjects :: Text -> DescribeObjects
describeObjects pPipelineId_ =
    DescribeObjects'
    { _doEvaluateExpressions = Nothing
    , _doMarker = Nothing
    , _doPipelineId = pPipelineId_
    , _doObjectIds = mempty
    }

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
          | stop (rs ^. dorsHasMoreResults) = Nothing
          | isNothing (rs ^. dorsMarker) = Nothing
          | otherwise =
            Just $ rq & doMarker .~ rs ^. dorsMarker

instance AWSRequest DescribeObjects where
        type Sv DescribeObjects = DataPipeline
        type Rs DescribeObjects = DescribeObjectsResponse
        request = postJSON "DescribeObjects"
        response
          = receiveJSON
              (\ s h x ->
                 DescribeObjectsResponse' <$>
                   (x .?> "hasMoreResults") <*> (x .?> "marker") <*>
                     (pure (fromEnum s))
                     <*> (x .?> "pipelineObjects" .!@ mempty))

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

-- | Contains the output of DescribeObjects.
--
-- /See:/ 'describeObjectsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dorsHasMoreResults'
--
-- * 'dorsMarker'
--
-- * 'dorsStatus'
--
-- * 'dorsPipelineObjects'
data DescribeObjectsResponse = DescribeObjectsResponse'
    { _dorsHasMoreResults  :: !(Maybe Bool)
    , _dorsMarker          :: !(Maybe Text)
    , _dorsStatus          :: !Int
    , _dorsPipelineObjects :: ![PipelineObject]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeObjectsResponse' smart constructor.
describeObjectsResponse :: Int -> DescribeObjectsResponse
describeObjectsResponse pStatus_ =
    DescribeObjectsResponse'
    { _dorsHasMoreResults = Nothing
    , _dorsMarker = Nothing
    , _dorsStatus = pStatus_
    , _dorsPipelineObjects = mempty
    }

-- | Indicates whether there are more results to return.
dorsHasMoreResults :: Lens' DescribeObjectsResponse (Maybe Bool)
dorsHasMoreResults = lens _dorsHasMoreResults (\ s a -> s{_dorsHasMoreResults = a});

-- | The starting point for the next page of results. To view the next page
-- of results, call @DescribeObjects@ again with this marker value. If the
-- value is null, there are no more results.
dorsMarker :: Lens' DescribeObjectsResponse (Maybe Text)
dorsMarker = lens _dorsMarker (\ s a -> s{_dorsMarker = a});

-- | FIXME: Undocumented member.
dorsStatus :: Lens' DescribeObjectsResponse Int
dorsStatus = lens _dorsStatus (\ s a -> s{_dorsStatus = a});

-- | An array of object definitions.
dorsPipelineObjects :: Lens' DescribeObjectsResponse [PipelineObject]
dorsPipelineObjects = lens _dorsPipelineObjects (\ s a -> s{_dorsPipelineObjects = a});
