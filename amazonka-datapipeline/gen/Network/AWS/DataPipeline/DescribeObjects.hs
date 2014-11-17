{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DataPipeline.DescribeObjects
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the object definitions for a set of objects associated with the
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
    , doObjectIds
    , doPipelineId

    -- * Response
    , DescribeObjectsResponse
    -- ** Response constructor
    , describeObjectsResponse
    -- ** Response lenses
    , dorHasMoreResults
    , dorMarker
    , dorPipelineObjects
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.DataPipeline.Types
import qualified GHC.Exts

data DescribeObjects = DescribeObjects
    { _doEvaluateExpressions :: Maybe Bool
    , _doMarker              :: Maybe Text
    , _doObjectIds           :: [Text]
    , _doPipelineId          :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeObjects' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'doEvaluateExpressions' @::@ 'Maybe' 'Bool'
--
-- * 'doMarker' @::@ 'Maybe' 'Text'
--
-- * 'doObjectIds' @::@ ['Text']
--
-- * 'doPipelineId' @::@ 'Text'
--
describeObjects :: Text -- ^ 'doPipelineId'
                -> DescribeObjects
describeObjects p1 = DescribeObjects
    { _doPipelineId          = p1
    , _doObjectIds           = mempty
    , _doEvaluateExpressions = Nothing
    , _doMarker              = Nothing
    }

-- | Indicates whether any expressions in the object should be evaluated when
-- the object descriptions are returned.
doEvaluateExpressions :: Lens' DescribeObjects (Maybe Bool)
doEvaluateExpressions =
    lens _doEvaluateExpressions (\s a -> s { _doEvaluateExpressions = a })

-- | The starting point for the results to be returned. The first time you
-- call DescribeObjects, this value should be empty. As long as the action
-- returns HasMoreResults as True, you can call DescribeObjects again and
-- pass the marker value from the response to retrieve the next set of
-- results.
doMarker :: Lens' DescribeObjects (Maybe Text)
doMarker = lens _doMarker (\s a -> s { _doMarker = a })

-- | Identifiers of the pipeline objects that contain the definitions to be
-- described. You can pass as many as 25 identifiers in a single call to
-- DescribeObjects.
doObjectIds :: Lens' DescribeObjects [Text]
doObjectIds = lens _doObjectIds (\s a -> s { _doObjectIds = a })

-- | Identifier of the pipeline that contains the object definitions.
doPipelineId :: Lens' DescribeObjects Text
doPipelineId = lens _doPipelineId (\s a -> s { _doPipelineId = a })

data DescribeObjectsResponse = DescribeObjectsResponse
    { _dorHasMoreResults  :: Maybe Bool
    , _dorMarker          :: Maybe Text
    , _dorPipelineObjects :: [PipelineObject]
    } deriving (Eq, Show, Generic)

-- | 'DescribeObjectsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dorHasMoreResults' @::@ 'Maybe' 'Bool'
--
-- * 'dorMarker' @::@ 'Maybe' 'Text'
--
-- * 'dorPipelineObjects' @::@ ['PipelineObject']
--
describeObjectsResponse :: DescribeObjectsResponse
describeObjectsResponse = DescribeObjectsResponse
    { _dorPipelineObjects = mempty
    , _dorMarker          = Nothing
    , _dorHasMoreResults  = Nothing
    }

-- | If True, there are more pages of results to return.
dorHasMoreResults :: Lens' DescribeObjectsResponse (Maybe Bool)
dorHasMoreResults =
    lens _dorHasMoreResults (\s a -> s { _dorHasMoreResults = a })

-- | The starting point for the next page of results. To view the next page of
-- results, call DescribeObjects again with this marker value.
dorMarker :: Lens' DescribeObjectsResponse (Maybe Text)
dorMarker = lens _dorMarker (\s a -> s { _dorMarker = a })

-- | An array of object definitions that are returned by the call to
-- DescribeObjects.
dorPipelineObjects :: Lens' DescribeObjectsResponse [PipelineObject]
dorPipelineObjects =
    lens _dorPipelineObjects (\s a -> s { _dorPipelineObjects = a })

instance ToPath DescribeObjects where
    toPath = const "/"

instance ToQuery DescribeObjects where
    toQuery = const mempty

instance ToHeaders DescribeObjects
instance ToJSON DescribeObjects where
    toJSON = genericToJSON jsonOptions

instance AWSRequest DescribeObjects where
    type Sv DescribeObjects = DataPipeline
    type Rs DescribeObjects = DescribeObjectsResponse

    request  = post
    response = jsonResponse

instance FromJSON DescribeObjectsResponse where
    parseJSON = genericParseJSON jsonOptions
