{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.DataPipeline.ListPipelines
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of pipeline identifiers for all active pipelines.
-- Identifiers are returned only for pipelines you have permission to access.
module Network.AWS.DataPipeline.ListPipelines
    (
    -- * Request
      ListPipelines
    -- ** Request constructor
    , listPipelines
    -- ** Request lenses
    , lpMarker

    -- * Response
    , ListPipelinesResponse
    -- ** Response constructor
    , listPipelinesResponse
    -- ** Response lenses
    , lprHasMoreResults
    , lprMarker
    , lprPipelineIdList
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.DataPipeline.Types

newtype ListPipelines = ListPipelines
    { _lpMarker :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'ListPipelines' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lpMarker' @::@ 'Maybe' 'Text'
--
listPipelines :: ListPipelines
listPipelines = ListPipelines
    { _lpMarker = Nothing
    }

-- | The starting point for the results to be returned. The first time you
-- call ListPipelines, this value should be empty. As long as the action
-- returns HasMoreResults as True, you can call ListPipelines again and pass
-- the marker value from the response to retrieve the next set of results.
lpMarker :: Lens' ListPipelines (Maybe Text)
lpMarker = lens _lpMarker (\s a -> s { _lpMarker = a })

instance ToPath ListPipelines where
    toPath = const "/"

instance ToQuery ListPipelines where
    toQuery = const mempty

instance ToHeaders ListPipelines

instance ToBody ListPipelines where
    toBody = toBody . encode . _lpMarker

data ListPipelinesResponse = ListPipelinesResponse
    { _lprHasMoreResults :: Maybe Bool
    , _lprMarker         :: Maybe Text
    , _lprPipelineIdList :: [PipelineIdName]
    } deriving (Eq, Show, Generic)

-- | 'ListPipelinesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lprHasMoreResults' @::@ 'Maybe' 'Bool'
--
-- * 'lprMarker' @::@ 'Maybe' 'Text'
--
-- * 'lprPipelineIdList' @::@ ['PipelineIdName']
--
listPipelinesResponse :: ListPipelinesResponse
listPipelinesResponse = ListPipelinesResponse
    { _lprPipelineIdList = mempty
    , _lprMarker         = Nothing
    , _lprHasMoreResults = Nothing
    }

-- | If True, there are more results that can be obtained by a subsequent call
-- to ListPipelines.
lprHasMoreResults :: Lens' ListPipelinesResponse (Maybe Bool)
lprHasMoreResults =
    lens _lprHasMoreResults (\s a -> s { _lprHasMoreResults = a })

-- | If not null, indicates the starting point for the set of pipeline
-- identifiers that the next call to ListPipelines will retrieve. If null,
-- there are no more pipeline identifiers.
lprMarker :: Lens' ListPipelinesResponse (Maybe Text)
lprMarker = lens _lprMarker (\s a -> s { _lprMarker = a })

-- | A list of all the pipeline identifiers that your account has permission
-- to access. If you require additional information about the pipelines, you
-- can use these identifiers to call DescribePipelines and
-- GetPipelineDefinition.
lprPipelineIdList :: Lens' ListPipelinesResponse [PipelineIdName]
lprPipelineIdList =
    lens _lprPipelineIdList (\s a -> s { _lprPipelineIdList = a })

instance AWSRequest ListPipelines where
    type Sv ListPipelines = DataPipeline
    type Rs ListPipelines = ListPipelinesResponse

    request  = post
    response = jsonResponse $ \h o -> ListPipelinesResponse
        <$> o .: "hasMoreResults"
        <*> o .: "marker"
        <*> o .: "pipelineIdList"
