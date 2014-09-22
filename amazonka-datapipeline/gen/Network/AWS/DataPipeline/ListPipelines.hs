{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
-- POST / HTTP/1.1 Content-Type: application/x-amz-json-1.1 X-Amz-Target:
-- DataPipeline.ListPipelines Content-Length: 14 Host:
-- datapipeline.us-east-1.amazonaws.com X-Amz-Date: Mon, 12 Nov 2012 17:49:52
-- GMT Authorization: AuthParams {} Status: x-amzn-RequestId:
-- b3104dc5-0734-11e2-af6f-6bc7a6be60d9 Content-Type:
-- application/x-amz-json-1.1 Content-Length: 39 Date: Mon, 12 Nov 2012
-- 17:50:53 GMT {"PipelineIdList": [ {"id": "df-08785951KAKJEXAMPLE", "name":
-- "MyPipeline"}, {"id": "df-08662578ISYEXAMPLE", "name": "MySecondPipeline"}
-- ] }.
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
    , lprPipelineIdList
    , lprMarker
    , lprHasMoreResults
    ) where

import Network.AWS.DataPipeline.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | The input to the ListPipelines action.
newtype ListPipelines = ListPipelines
    { _lpMarker :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListPipelines' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Marker ::@ @Maybe Text@
--
listPipelines :: ListPipelines
listPipelines = ListPipelines
    { _lpMarker = Nothing
    }

-- | The starting point for the results to be returned. The first time you call
-- ListPipelines, this value should be empty. As long as the action returns
-- HasMoreResults as True, you can call ListPipelines again and pass the
-- marker value from the response to retrieve the next set of results.
lpMarker :: Lens' ListPipelines (Maybe Text)
lpMarker = lens _lpMarker (\s a -> s { _lpMarker = a })

instance ToPath ListPipelines

instance ToQuery ListPipelines

instance ToHeaders ListPipelines

instance ToJSON ListPipelines

-- | Contains the output from the ListPipelines action.
data ListPipelinesResponse = ListPipelinesResponse
    { _lprPipelineIdList :: [PipelineIdName]
    , _lprMarker :: Maybe Text
    , _lprHasMoreResults :: Bool
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListPipelinesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @PipelineIdList ::@ @[PipelineIdName]@
--
-- * @Marker ::@ @Maybe Text@
--
-- * @HasMoreResults ::@ @Bool@
--
listPipelinesResponse :: [PipelineIdName] -- ^ 'lprPipelineIdList'
                      -> Bool -- ^ 'lprHasMoreResults'
                      -> ListPipelinesResponse
listPipelinesResponse p1 p3 = ListPipelinesResponse
    { _lprPipelineIdList = p1
    , _lprMarker = Nothing
    , _lprHasMoreResults = p3
    }

-- | A list of all the pipeline identifiers that your account has permission to
-- access. If you require additional information about the pipelines, you can
-- use these identifiers to call DescribePipelines and GetPipelineDefinition.
lprPipelineIdList :: Lens' ListPipelinesResponse [PipelineIdName]
lprPipelineIdList =
    lens _lprPipelineIdList (\s a -> s { _lprPipelineIdList = a })

-- | If not null, indicates the starting point for the set of pipeline
-- identifiers that the next call to ListPipelines will retrieve. If null,
-- there are no more pipeline identifiers.
lprMarker :: Lens' ListPipelinesResponse (Maybe Text)
lprMarker = lens _lprMarker (\s a -> s { _lprMarker = a })

-- | If True, there are more results that can be obtained by a subsequent call
-- to ListPipelines.
lprHasMoreResults :: Lens' ListPipelinesResponse Bool
lprHasMoreResults =
    lens _lprHasMoreResults (\s a -> s { _lprHasMoreResults = a })

instance FromJSON ListPipelinesResponse

instance AWSRequest ListPipelines where
    type Sv ListPipelines = DataPipeline
    type Rs ListPipelines = ListPipelinesResponse

    request = get
    response _ = jsonResponse

instance AWSPager ListPipelines where
    next rq rs
        | not (rs ^. lprHasMoreResults) = Nothing
        | otherwise = Just $
            rq & lpMarker .~ rs ^. lprMarker
