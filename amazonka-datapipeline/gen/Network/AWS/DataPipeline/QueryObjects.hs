{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DataPipeline.QueryObjects
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
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
    , qoLimit
    , qoMarker
    , qoPipelineId
    , qoQuery
    , qoSphere

    -- * Response
    , QueryObjectsResponse
    -- ** Response constructor
    , queryObjectsResponse
    -- ** Response lenses
    , qorHasMoreResults
    , qorIds
    , qorMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.DataPipeline.Types
import qualified GHC.Exts

data QueryObjects = QueryObjects
    { _qoLimit      :: Maybe Int
    , _qoMarker     :: Maybe Text
    , _qoPipelineId :: Text
    , _qoQuery      :: Maybe Query
    , _qoSphere     :: Text
    } deriving (Eq, Read, Show)

-- | 'QueryObjects' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'qoLimit' @::@ 'Maybe' 'Int'
--
-- * 'qoMarker' @::@ 'Maybe' 'Text'
--
-- * 'qoPipelineId' @::@ 'Text'
--
-- * 'qoQuery' @::@ 'Maybe' 'Query'
--
-- * 'qoSphere' @::@ 'Text'
--
queryObjects :: Text -- ^ 'qoPipelineId'
             -> Text -- ^ 'qoSphere'
             -> QueryObjects
queryObjects p1 p2 = QueryObjects
    { _qoPipelineId = p1
    , _qoSphere     = p2
    , _qoQuery      = Nothing
    , _qoMarker     = Nothing
    , _qoLimit      = Nothing
    }

-- | The maximum number of object names that 'QueryObjects' will return in a single
-- call. The default value is 100.
qoLimit :: Lens' QueryObjects (Maybe Int)
qoLimit = lens _qoLimit (\s a -> s { _qoLimit = a })

-- | The starting point for the results to be returned. For the first call, this
-- value should be empty. As long as there are more results, continue to call 'QueryObjects' with the marker value from the previous call to retrieve the next set of
-- results.
qoMarker :: Lens' QueryObjects (Maybe Text)
qoMarker = lens _qoMarker (\s a -> s { _qoMarker = a })

-- | The ID of the pipeline.
qoPipelineId :: Lens' QueryObjects Text
qoPipelineId = lens _qoPipelineId (\s a -> s { _qoPipelineId = a })

-- | The query that defines the objects to be returned. The 'Query' object can
-- contain a maximum of ten selectors. The conditions in the query are limited
-- to top-level String fields in the object. These filters can be applied to
-- components, instances, and attempts.
qoQuery :: Lens' QueryObjects (Maybe Query)
qoQuery = lens _qoQuery (\s a -> s { _qoQuery = a })

-- | Indicates whether the query applies to components or instances. The possible
-- values are: 'COMPONENT', 'INSTANCE', and 'ATTEMPT'.
qoSphere :: Lens' QueryObjects Text
qoSphere = lens _qoSphere (\s a -> s { _qoSphere = a })

data QueryObjectsResponse = QueryObjectsResponse
    { _qorHasMoreResults :: Maybe Bool
    , _qorIds            :: List "ids" Text
    , _qorMarker         :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'QueryObjectsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'qorHasMoreResults' @::@ 'Maybe' 'Bool'
--
-- * 'qorIds' @::@ ['Text']
--
-- * 'qorMarker' @::@ 'Maybe' 'Text'
--
queryObjectsResponse :: QueryObjectsResponse
queryObjectsResponse = QueryObjectsResponse
    { _qorIds            = mempty
    , _qorMarker         = Nothing
    , _qorHasMoreResults = Nothing
    }

-- | Indicates whether there are more results that can be obtained by a subsequent
-- call.
qorHasMoreResults :: Lens' QueryObjectsResponse (Maybe Bool)
qorHasMoreResults =
    lens _qorHasMoreResults (\s a -> s { _qorHasMoreResults = a })

-- | The identifiers that match the query selectors.
qorIds :: Lens' QueryObjectsResponse [Text]
qorIds = lens _qorIds (\s a -> s { _qorIds = a }) . _List

-- | The starting point for the next page of results. To view the next page of
-- results, call 'QueryObjects' again with this marker value. If the value is
-- null, there are no more results.
qorMarker :: Lens' QueryObjectsResponse (Maybe Text)
qorMarker = lens _qorMarker (\s a -> s { _qorMarker = a })

instance ToPath QueryObjects where
    toPath = const "/"

instance ToQuery QueryObjects where
    toQuery = const mempty

instance ToHeaders QueryObjects

instance ToJSON QueryObjects where
    toJSON QueryObjects{..} = object
        [ "pipelineId" .= _qoPipelineId
        , "query"      .= _qoQuery
        , "sphere"     .= _qoSphere
        , "marker"     .= _qoMarker
        , "limit"      .= _qoLimit
        ]

instance AWSRequest QueryObjects where
    type Sv QueryObjects = DataPipeline
    type Rs QueryObjects = QueryObjectsResponse

    request  = post "QueryObjects"
    response = jsonResponse

instance FromJSON QueryObjectsResponse where
    parseJSON = withObject "QueryObjectsResponse" $ \o -> QueryObjectsResponse
        <$> o .:? "hasMoreResults"
        <*> o .:? "ids" .!= mempty
        <*> o .:? "marker"

instance AWSPager QueryObjects where
    page rq rs
        | stop (rs ^. qorHasMoreResults) = Nothing
        | otherwise = Just $ rq
            & qoMarker .~ rs ^. qorMarker
