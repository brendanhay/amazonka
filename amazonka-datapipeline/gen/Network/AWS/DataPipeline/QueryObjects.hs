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

-- | Queries a pipeline for the names of objects that match a specified set of
-- conditions.
--
-- The objects returned by 'QueryObjects' are paginated and then filtered by the
-- value you set for query. This means the action may return an empty result set
-- with a value set for marker. If 'HasMoreResults' is set to 'True', you should
-- continue to call 'QueryObjects', passing in the returned value for marker,
-- until 'HasMoreResults' returns 'False'.
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

-- | Specifies the maximum number of object names that 'QueryObjects' will return in
-- a single call. The default value is 100.
qoLimit :: Lens' QueryObjects (Maybe Int)
qoLimit = lens _qoLimit (\s a -> s { _qoLimit = a })

-- | The starting point for the results to be returned. The first time you call 'QueryObjects', this value should be empty. As long as the action returns 'HasMoreResults' as 'True', you can call 'QueryObjects' again and pass the marker value from the
-- response to retrieve the next set of results.
qoMarker :: Lens' QueryObjects (Maybe Text)
qoMarker = lens _qoMarker (\s a -> s { _qoMarker = a })

-- | Identifier of the pipeline to be queried for object names.
qoPipelineId :: Lens' QueryObjects Text
qoPipelineId = lens _qoPipelineId (\s a -> s { _qoPipelineId = a })

-- | Query that defines the objects to be returned. The 'Query' object can contain
-- a maximum of ten selectors. The conditions in the query are limited to
-- top-level String fields in the object. These filters can be applied to
-- components, instances, and attempts.
qoQuery :: Lens' QueryObjects (Maybe Query)
qoQuery = lens _qoQuery (\s a -> s { _qoQuery = a })

-- | Specifies whether the query applies to components or instances. Allowable
-- values: 'COMPONENT', 'INSTANCE', 'ATTEMPT'.
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

-- | If 'True', there are more results that can be obtained by a subsequent call to 'QueryObjects'.
qorHasMoreResults :: Lens' QueryObjectsResponse (Maybe Bool)
qorHasMoreResults =
    lens _qorHasMoreResults (\s a -> s { _qorHasMoreResults = a })

-- | A list of identifiers that match the query selectors.
qorIds :: Lens' QueryObjectsResponse [Text]
qorIds = lens _qorIds (\s a -> s { _qorIds = a }) . _List

-- | The starting point for the results to be returned. As long as the action
-- returns 'HasMoreResults' as 'True', you can call 'QueryObjects' again and pass the
-- marker value from the response to retrieve the next set of results.
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
