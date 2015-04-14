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

-- Module      : Network.AWS.ECS.ListClusters
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

-- | Returns a list of existing clusters.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_ListClusters.html>
module Network.AWS.ECS.ListClusters
    (
    -- * Request
      ListClusters
    -- ** Request constructor
    , listClusters
    -- ** Request lenses
    , lcMaxResults
    , lcNextToken

    -- * Response
    , ListClustersResponse
    -- ** Response constructor
    , listClustersResponse
    -- ** Response lenses
    , lcrClusterArns
    , lcrNextToken
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.ECS.Types
import qualified GHC.Exts

data ListClusters = ListClusters
    { _lcMaxResults :: Maybe Int
    , _lcNextToken  :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ListClusters' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lcMaxResults' @::@ 'Maybe' 'Int'
--
-- * 'lcNextToken' @::@ 'Maybe' 'Text'
--
listClusters :: ListClusters
listClusters = ListClusters
    { _lcNextToken  = Nothing
    , _lcMaxResults = Nothing
    }

-- | The maximum number of cluster results returned by 'ListClusters' in paginated
-- output. When this parameter is used, 'ListClusters' only returns 'maxResults'
-- results in a single page along with a 'nextToken' response element. The
-- remaining results of the initial request can be seen by sending another 'ListClusters' request with the returned 'nextToken' value. This value can be between 1 and
-- 100. If this parameter is not used, then 'ListClusters' returns up to 100
-- results and a 'nextToken' value if applicable.
lcMaxResults :: Lens' ListClusters (Maybe Int)
lcMaxResults = lens _lcMaxResults (\s a -> s { _lcMaxResults = a })

-- | The 'nextToken' value returned from a previous paginated 'ListClusters' request
-- where 'maxResults' was used and the results exceeded the value of that
-- parameter. Pagination continues from the end of the previous results that
-- returned the 'nextToken' value. This value is 'null' when there are no more
-- results to return.
lcNextToken :: Lens' ListClusters (Maybe Text)
lcNextToken = lens _lcNextToken (\s a -> s { _lcNextToken = a })

data ListClustersResponse = ListClustersResponse
    { _lcrClusterArns :: List "clusterArns" Text
    , _lcrNextToken   :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ListClustersResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lcrClusterArns' @::@ ['Text']
--
-- * 'lcrNextToken' @::@ 'Maybe' 'Text'
--
listClustersResponse :: ListClustersResponse
listClustersResponse = ListClustersResponse
    { _lcrClusterArns = mempty
    , _lcrNextToken   = Nothing
    }

-- | The list of full Amazon Resource Name (ARN) entries for each cluster
-- associated with your account.
lcrClusterArns :: Lens' ListClustersResponse [Text]
lcrClusterArns = lens _lcrClusterArns (\s a -> s { _lcrClusterArns = a }) . _List

-- | The 'nextToken' value to include in a future 'ListClusters' request. When the
-- results of a 'ListClusters' request exceed 'maxResults', this value can be used
-- to retrieve the next page of results. This value is 'null' when there are no
-- more results to return.
lcrNextToken :: Lens' ListClustersResponse (Maybe Text)
lcrNextToken = lens _lcrNextToken (\s a -> s { _lcrNextToken = a })

instance ToPath ListClusters where
    toPath = const "/"

instance ToQuery ListClusters where
    toQuery = const mempty

instance ToHeaders ListClusters

instance ToJSON ListClusters where
    toJSON ListClusters{..} = object
        [ "nextToken"  .= _lcNextToken
        , "maxResults" .= _lcMaxResults
        ]

instance AWSRequest ListClusters where
    type Sv ListClusters = ECS
    type Rs ListClusters = ListClustersResponse

    request  = post "ListClusters"
    response = jsonResponse

instance FromJSON ListClustersResponse where
    parseJSON = withObject "ListClustersResponse" $ \o -> ListClustersResponse
        <$> o .:? "clusterArns" .!= mempty
        <*> o .:? "nextToken"
