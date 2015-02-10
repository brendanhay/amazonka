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

-- Module      : Network.AWS.ECS.ListContainerInstances
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

-- | Returns a list of container instances in a specified cluster.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_ListContainerInstances.html>
module Network.AWS.ECS.ListContainerInstances
    (
    -- * Request
      ListContainerInstances
    -- ** Request constructor
    , listContainerInstances
    -- ** Request lenses
    , lciCluster
    , lciMaxResults
    , lciNextToken

    -- * Response
    , ListContainerInstancesResponse
    -- ** Response constructor
    , listContainerInstancesResponse
    -- ** Response lenses
    , lcirContainerInstanceArns
    , lcirNextToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ECS.Types
import qualified GHC.Exts

data ListContainerInstances = ListContainerInstances
    { _lciCluster    :: Maybe Text
    , _lciMaxResults :: Maybe Int
    , _lciNextToken  :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ListContainerInstances' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lciCluster' @::@ 'Maybe' 'Text'
--
-- * 'lciMaxResults' @::@ 'Maybe' 'Int'
--
-- * 'lciNextToken' @::@ 'Maybe' 'Text'
--
listContainerInstances :: ListContainerInstances
listContainerInstances = ListContainerInstances
    { _lciCluster    = Nothing
    , _lciNextToken  = Nothing
    , _lciMaxResults = Nothing
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts
-- the container instances you want to list. If you do not specify a cluster,
-- the default cluster is assumed..
lciCluster :: Lens' ListContainerInstances (Maybe Text)
lciCluster = lens _lciCluster (\s a -> s { _lciCluster = a })

-- | The maximum number of container instance results returned by 'ListContainerInstances' in paginated output. When this parameter is used, 'ListContainerInstances'
-- only returns 'maxResults' results in a single page along with a 'nextToken'
-- response element. The remaining results of the initial request can be seen by
-- sending another 'ListContainerInstances' request with the returned 'nextToken'
-- value. This value can be between 1 and 100. If this parameter is not used,
-- then 'ListContainerInstances' returns up to 100 results and a 'nextToken' value
-- if applicable.
lciMaxResults :: Lens' ListContainerInstances (Maybe Int)
lciMaxResults = lens _lciMaxResults (\s a -> s { _lciMaxResults = a })

-- | The 'nextToken' value returned from a previous paginated 'ListContainerInstances'
-- request where 'maxResults' was used and the results exceeded the value of that
-- parameter. Pagination continues from the end of the previous results that
-- returned the 'nextToken' value. This value is 'null' when there are no more
-- results to return.
lciNextToken :: Lens' ListContainerInstances (Maybe Text)
lciNextToken = lens _lciNextToken (\s a -> s { _lciNextToken = a })

data ListContainerInstancesResponse = ListContainerInstancesResponse
    { _lcirContainerInstanceArns :: List "member" Text
    , _lcirNextToken             :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ListContainerInstancesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lcirContainerInstanceArns' @::@ ['Text']
--
-- * 'lcirNextToken' @::@ 'Maybe' 'Text'
--
listContainerInstancesResponse :: ListContainerInstancesResponse
listContainerInstancesResponse = ListContainerInstancesResponse
    { _lcirContainerInstanceArns = mempty
    , _lcirNextToken             = Nothing
    }

-- | The list of container instance full Amazon Resource Name (ARN) entries for
-- each container instance associated with the specified cluster.
lcirContainerInstanceArns :: Lens' ListContainerInstancesResponse [Text]
lcirContainerInstanceArns =
    lens _lcirContainerInstanceArns
        (\s a -> s { _lcirContainerInstanceArns = a })
            . _List

-- | The 'nextToken' value to include in a future 'ListContainerInstances' request.
-- When the results of a 'ListContainerInstances' request exceed 'maxResults', this
-- value can be used to retrieve the next page of results. This value is 'null'
-- when there are no more results to return.
lcirNextToken :: Lens' ListContainerInstancesResponse (Maybe Text)
lcirNextToken = lens _lcirNextToken (\s a -> s { _lcirNextToken = a })

instance ToPath ListContainerInstances where
    toPath = const "/"

instance ToQuery ListContainerInstances where
    toQuery ListContainerInstances{..} = mconcat
        [ "cluster"    =? _lciCluster
        , "maxResults" =? _lciMaxResults
        , "nextToken"  =? _lciNextToken
        ]

instance ToHeaders ListContainerInstances

instance AWSRequest ListContainerInstances where
    type Sv ListContainerInstances = ECS
    type Rs ListContainerInstances = ListContainerInstancesResponse

    request  = post "ListContainerInstances"
    response = xmlResponse

instance FromXML ListContainerInstancesResponse where
    parseXML = withElement "ListContainerInstancesResult" $ \x -> ListContainerInstancesResponse
        <$> x .@? "containerInstanceArns" .!@ mempty
        <*> x .@? "nextToken"
