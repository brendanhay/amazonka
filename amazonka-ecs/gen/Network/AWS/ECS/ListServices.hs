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

-- Module      : Network.AWS.ECS.ListServices
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

-- | Lists the services that are running in a specified cluster.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_ListServices.html>
module Network.AWS.ECS.ListServices
    (
    -- * Request
      ListServices
    -- ** Request constructor
    , listServices
    -- ** Request lenses
    , lsCluster
    , lsMaxResults
    , lsNextToken

    -- * Response
    , ListServicesResponse
    -- ** Response constructor
    , listServicesResponse
    -- ** Response lenses
    , lsrNextToken
    , lsrServiceArns
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.ECS.Types
import qualified GHC.Exts

data ListServices = ListServices
    { _lsCluster    :: Maybe Text
    , _lsMaxResults :: Maybe Int
    , _lsNextToken  :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ListServices' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsCluster' @::@ 'Maybe' 'Text'
--
-- * 'lsMaxResults' @::@ 'Maybe' 'Int'
--
-- * 'lsNextToken' @::@ 'Maybe' 'Text'
--
listServices :: ListServices
listServices = ListServices
    { _lsCluster    = Nothing
    , _lsNextToken  = Nothing
    , _lsMaxResults = Nothing
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts
-- the services you want to list. If you do not specify a cluster, the default
-- cluster is assumed..
lsCluster :: Lens' ListServices (Maybe Text)
lsCluster = lens _lsCluster (\s a -> s { _lsCluster = a })

-- | The maximum number of container instance results returned by 'ListServices' in
-- paginated output. When this parameter is used, 'ListServices' only returns 'maxResults' results in a single page along with a 'nextToken' response element. The
-- remaining results of the initial request can be seen by sending another 'ListServices' request with the returned 'nextToken' value. This value can be between 1 and
-- 100. If this parameter is not used, then 'ListServices' returns up to 100
-- results and a 'nextToken' value if applicable.
lsMaxResults :: Lens' ListServices (Maybe Int)
lsMaxResults = lens _lsMaxResults (\s a -> s { _lsMaxResults = a })

-- | The 'nextToken' value returned from a previous paginated 'ListServices' request
-- where 'maxResults' was used and the results exceeded the value of that
-- parameter. Pagination continues from the end of the previous results that
-- returned the 'nextToken' value. This value is 'null' when there are no more
-- results to return.
lsNextToken :: Lens' ListServices (Maybe Text)
lsNextToken = lens _lsNextToken (\s a -> s { _lsNextToken = a })

data ListServicesResponse = ListServicesResponse
    { _lsrNextToken   :: Maybe Text
    , _lsrServiceArns :: List "serviceArns" Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ListServicesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsrNextToken' @::@ 'Maybe' 'Text'
--
-- * 'lsrServiceArns' @::@ ['Text']
--
listServicesResponse :: ListServicesResponse
listServicesResponse = ListServicesResponse
    { _lsrServiceArns = mempty
    , _lsrNextToken   = Nothing
    }

-- | The 'nextToken' value to include in a future 'ListServices' request. When the
-- results of a 'ListServices' request exceed 'maxResults', this value can be used
-- to retrieve the next page of results. This value is 'null' when there are no
-- more results to return.
lsrNextToken :: Lens' ListServicesResponse (Maybe Text)
lsrNextToken = lens _lsrNextToken (\s a -> s { _lsrNextToken = a })

-- | The list of full Amazon Resource Name (ARN) entries for each service
-- associated with the specified cluster.
lsrServiceArns :: Lens' ListServicesResponse [Text]
lsrServiceArns = lens _lsrServiceArns (\s a -> s { _lsrServiceArns = a }) . _List

instance ToPath ListServices where
    toPath = const "/"

instance ToQuery ListServices where
    toQuery = const mempty

instance ToHeaders ListServices

instance ToJSON ListServices where
    toJSON ListServices{..} = object
        [ "cluster"    .= _lsCluster
        , "nextToken"  .= _lsNextToken
        , "maxResults" .= _lsMaxResults
        ]

instance AWSRequest ListServices where
    type Sv ListServices = ECS
    type Rs ListServices = ListServicesResponse

    request  = post "ListServices"
    response = jsonResponse

instance FromJSON ListServicesResponse where
    parseJSON = withObject "ListServicesResponse" $ \o -> ListServicesResponse
        <$> o .:? "nextToken"
        <*> o .:? "serviceArns" .!= mempty
