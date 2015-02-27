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

-- Module      : Network.AWS.ECS.ListTasks
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

-- | Returns a list of tasks for a specified cluster. You can filter the results
-- by family name or by a particular container instance with the 'family' and 'containerInstance' parameters.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_ListTasks.html>
module Network.AWS.ECS.ListTasks
    (
    -- * Request
      ListTasks
    -- ** Request constructor
    , listTasks
    -- ** Request lenses
    , ltCluster
    , ltContainerInstance
    , ltFamily
    , ltMaxResults
    , ltNextToken

    -- * Response
    , ListTasksResponse
    -- ** Response constructor
    , listTasksResponse
    -- ** Response lenses
    , ltrNextToken
    , ltrTaskArns
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.ECS.Types
import qualified GHC.Exts

data ListTasks = ListTasks
    { _ltCluster           :: Maybe Text
    , _ltContainerInstance :: Maybe Text
    , _ltFamily            :: Maybe Text
    , _ltMaxResults        :: Maybe Int
    , _ltNextToken         :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ListTasks' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltCluster' @::@ 'Maybe' 'Text'
--
-- * 'ltContainerInstance' @::@ 'Maybe' 'Text'
--
-- * 'ltFamily' @::@ 'Maybe' 'Text'
--
-- * 'ltMaxResults' @::@ 'Maybe' 'Int'
--
-- * 'ltNextToken' @::@ 'Maybe' 'Text'
--
listTasks :: ListTasks
listTasks = ListTasks
    { _ltCluster           = Nothing
    , _ltContainerInstance = Nothing
    , _ltFamily            = Nothing
    , _ltNextToken         = Nothing
    , _ltMaxResults        = Nothing
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts
-- the tasks you want to list. If you do not specify a cluster, the default
-- cluster is assumed..
ltCluster :: Lens' ListTasks (Maybe Text)
ltCluster = lens _ltCluster (\s a -> s { _ltCluster = a })

-- | The container instance UUID or full Amazon Resource Name (ARN) of the
-- container instance that you want to filter the 'ListTasks' results with.
-- Specifying a 'containerInstance' will limit the results to tasks that belong to
-- that container instance.
ltContainerInstance :: Lens' ListTasks (Maybe Text)
ltContainerInstance =
    lens _ltContainerInstance (\s a -> s { _ltContainerInstance = a })

-- | The name of the family that you want to filter the 'ListTasks' results with.
-- Specifying a 'family' will limit the results to tasks that belong to that
-- family.
ltFamily :: Lens' ListTasks (Maybe Text)
ltFamily = lens _ltFamily (\s a -> s { _ltFamily = a })

-- | The maximum number of task results returned by 'ListTasks' in paginated output.
-- When this parameter is used, 'ListTasks' only returns 'maxResults' results in a
-- single page along with a 'nextToken' response element. The remaining results of
-- the initial request can be seen by sending another 'ListTasks' request with the
-- returned 'nextToken' value. This value can be between 1 and 100. If this
-- parameter is not used, then 'ListTasks' returns up to 100 results and a 'nextToken' value if applicable.
ltMaxResults :: Lens' ListTasks (Maybe Int)
ltMaxResults = lens _ltMaxResults (\s a -> s { _ltMaxResults = a })

-- | The 'nextToken' value returned from a previous paginated 'ListTasks' request
-- where 'maxResults' was used and the results exceeded the value of that
-- parameter. Pagination continues from the end of the previous results that
-- returned the 'nextToken' value. This value is 'null' when there are no more
-- results to return.
ltNextToken :: Lens' ListTasks (Maybe Text)
ltNextToken = lens _ltNextToken (\s a -> s { _ltNextToken = a })

data ListTasksResponse = ListTasksResponse
    { _ltrNextToken :: Maybe Text
    , _ltrTaskArns  :: List "taskArns" Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ListTasksResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltrNextToken' @::@ 'Maybe' 'Text'
--
-- * 'ltrTaskArns' @::@ ['Text']
--
listTasksResponse :: ListTasksResponse
listTasksResponse = ListTasksResponse
    { _ltrTaskArns  = mempty
    , _ltrNextToken = Nothing
    }

-- | The 'nextToken' value to include in a future 'ListTasks' request. When the
-- results of a 'ListTasks' request exceed 'maxResults', this value can be used to
-- retrieve the next page of results. This value is 'null' when there are no more
-- results to return.
ltrNextToken :: Lens' ListTasksResponse (Maybe Text)
ltrNextToken = lens _ltrNextToken (\s a -> s { _ltrNextToken = a })

-- | The list of task Amazon Resource Name (ARN) entries for the 'ListTasks' request.
ltrTaskArns :: Lens' ListTasksResponse [Text]
ltrTaskArns = lens _ltrTaskArns (\s a -> s { _ltrTaskArns = a }) . _List

instance ToPath ListTasks where
    toPath = const "/"

instance ToQuery ListTasks where
    toQuery = const mempty

instance ToHeaders ListTasks

instance ToJSON ListTasks where
    toJSON ListTasks{..} = object
        [ "cluster"           .= _ltCluster
        , "containerInstance" .= _ltContainerInstance
        , "family"            .= _ltFamily
        , "nextToken"         .= _ltNextToken
        , "maxResults"        .= _ltMaxResults
        ]

instance AWSRequest ListTasks where
    type Sv ListTasks = ECS
    type Rs ListTasks = ListTasksResponse

    request  = post "ListTasks"
    response = jsonResponse

instance FromJSON ListTasksResponse where
    parseJSON = withObject "ListTasksResponse" $ \o -> ListTasksResponse
        <$> o .:? "nextToken"
        <*> o .:? "taskArns" .!= mempty
