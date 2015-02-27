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

-- Module      : Network.AWS.ECS.DescribeTasks
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

-- | Describes a specified task or tasks.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DescribeTasks.html>
module Network.AWS.ECS.DescribeTasks
    (
    -- * Request
      DescribeTasks
    -- ** Request constructor
    , describeTasks
    -- ** Request lenses
    , dtCluster
    , dtTasks

    -- * Response
    , DescribeTasksResponse
    -- ** Response constructor
    , describeTasksResponse
    -- ** Response lenses
    , dtrFailures
    , dtrTasks
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.ECS.Types
import qualified GHC.Exts

data DescribeTasks = DescribeTasks
    { _dtCluster :: Maybe Text
    , _dtTasks   :: List "tasks" Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DescribeTasks' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtCluster' @::@ 'Maybe' 'Text'
--
-- * 'dtTasks' @::@ ['Text']
--
describeTasks :: DescribeTasks
describeTasks = DescribeTasks
    { _dtCluster = Nothing
    , _dtTasks   = mempty
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts
-- the task you want to describe. If you do not specify a cluster, the default
-- cluster is assumed.
dtCluster :: Lens' DescribeTasks (Maybe Text)
dtCluster = lens _dtCluster (\s a -> s { _dtCluster = a })

-- | A space-separated list of task UUIDs or full Amazon Resource Name (ARN)
-- entries.
dtTasks :: Lens' DescribeTasks [Text]
dtTasks = lens _dtTasks (\s a -> s { _dtTasks = a }) . _List

data DescribeTasksResponse = DescribeTasksResponse
    { _dtrFailures :: List "failures" Failure
    , _dtrTasks    :: List "tasks" Task
    } deriving (Eq, Read, Show)

-- | 'DescribeTasksResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtrFailures' @::@ ['Failure']
--
-- * 'dtrTasks' @::@ ['Task']
--
describeTasksResponse :: DescribeTasksResponse
describeTasksResponse = DescribeTasksResponse
    { _dtrTasks    = mempty
    , _dtrFailures = mempty
    }

dtrFailures :: Lens' DescribeTasksResponse [Failure]
dtrFailures = lens _dtrFailures (\s a -> s { _dtrFailures = a }) . _List

-- | The list of tasks.
dtrTasks :: Lens' DescribeTasksResponse [Task]
dtrTasks = lens _dtrTasks (\s a -> s { _dtrTasks = a }) . _List

instance ToPath DescribeTasks where
    toPath = const "/"

instance ToQuery DescribeTasks where
    toQuery = const mempty

instance ToHeaders DescribeTasks

instance ToJSON DescribeTasks where
    toJSON DescribeTasks{..} = object
        [ "cluster" .= _dtCluster
        , "tasks"   .= _dtTasks
        ]

instance AWSRequest DescribeTasks where
    type Sv DescribeTasks = ECS
    type Rs DescribeTasks = DescribeTasksResponse

    request  = post "DescribeTasks"
    response = jsonResponse

instance FromJSON DescribeTasksResponse where
    parseJSON = withObject "DescribeTasksResponse" $ \o -> DescribeTasksResponse
        <$> o .:? "failures" .!= mempty
        <*> o .:? "tasks" .!= mempty
