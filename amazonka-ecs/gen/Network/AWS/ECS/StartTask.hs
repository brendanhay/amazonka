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

-- Module      : Network.AWS.ECS.StartTask
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

-- | Starts a new task from the specified task definition on the specified
-- container instance or instances. If you want to use the default Amazon ECS
-- scheduler to place your task, use 'RunTask' instead.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_StartTask.html>
module Network.AWS.ECS.StartTask
    (
    -- * Request
      StartTask
    -- ** Request constructor
    , startTask
    -- ** Request lenses
    , st1Cluster
    , st1ContainerInstances
    , st1Overrides
    , st1TaskDefinition

    -- * Response
    , StartTaskResponse
    -- ** Response constructor
    , startTaskResponse
    -- ** Response lenses
    , strFailures
    , strTasks
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ECS.Types
import qualified GHC.Exts

data StartTask = StartTask
    { _st1Cluster            :: Maybe Text
    , _st1ContainerInstances :: List "member" Text
    , _st1Overrides          :: Maybe TaskOverride
    , _st1TaskDefinition     :: Text
    } deriving (Eq, Read, Show)

-- | 'StartTask' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'st1Cluster' @::@ 'Maybe' 'Text'
--
-- * 'st1ContainerInstances' @::@ ['Text']
--
-- * 'st1Overrides' @::@ 'Maybe' 'TaskOverride'
--
-- * 'st1TaskDefinition' @::@ 'Text'
--
startTask :: Text -- ^ 'st1TaskDefinition'
          -> StartTask
startTask p1 = StartTask
    { _st1TaskDefinition     = p1
    , _st1Cluster            = Nothing
    , _st1Overrides          = Nothing
    , _st1ContainerInstances = mempty
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that you
-- want to start your task on. If you do not specify a cluster, the default
-- cluster is assumed..
st1Cluster :: Lens' StartTask (Maybe Text)
st1Cluster = lens _st1Cluster (\s a -> s { _st1Cluster = a })

-- | The container instance UUIDs or full Amazon Resource Name (ARN) entries for
-- the container instances on which you would like to place your task.
st1ContainerInstances :: Lens' StartTask [Text]
st1ContainerInstances =
    lens _st1ContainerInstances (\s a -> s { _st1ContainerInstances = a })
        . _List

st1Overrides :: Lens' StartTask (Maybe TaskOverride)
st1Overrides = lens _st1Overrides (\s a -> s { _st1Overrides = a })

-- | The 'family' and 'revision' ('family:revision') or full Amazon Resource Name (ARN)
-- of the task definition that you want to start.
st1TaskDefinition :: Lens' StartTask Text
st1TaskDefinition =
    lens _st1TaskDefinition (\s a -> s { _st1TaskDefinition = a })

data StartTaskResponse = StartTaskResponse
    { _strFailures :: List "member" Failure
    , _strTasks    :: List "member" Task
    } deriving (Eq, Read, Show)

-- | 'StartTaskResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'strFailures' @::@ ['Failure']
--
-- * 'strTasks' @::@ ['Task']
--
startTaskResponse :: StartTaskResponse
startTaskResponse = StartTaskResponse
    { _strTasks    = mempty
    , _strFailures = mempty
    }

-- | Any failed tasks from your 'StartTask' action are listed here.
strFailures :: Lens' StartTaskResponse [Failure]
strFailures = lens _strFailures (\s a -> s { _strFailures = a }) . _List

-- | A full description of the tasks that were started. Each task that was
-- successfully placed on your container instances will be described here.
strTasks :: Lens' StartTaskResponse [Task]
strTasks = lens _strTasks (\s a -> s { _strTasks = a }) . _List

instance ToPath StartTask where
    toPath = const "/"

instance ToQuery StartTask where
    toQuery StartTask{..} = mconcat
        [ "cluster"            =? _st1Cluster
        , "containerInstances" =? _st1ContainerInstances
        , "overrides"          =? _st1Overrides
        , "taskDefinition"     =? _st1TaskDefinition
        ]

instance ToHeaders StartTask

instance AWSRequest StartTask where
    type Sv StartTask = ECS
    type Rs StartTask = StartTaskResponse

    request  = post "StartTask"
    response = xmlResponse

instance FromXML StartTaskResponse where
    parseXML = withElement "StartTaskResult" $ \x -> StartTaskResponse
        <$> x .@? "failures" .!@ mempty
        <*> x .@? "tasks" .!@ mempty
