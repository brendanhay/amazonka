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

-- Module      : Network.AWS.ECS.RegisterTaskDefinition
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

-- | Registers a new task definition from the supplied 'family' and 'containerDefinitions'. Optionally, you can add data volumes to your containers with the 'volumes'
-- parameter. For more information on task definition parameters and defaults,
-- see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_defintions.html Amazon ECS Task Definitions> in the /Amazon EC2 Container Service DeveloperGuide/.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_RegisterTaskDefinition.html>
module Network.AWS.ECS.RegisterTaskDefinition
    (
    -- * Request
      RegisterTaskDefinition
    -- ** Request constructor
    , registerTaskDefinition
    -- ** Request lenses
    , rtdContainerDefinitions
    , rtdFamily
    , rtdVolumes

    -- * Response
    , RegisterTaskDefinitionResponse
    -- ** Response constructor
    , registerTaskDefinitionResponse
    -- ** Response lenses
    , rtdrTaskDefinition
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.ECS.Types
import qualified GHC.Exts

data RegisterTaskDefinition = RegisterTaskDefinition
    { _rtdContainerDefinitions :: List "containerDefinitions" ContainerDefinition
    , _rtdFamily               :: Text
    , _rtdVolumes              :: List "volumes" Volume
    } deriving (Eq, Read, Show)

-- | 'RegisterTaskDefinition' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtdContainerDefinitions' @::@ ['ContainerDefinition']
--
-- * 'rtdFamily' @::@ 'Text'
--
-- * 'rtdVolumes' @::@ ['Volume']
--
registerTaskDefinition :: Text -- ^ 'rtdFamily'
                       -> RegisterTaskDefinition
registerTaskDefinition p1 = RegisterTaskDefinition
    { _rtdFamily               = p1
    , _rtdContainerDefinitions = mempty
    , _rtdVolumes              = mempty
    }

-- | A list of container definitions in JSON format that describe the different
-- containers that make up your task.
rtdContainerDefinitions :: Lens' RegisterTaskDefinition [ContainerDefinition]
rtdContainerDefinitions =
    lens _rtdContainerDefinitions (\s a -> s { _rtdContainerDefinitions = a })
        . _List

-- | You must specify a 'family' for a task definition, which allows you to track
-- multiple versions of the same task definition. You can think of the 'family' as
-- a name for your task definition. Up to 255 letters (uppercase and lowercase),
-- numbers, hyphens, and underscores are allowed.
rtdFamily :: Lens' RegisterTaskDefinition Text
rtdFamily = lens _rtdFamily (\s a -> s { _rtdFamily = a })

-- | A list of volume definitions in JSON format that containers in your task may
-- use.
rtdVolumes :: Lens' RegisterTaskDefinition [Volume]
rtdVolumes = lens _rtdVolumes (\s a -> s { _rtdVolumes = a }) . _List

newtype RegisterTaskDefinitionResponse = RegisterTaskDefinitionResponse
    { _rtdrTaskDefinition :: Maybe TaskDefinition
    } deriving (Eq, Read, Show)

-- | 'RegisterTaskDefinitionResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtdrTaskDefinition' @::@ 'Maybe' 'TaskDefinition'
--
registerTaskDefinitionResponse :: RegisterTaskDefinitionResponse
registerTaskDefinitionResponse = RegisterTaskDefinitionResponse
    { _rtdrTaskDefinition = Nothing
    }

rtdrTaskDefinition :: Lens' RegisterTaskDefinitionResponse (Maybe TaskDefinition)
rtdrTaskDefinition =
    lens _rtdrTaskDefinition (\s a -> s { _rtdrTaskDefinition = a })

instance ToPath RegisterTaskDefinition where
    toPath = const "/"

instance ToQuery RegisterTaskDefinition where
    toQuery = const mempty

instance ToHeaders RegisterTaskDefinition

instance ToJSON RegisterTaskDefinition where
    toJSON RegisterTaskDefinition{..} = object
        [ "family"               .= _rtdFamily
        , "containerDefinitions" .= _rtdContainerDefinitions
        , "volumes"              .= _rtdVolumes
        ]

instance AWSRequest RegisterTaskDefinition where
    type Sv RegisterTaskDefinition = ECS
    type Rs RegisterTaskDefinition = RegisterTaskDefinitionResponse

    request  = post "RegisterTaskDefinition"
    response = jsonResponse

instance FromJSON RegisterTaskDefinitionResponse where
    parseJSON = withObject "RegisterTaskDefinitionResponse" $ \o -> RegisterTaskDefinitionResponse
        <$> o .:? "taskDefinition"
