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

-- | Registers a new task definition from the supplied 'family' and 'containerDefinitions'.
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

    -- * Response
    , RegisterTaskDefinitionResponse
    -- ** Response constructor
    , registerTaskDefinitionResponse
    -- ** Response lenses
    , rtdrTaskDefinition
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ECS.Types
import qualified GHC.Exts

data RegisterTaskDefinition = RegisterTaskDefinition
    { _rtdContainerDefinitions :: List "member" ContainerDefinition
    , _rtdFamily               :: Text
    } deriving (Eq, Read, Show)

-- | 'RegisterTaskDefinition' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtdContainerDefinitions' @::@ ['ContainerDefinition']
--
-- * 'rtdFamily' @::@ 'Text'
--
registerTaskDefinition :: Text -- ^ 'rtdFamily'
                       -> RegisterTaskDefinition
registerTaskDefinition p1 = RegisterTaskDefinition
    { _rtdFamily               = p1
    , _rtdContainerDefinitions = mempty
    }

-- | A list of container definitions in JSON format that describe the different
-- containers that make up your task.
rtdContainerDefinitions :: Lens' RegisterTaskDefinition [ContainerDefinition]
rtdContainerDefinitions =
    lens _rtdContainerDefinitions (\s a -> s { _rtdContainerDefinitions = a })
        . _List

-- | You can specify a 'family' for a task definition, which allows you to track
-- multiple versions of the same task definition. You can think of the 'family' as
-- a name for your task definition.
rtdFamily :: Lens' RegisterTaskDefinition Text
rtdFamily = lens _rtdFamily (\s a -> s { _rtdFamily = a })

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
    toQuery RegisterTaskDefinition{..} = mconcat
        [ "containerDefinitions" =? _rtdContainerDefinitions
        , "family"               =? _rtdFamily
        ]

instance ToHeaders RegisterTaskDefinition

instance AWSRequest RegisterTaskDefinition where
    type Sv RegisterTaskDefinition = ECS
    type Rs RegisterTaskDefinition = RegisterTaskDefinitionResponse

    request  = post "RegisterTaskDefinition"
    response = xmlResponse

instance FromXML RegisterTaskDefinitionResponse where
    parseXML = withElement "RegisterTaskDefinitionResult" $ \x -> RegisterTaskDefinitionResponse
        <$> x .@? "taskDefinition"
