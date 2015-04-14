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

-- Module      : Network.AWS.ECS.DeregisterTaskDefinition
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

-- | NOT YET IMPLEMENTED.
--
-- Deregisters the specified task definition. You will no longer be able to run
-- tasks from this definition after deregistration.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DeregisterTaskDefinition.html>
module Network.AWS.ECS.DeregisterTaskDefinition
    (
    -- * Request
      DeregisterTaskDefinition
    -- ** Request constructor
    , deregisterTaskDefinition
    -- ** Request lenses
    , dtd1TaskDefinition

    -- * Response
    , DeregisterTaskDefinitionResponse
    -- ** Response constructor
    , deregisterTaskDefinitionResponse
    -- ** Response lenses
    , dtdrTaskDefinition
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.ECS.Types
import qualified GHC.Exts

newtype DeregisterTaskDefinition = DeregisterTaskDefinition
    { _dtd1TaskDefinition :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'DeregisterTaskDefinition' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtd1TaskDefinition' @::@ 'Text'
--
deregisterTaskDefinition :: Text -- ^ 'dtd1TaskDefinition'
                         -> DeregisterTaskDefinition
deregisterTaskDefinition p1 = DeregisterTaskDefinition
    { _dtd1TaskDefinition = p1
    }

-- | The 'family' and 'revision' ('family:revision') or full Amazon Resource Name (ARN)
-- of the task definition that you want to deregister.
dtd1TaskDefinition :: Lens' DeregisterTaskDefinition Text
dtd1TaskDefinition =
    lens _dtd1TaskDefinition (\s a -> s { _dtd1TaskDefinition = a })

newtype DeregisterTaskDefinitionResponse = DeregisterTaskDefinitionResponse
    { _dtdrTaskDefinition :: Maybe TaskDefinition
    } deriving (Eq, Read, Show)

-- | 'DeregisterTaskDefinitionResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtdrTaskDefinition' @::@ 'Maybe' 'TaskDefinition'
--
deregisterTaskDefinitionResponse :: DeregisterTaskDefinitionResponse
deregisterTaskDefinitionResponse = DeregisterTaskDefinitionResponse
    { _dtdrTaskDefinition = Nothing
    }

-- | The full description of the deregistered task.
dtdrTaskDefinition :: Lens' DeregisterTaskDefinitionResponse (Maybe TaskDefinition)
dtdrTaskDefinition =
    lens _dtdrTaskDefinition (\s a -> s { _dtdrTaskDefinition = a })

instance ToPath DeregisterTaskDefinition where
    toPath = const "/"

instance ToQuery DeregisterTaskDefinition where
    toQuery = const mempty

instance ToHeaders DeregisterTaskDefinition

instance ToJSON DeregisterTaskDefinition where
    toJSON DeregisterTaskDefinition{..} = object
        [ "taskDefinition" .= _dtd1TaskDefinition
        ]

instance AWSRequest DeregisterTaskDefinition where
    type Sv DeregisterTaskDefinition = ECS
    type Rs DeregisterTaskDefinition = DeregisterTaskDefinitionResponse

    request  = post "DeregisterTaskDefinition"
    response = jsonResponse

instance FromJSON DeregisterTaskDefinitionResponse where
    parseJSON = withObject "DeregisterTaskDefinitionResponse" $ \o -> DeregisterTaskDefinitionResponse
        <$> o .:? "taskDefinition"
