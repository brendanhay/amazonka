{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.ElasticBeanstalk.RebuildEnvironment
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes and recreates all of the AWS resources (for example: the Auto
-- Scaling group, load balancer, etc.) for a specified environment and forces
-- a restart.
module Network.AWS.ElasticBeanstalk.RebuildEnvironment
    (
    -- * Request
      RebuildEnvironmentMessage
    -- ** Request constructor
    , rebuildEnvironmentMessage
    -- ** Request lenses
    , remEnvironmentId
    , remEnvironmentName

    -- * Response
    , RebuildEnvironmentResponse
    -- ** Response constructor
    , rebuildEnvironmentResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types

data RebuildEnvironmentMessage = RebuildEnvironmentMessage
    { _remEnvironmentId   :: Maybe Text
    , _remEnvironmentName :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'RebuildEnvironmentMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'remEnvironmentId' @::@ 'Maybe' 'Text'
--
-- * 'remEnvironmentName' @::@ 'Maybe' 'Text'
--
rebuildEnvironmentMessage :: RebuildEnvironmentMessage
rebuildEnvironmentMessage = RebuildEnvironmentMessage
    { _remEnvironmentId   = Nothing
    , _remEnvironmentName = Nothing
    }

-- | The ID of the environment to rebuild. Condition: You must specify either
-- this or an EnvironmentName, or both. If you do not specify either, AWS
-- Elastic Beanstalk returns MissingRequiredParameter error.
remEnvironmentId :: Lens' RebuildEnvironmentMessage (Maybe Text)
remEnvironmentId = lens _remEnvironmentId (\s a -> s { _remEnvironmentId = a })

-- | The name of the environment to rebuild. Condition: You must specify
-- either this or an EnvironmentId, or both. If you do not specify either,
-- AWS Elastic Beanstalk returns MissingRequiredParameter error.
remEnvironmentName :: Lens' RebuildEnvironmentMessage (Maybe Text)
remEnvironmentName =
    lens _remEnvironmentName (\s a -> s { _remEnvironmentName = a })

instance ToQuery RebuildEnvironmentMessage

instance ToPath RebuildEnvironmentMessage where
    toPath = const "/"

data RebuildEnvironmentResponse = RebuildEnvironmentResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'RebuildEnvironmentResponse' constructor.
rebuildEnvironmentResponse :: RebuildEnvironmentResponse
rebuildEnvironmentResponse = RebuildEnvironmentResponse

instance FromXML RebuildEnvironmentResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RebuildEnvironmentResponse"

instance AWSRequest RebuildEnvironmentMessage where
    type Sv RebuildEnvironmentMessage = ElasticBeanstalk
    type Rs RebuildEnvironmentMessage = RebuildEnvironmentResponse

    request  = post "RebuildEnvironment"
    response = nullaryResponse RebuildEnvironmentResponse
