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
      RebuildEnvironment
    -- ** Request constructor
    , rebuildEnvironment
    -- ** Request lenses
    , reEnvironmentId
    , reEnvironmentName

    -- * Response
    , RebuildEnvironmentResponse
    -- ** Response constructor
    , rebuildEnvironmentResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types

data RebuildEnvironment = RebuildEnvironment
    { _reEnvironmentId   :: Maybe Text
    , _reEnvironmentName :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'RebuildEnvironment' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'reEnvironmentId' @::@ 'Maybe' 'Text'
--
-- * 'reEnvironmentName' @::@ 'Maybe' 'Text'
--
rebuildEnvironment :: RebuildEnvironment
rebuildEnvironment = RebuildEnvironment
    { _reEnvironmentId   = Nothing
    , _reEnvironmentName = Nothing
    }

-- | The ID of the environment to rebuild. Condition: You must specify either
-- this or an EnvironmentName, or both. If you do not specify either, AWS
-- Elastic Beanstalk returns MissingRequiredParameter error.
reEnvironmentId :: Lens' RebuildEnvironment (Maybe Text)
reEnvironmentId = lens _reEnvironmentId (\s a -> s { _reEnvironmentId = a })

-- | The name of the environment to rebuild. Condition: You must specify
-- either this or an EnvironmentId, or both. If you do not specify either,
-- AWS Elastic Beanstalk returns MissingRequiredParameter error.
reEnvironmentName :: Lens' RebuildEnvironment (Maybe Text)
reEnvironmentName =
    lens _reEnvironmentName (\s a -> s { _reEnvironmentName = a })

instance ToQuery RebuildEnvironment

instance ToPath RebuildEnvironment where
    toPath = const "/"

data RebuildEnvironmentResponse = RebuildEnvironmentResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'RebuildEnvironmentResponse' constructor.
rebuildEnvironmentResponse :: RebuildEnvironmentResponse
rebuildEnvironmentResponse = RebuildEnvironmentResponse

instance FromXML RebuildEnvironmentResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RebuildEnvironmentResponse"

instance AWSRequest RebuildEnvironment where
    type Sv RebuildEnvironment = ElasticBeanstalk
    type Rs RebuildEnvironment = RebuildEnvironmentResponse

    request  = post "RebuildEnvironment"
    response = nullaryResponse RebuildEnvironmentResponse
