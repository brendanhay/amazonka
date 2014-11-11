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

-- Module      : Network.AWS.ElasticBeanstalk.RestartAppServer
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Causes the environment to restart the application container server running
-- on each Amazon EC2 instance.
module Network.AWS.ElasticBeanstalk.RestartAppServer
    (
    -- * Request
      RestartAppServerMessage
    -- ** Request constructor
    , restartAppServerMessage
    -- ** Request lenses
    , rasmEnvironmentId
    , rasmEnvironmentName

    -- * Response
    , RestartAppServerResponse
    -- ** Response constructor
    , restartAppServerResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types

data RestartAppServerMessage = RestartAppServerMessage
    { _rasmEnvironmentId   :: Maybe Text
    , _rasmEnvironmentName :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'RestartAppServerMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rasmEnvironmentId' @::@ 'Maybe' 'Text'
--
-- * 'rasmEnvironmentName' @::@ 'Maybe' 'Text'
--
restartAppServerMessage :: RestartAppServerMessage
restartAppServerMessage = RestartAppServerMessage
    { _rasmEnvironmentId   = Nothing
    , _rasmEnvironmentName = Nothing
    }

-- | The ID of the environment to restart the server for. Condition: You must
-- specify either this or an EnvironmentName, or both. If you do not specify
-- either, AWS Elastic Beanstalk returns MissingRequiredParameter error.
rasmEnvironmentId :: Lens' RestartAppServerMessage (Maybe Text)
rasmEnvironmentId =
    lens _rasmEnvironmentId (\s a -> s { _rasmEnvironmentId = a })

-- | The name of the environment to restart the server for. Condition: You
-- must specify either this or an EnvironmentId, or both. If you do not
-- specify either, AWS Elastic Beanstalk returns MissingRequiredParameter
-- error.
rasmEnvironmentName :: Lens' RestartAppServerMessage (Maybe Text)
rasmEnvironmentName =
    lens _rasmEnvironmentName (\s a -> s { _rasmEnvironmentName = a })
instance ToQuery RestartAppServerMessage

instance ToPath RestartAppServerMessage where
    toPath = const "/"

data RestartAppServerResponse = RestartAppServerResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'RestartAppServerResponse' constructor.
restartAppServerResponse :: RestartAppServerResponse
restartAppServerResponse = RestartAppServerResponse
instance FromXML RestartAppServerResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RestartAppServerResponse"

instance AWSRequest RestartAppServerMessage where
    type Sv RestartAppServerMessage = ElasticBeanstalk
    type Rs RestartAppServerMessage = RestartAppServerResponse

    request  = post "RestartAppServer"
    response = nullaryResponse RestartAppServerResponse
