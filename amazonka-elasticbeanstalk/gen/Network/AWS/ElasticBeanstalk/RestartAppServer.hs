{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
      RestartAppServer
    -- ** Request constructor
    , restartAppServer
    -- ** Request lenses
    , rasEnvironmentId
    , rasEnvironmentName

    -- * Response
    , RestartAppServerResponse
    -- ** Response constructor
    , restartAppServerResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types
import qualified GHC.Exts

data RestartAppServer = RestartAppServer
    { _rasEnvironmentId   :: Maybe Text
    , _rasEnvironmentName :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'RestartAppServer' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rasEnvironmentId' @::@ 'Maybe' 'Text'
--
-- * 'rasEnvironmentName' @::@ 'Maybe' 'Text'
--
restartAppServer :: RestartAppServer
restartAppServer = RestartAppServer
    { _rasEnvironmentId   = Nothing
    , _rasEnvironmentName = Nothing
    }

-- | The ID of the environment to restart the server for. Condition: You must
-- specify either this or an EnvironmentName, or both. If you do not specify
-- either, AWS Elastic Beanstalk returns MissingRequiredParameter error.
rasEnvironmentId :: Lens' RestartAppServer (Maybe Text)
rasEnvironmentId = lens _rasEnvironmentId (\s a -> s { _rasEnvironmentId = a })

-- | The name of the environment to restart the server for. Condition: You
-- must specify either this or an EnvironmentId, or both. If you do not
-- specify either, AWS Elastic Beanstalk returns MissingRequiredParameter
-- error.
rasEnvironmentName :: Lens' RestartAppServer (Maybe Text)
rasEnvironmentName =
    lens _rasEnvironmentName (\s a -> s { _rasEnvironmentName = a })

data RestartAppServerResponse = RestartAppServerResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'RestartAppServerResponse' constructor.
restartAppServerResponse :: RestartAppServerResponse
restartAppServerResponse = RestartAppServerResponse

instance AWSRequest RestartAppServer where
    type Sv RestartAppServer = ElasticBeanstalk
    type Rs RestartAppServer = RestartAppServerResponse

    request  = post "RestartAppServer"
    response = nullResponse RestartAppServerResponse

instance ToPath RestartAppServer where
    toPath = const "/"

instance ToHeaders RestartAppServer

instance ToQuery RestartAppServer
