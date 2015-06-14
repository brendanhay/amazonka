{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ElasticBeanstalk.RestartAppServer
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Causes the environment to restart the application container server
-- running on each Amazon EC2 instance.
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_RestartAppServer.html>
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

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.ElasticBeanstalk.Types

-- | /See:/ 'restartAppServer' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rasEnvironmentId'
--
-- * 'rasEnvironmentName'
data RestartAppServer = RestartAppServer'{_rasEnvironmentId :: Maybe Text, _rasEnvironmentName :: Text} deriving (Eq, Read, Show)

-- | 'RestartAppServer' smart constructor.
restartAppServer :: Text -> RestartAppServer
restartAppServer pEnvironmentName = RestartAppServer'{_rasEnvironmentId = Nothing, _rasEnvironmentName = pEnvironmentName};

-- | The ID of the environment to restart the server for.
--
-- Condition: You must specify either this or an EnvironmentName, or both.
-- If you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
rasEnvironmentId :: Lens' RestartAppServer (Maybe Text)
rasEnvironmentId = lens _rasEnvironmentId (\ s a -> s{_rasEnvironmentId = a});

-- | The name of the environment to restart the server for.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
rasEnvironmentName :: Lens' RestartAppServer Text
rasEnvironmentName = lens _rasEnvironmentName (\ s a -> s{_rasEnvironmentName = a});

instance AWSRequest RestartAppServer where
        type Sv RestartAppServer = ElasticBeanstalk
        type Rs RestartAppServer = RestartAppServerResponse
        request = post
        response = receiveNull RestartAppServerResponse'

instance ToHeaders RestartAppServer where
        toHeaders = const mempty

instance ToPath RestartAppServer where
        toPath = const "/"

instance ToQuery RestartAppServer where
        toQuery RestartAppServer'{..}
          = mconcat
              ["Action" =: ("RestartAppServer" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "EnvironmentId" =: _rasEnvironmentId,
               "EnvironmentName" =: _rasEnvironmentName]

-- | /See:/ 'restartAppServerResponse' smart constructor.
data RestartAppServerResponse = RestartAppServerResponse' deriving (Eq, Read, Show)

-- | 'RestartAppServerResponse' smart constructor.
restartAppServerResponse :: RestartAppServerResponse
restartAppServerResponse = RestartAppServerResponse';
