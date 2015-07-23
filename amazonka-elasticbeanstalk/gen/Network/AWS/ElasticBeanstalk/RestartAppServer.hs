{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.RestartAppServer
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Causes the environment to restart the application container server
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
    , rasrqEnvironmentName
    , rasrqEnvironmentId

    -- * Response
    , RestartAppServerResponse
    -- ** Response constructor
    , restartAppServerResponse
    ) where

import           Network.AWS.ElasticBeanstalk.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'restartAppServer' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rasrqEnvironmentName'
--
-- * 'rasrqEnvironmentId'
data RestartAppServer = RestartAppServer'
    { _rasrqEnvironmentName :: !(Maybe Text)
    , _rasrqEnvironmentId   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RestartAppServer' smart constructor.
restartAppServer :: RestartAppServer
restartAppServer =
    RestartAppServer'
    { _rasrqEnvironmentName = Nothing
    , _rasrqEnvironmentId = Nothing
    }

-- | The name of the environment to restart the server for.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
rasrqEnvironmentName :: Lens' RestartAppServer (Maybe Text)
rasrqEnvironmentName = lens _rasrqEnvironmentName (\ s a -> s{_rasrqEnvironmentName = a});

-- | The ID of the environment to restart the server for.
--
-- Condition: You must specify either this or an EnvironmentName, or both.
-- If you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
rasrqEnvironmentId :: Lens' RestartAppServer (Maybe Text)
rasrqEnvironmentId = lens _rasrqEnvironmentId (\ s a -> s{_rasrqEnvironmentId = a});

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
               "EnvironmentName" =: _rasrqEnvironmentName,
               "EnvironmentId" =: _rasrqEnvironmentId]

-- | /See:/ 'restartAppServerResponse' smart constructor.
data RestartAppServerResponse =
    RestartAppServerResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RestartAppServerResponse' smart constructor.
restartAppServerResponse :: RestartAppServerResponse
restartAppServerResponse = RestartAppServerResponse'
