{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.RestartAppServer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Causes the environment to restart the application container server running on each Amazon EC2 instance.
--
--
module Network.AWS.ElasticBeanstalk.RestartAppServer
    (
    -- * Creating a Request
      restartAppServer
    , RestartAppServer
    -- * Request Lenses
    , rasEnvironmentName
    , rasEnvironmentId

    -- * Destructuring the Response
    , restartAppServerResponse
    , RestartAppServerResponse
    ) where

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.ElasticBeanstalk.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'restartAppServer' smart constructor.
data RestartAppServer = RestartAppServer'
  { _rasEnvironmentName :: !(Maybe Text)
  , _rasEnvironmentId   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RestartAppServer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rasEnvironmentName' - The name of the environment to restart the server for. Condition: You must specify either this or an EnvironmentId, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
--
-- * 'rasEnvironmentId' - The ID of the environment to restart the server for. Condition: You must specify either this or an EnvironmentName, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
restartAppServer
    :: RestartAppServer
restartAppServer =
  RestartAppServer' {_rasEnvironmentName = Nothing, _rasEnvironmentId = Nothing}


-- | The name of the environment to restart the server for. Condition: You must specify either this or an EnvironmentId, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
rasEnvironmentName :: Lens' RestartAppServer (Maybe Text)
rasEnvironmentName = lens _rasEnvironmentName (\ s a -> s{_rasEnvironmentName = a})

-- | The ID of the environment to restart the server for. Condition: You must specify either this or an EnvironmentName, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
rasEnvironmentId :: Lens' RestartAppServer (Maybe Text)
rasEnvironmentId = lens _rasEnvironmentId (\ s a -> s{_rasEnvironmentId = a})

instance AWSRequest RestartAppServer where
        type Rs RestartAppServer = RestartAppServerResponse
        request = postQuery elasticBeanstalk
        response = receiveNull RestartAppServerResponse'

instance Hashable RestartAppServer where

instance NFData RestartAppServer where

instance ToHeaders RestartAppServer where
        toHeaders = const mempty

instance ToPath RestartAppServer where
        toPath = const "/"

instance ToQuery RestartAppServer where
        toQuery RestartAppServer'{..}
          = mconcat
              ["Action" =: ("RestartAppServer" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "EnvironmentName" =: _rasEnvironmentName,
               "EnvironmentId" =: _rasEnvironmentId]

-- | /See:/ 'restartAppServerResponse' smart constructor.
data RestartAppServerResponse =
  RestartAppServerResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RestartAppServerResponse' with the minimum fields required to make a request.
--
restartAppServerResponse
    :: RestartAppServerResponse
restartAppServerResponse = RestartAppServerResponse'


instance NFData RestartAppServerResponse where
