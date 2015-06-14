{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ElasticBeanstalk.RebuildEnvironment
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

-- | Deletes and recreates all of the AWS resources (for example: the Auto
-- Scaling group, load balancer, etc.) for a specified environment and
-- forces a restart.
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_RebuildEnvironment.html>
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

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.ElasticBeanstalk.Types

-- | /See:/ 'rebuildEnvironment' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'reEnvironmentId'
--
-- * 'reEnvironmentName'
data RebuildEnvironment = RebuildEnvironment'{_reEnvironmentId :: Maybe Text, _reEnvironmentName :: Text} deriving (Eq, Read, Show)

-- | 'RebuildEnvironment' smart constructor.
rebuildEnvironment :: Text -> RebuildEnvironment
rebuildEnvironment pEnvironmentName = RebuildEnvironment'{_reEnvironmentId = Nothing, _reEnvironmentName = pEnvironmentName};

-- | The ID of the environment to rebuild.
--
-- Condition: You must specify either this or an EnvironmentName, or both.
-- If you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
reEnvironmentId :: Lens' RebuildEnvironment (Maybe Text)
reEnvironmentId = lens _reEnvironmentId (\ s a -> s{_reEnvironmentId = a});

-- | The name of the environment to rebuild.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
reEnvironmentName :: Lens' RebuildEnvironment Text
reEnvironmentName = lens _reEnvironmentName (\ s a -> s{_reEnvironmentName = a});

instance AWSRequest RebuildEnvironment where
        type Sv RebuildEnvironment = ElasticBeanstalk
        type Rs RebuildEnvironment =
             RebuildEnvironmentResponse
        request = post
        response = receiveNull RebuildEnvironmentResponse'

instance ToHeaders RebuildEnvironment where
        toHeaders = const mempty

instance ToPath RebuildEnvironment where
        toPath = const "/"

instance ToQuery RebuildEnvironment where
        toQuery RebuildEnvironment'{..}
          = mconcat
              ["Action" =: ("RebuildEnvironment" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "EnvironmentId" =: _reEnvironmentId,
               "EnvironmentName" =: _reEnvironmentName]

-- | /See:/ 'rebuildEnvironmentResponse' smart constructor.
data RebuildEnvironmentResponse = RebuildEnvironmentResponse' deriving (Eq, Read, Show)

-- | 'RebuildEnvironmentResponse' smart constructor.
rebuildEnvironmentResponse :: RebuildEnvironmentResponse
rebuildEnvironmentResponse = RebuildEnvironmentResponse';
