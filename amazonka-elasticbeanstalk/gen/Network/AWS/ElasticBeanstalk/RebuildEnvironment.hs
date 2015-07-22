{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.RebuildEnvironment
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes and recreates all of the AWS resources (for example: the Auto
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
    , rerqEnvironmentName
    , rerqEnvironmentId

    -- * Response
    , RebuildEnvironmentResponse
    -- ** Response constructor
    , rebuildEnvironmentResponse
    ) where

import           Network.AWS.ElasticBeanstalk.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'rebuildEnvironment' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rerqEnvironmentName'
--
-- * 'rerqEnvironmentId'
data RebuildEnvironment = RebuildEnvironment'
    { _rerqEnvironmentName :: !(Maybe Text)
    , _rerqEnvironmentId   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RebuildEnvironment' smart constructor.
rebuildEnvironment :: RebuildEnvironment
rebuildEnvironment =
    RebuildEnvironment'
    { _rerqEnvironmentName = Nothing
    , _rerqEnvironmentId = Nothing
    }

-- | The name of the environment to rebuild.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
rerqEnvironmentName :: Lens' RebuildEnvironment (Maybe Text)
rerqEnvironmentName = lens _rerqEnvironmentName (\ s a -> s{_rerqEnvironmentName = a});

-- | The ID of the environment to rebuild.
--
-- Condition: You must specify either this or an EnvironmentName, or both.
-- If you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
rerqEnvironmentId :: Lens' RebuildEnvironment (Maybe Text)
rerqEnvironmentId = lens _rerqEnvironmentId (\ s a -> s{_rerqEnvironmentId = a});

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
               "EnvironmentName" =: _rerqEnvironmentName,
               "EnvironmentId" =: _rerqEnvironmentId]

-- | /See:/ 'rebuildEnvironmentResponse' smart constructor.
data RebuildEnvironmentResponse =
    RebuildEnvironmentResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RebuildEnvironmentResponse' smart constructor.
rebuildEnvironmentResponse :: RebuildEnvironmentResponse
rebuildEnvironmentResponse = RebuildEnvironmentResponse'
