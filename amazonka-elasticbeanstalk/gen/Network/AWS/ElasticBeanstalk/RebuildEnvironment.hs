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
-- Module      : Network.AWS.ElasticBeanstalk.RebuildEnvironment
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes and recreates all of the AWS resources (for example: the Auto
-- Scaling group, load balancer, etc.) for a specified environment and
-- forces a restart.
--
-- /See:/ <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_RebuildEnvironment.html AWS API Reference> for RebuildEnvironment.
module Network.AWS.ElasticBeanstalk.RebuildEnvironment
    (
    -- * Creating a Request
      RebuildEnvironment
    , rebuildEnvironment
    -- * Request Lenses
    , reEnvironmentName
    , reEnvironmentId

    -- * Destructuring the Response
    , RebuildEnvironmentResponse
    , rebuildEnvironmentResponse
    ) where

import           Network.AWS.ElasticBeanstalk.Types
import           Network.AWS.ElasticBeanstalk.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'rebuildEnvironment' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'reEnvironmentName'
--
-- * 'reEnvironmentId'
data RebuildEnvironment = RebuildEnvironment'
    { _reEnvironmentName :: !(Maybe Text)
    , _reEnvironmentId   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RebuildEnvironment' smart constructor.
rebuildEnvironment :: RebuildEnvironment
rebuildEnvironment =
    RebuildEnvironment'
    { _reEnvironmentName = Nothing
    , _reEnvironmentId = Nothing
    }

-- | The name of the environment to rebuild.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
reEnvironmentName :: Lens' RebuildEnvironment (Maybe Text)
reEnvironmentName = lens _reEnvironmentName (\ s a -> s{_reEnvironmentName = a});

-- | The ID of the environment to rebuild.
--
-- Condition: You must specify either this or an EnvironmentName, or both.
-- If you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
reEnvironmentId :: Lens' RebuildEnvironment (Maybe Text)
reEnvironmentId = lens _reEnvironmentId (\ s a -> s{_reEnvironmentId = a});

instance AWSRequest RebuildEnvironment where
        type Sv RebuildEnvironment = ElasticBeanstalk
        type Rs RebuildEnvironment =
             RebuildEnvironmentResponse
        request = postQuery
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
               "EnvironmentName" =: _reEnvironmentName,
               "EnvironmentId" =: _reEnvironmentId]

-- | /See:/ 'rebuildEnvironmentResponse' smart constructor.
data RebuildEnvironmentResponse =
    RebuildEnvironmentResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RebuildEnvironmentResponse' smart constructor.
rebuildEnvironmentResponse :: RebuildEnvironmentResponse
rebuildEnvironmentResponse = RebuildEnvironmentResponse'
