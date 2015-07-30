{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.DiscoverPollEndpoint
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This action is only used by the Amazon EC2 Container Service agent, and
-- it is not intended for use outside of the agent.
--
-- Returns an endpoint for the Amazon EC2 Container Service agent to poll
-- for updates.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DiscoverPollEndpoint.html>
module Network.AWS.ECS.DiscoverPollEndpoint
    (
    -- * Request
      DiscoverPollEndpoint
    -- ** Request constructor
    , discoverPollEndpoint
    -- ** Request lenses
    , dpeCluster
    , dpeContainerInstance

    -- * Response
    , DiscoverPollEndpointResponse
    -- ** Response constructor
    , discoverPollEndpointResponse
    -- ** Response lenses
    , dpersTelemetryEndpoint
    , dpersEndpoint
    , dpersStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'discoverPollEndpoint' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dpeCluster'
--
-- * 'dpeContainerInstance'
data DiscoverPollEndpoint = DiscoverPollEndpoint'
    { _dpeCluster           :: !(Maybe Text)
    , _dpeContainerInstance :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DiscoverPollEndpoint' smart constructor.
discoverPollEndpoint :: DiscoverPollEndpoint
discoverPollEndpoint =
    DiscoverPollEndpoint'
    { _dpeCluster = Nothing
    , _dpeContainerInstance = Nothing
    }

-- | The cluster that the container instance belongs to.
dpeCluster :: Lens' DiscoverPollEndpoint (Maybe Text)
dpeCluster = lens _dpeCluster (\ s a -> s{_dpeCluster = a});

-- | The container instance UUID or full Amazon Resource Name (ARN) of the
-- container instance. The ARN contains the @arn:aws:ecs@ namespace,
-- followed by the region of the container instance, the AWS account ID of
-- the container instance owner, the @container-instance@ namespace, and
-- then the container instance UUID. For example,
-- arn:aws:ecs:/region/:/aws_account_id/:container-instance\//container_instance_UUID/.
dpeContainerInstance :: Lens' DiscoverPollEndpoint (Maybe Text)
dpeContainerInstance = lens _dpeContainerInstance (\ s a -> s{_dpeContainerInstance = a});

instance AWSRequest DiscoverPollEndpoint where
        type Sv DiscoverPollEndpoint = ECS
        type Rs DiscoverPollEndpoint =
             DiscoverPollEndpointResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DiscoverPollEndpointResponse' <$>
                   (x .?> "telemetryEndpoint") <*> (x .?> "endpoint")
                     <*> (pure (fromEnum s)))

instance ToHeaders DiscoverPollEndpoint where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerServiceV20141113.DiscoverPollEndpoint"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DiscoverPollEndpoint where
        toJSON DiscoverPollEndpoint'{..}
          = object
              ["cluster" .= _dpeCluster,
               "containerInstance" .= _dpeContainerInstance]

instance ToPath DiscoverPollEndpoint where
        toPath = const "/"

instance ToQuery DiscoverPollEndpoint where
        toQuery = const mempty

-- | /See:/ 'discoverPollEndpointResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dpersTelemetryEndpoint'
--
-- * 'dpersEndpoint'
--
-- * 'dpersStatus'
data DiscoverPollEndpointResponse = DiscoverPollEndpointResponse'
    { _dpersTelemetryEndpoint :: !(Maybe Text)
    , _dpersEndpoint          :: !(Maybe Text)
    , _dpersStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DiscoverPollEndpointResponse' smart constructor.
discoverPollEndpointResponse :: Int -> DiscoverPollEndpointResponse
discoverPollEndpointResponse pStatus_ =
    DiscoverPollEndpointResponse'
    { _dpersTelemetryEndpoint = Nothing
    , _dpersEndpoint = Nothing
    , _dpersStatus = pStatus_
    }

-- | The telemetry endpoint for the Amazon ECS agent.
dpersTelemetryEndpoint :: Lens' DiscoverPollEndpointResponse (Maybe Text)
dpersTelemetryEndpoint = lens _dpersTelemetryEndpoint (\ s a -> s{_dpersTelemetryEndpoint = a});

-- | The endpoint for the Amazon ECS agent to poll.
dpersEndpoint :: Lens' DiscoverPollEndpointResponse (Maybe Text)
dpersEndpoint = lens _dpersEndpoint (\ s a -> s{_dpersEndpoint = a});

-- | FIXME: Undocumented member.
dpersStatus :: Lens' DiscoverPollEndpointResponse Int
dpersStatus = lens _dpersStatus (\ s a -> s{_dpersStatus = a});
