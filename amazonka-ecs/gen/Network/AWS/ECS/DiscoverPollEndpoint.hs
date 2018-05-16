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
-- Module      : Network.AWS.ECS.DiscoverPollEndpoint
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an endpoint for the Amazon ECS agent to poll for updates.
--
--
module Network.AWS.ECS.DiscoverPollEndpoint
    (
    -- * Creating a Request
      discoverPollEndpoint
    , DiscoverPollEndpoint
    -- * Request Lenses
    , dpeCluster
    , dpeContainerInstance

    -- * Destructuring the Response
    , discoverPollEndpointResponse
    , DiscoverPollEndpointResponse
    -- * Response Lenses
    , dpersTelemetryEndpoint
    , dpersEndpoint
    , dpersResponseStatus
    ) where

import Network.AWS.ECS.Types
import Network.AWS.ECS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'discoverPollEndpoint' smart constructor.
data DiscoverPollEndpoint = DiscoverPollEndpoint'
  { _dpeCluster           :: !(Maybe Text)
  , _dpeContainerInstance :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DiscoverPollEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpeCluster' - The short name or full Amazon Resource Name (ARN) of the cluster that the container instance belongs to.
--
-- * 'dpeContainerInstance' - The container instance ID or full ARN of the container instance. The ARN contains the @arn:aws:ecs@ namespace, followed by the region of the container instance, the AWS account ID of the container instance owner, the @container-instance@ namespace, and then the container instance ID. For example, @arn:aws:ecs:/region/ :/aws_account_id/ :container-instance//container_instance_ID/ @ .
discoverPollEndpoint
    :: DiscoverPollEndpoint
discoverPollEndpoint =
  DiscoverPollEndpoint' {_dpeCluster = Nothing, _dpeContainerInstance = Nothing}


-- | The short name or full Amazon Resource Name (ARN) of the cluster that the container instance belongs to.
dpeCluster :: Lens' DiscoverPollEndpoint (Maybe Text)
dpeCluster = lens _dpeCluster (\ s a -> s{_dpeCluster = a})

-- | The container instance ID or full ARN of the container instance. The ARN contains the @arn:aws:ecs@ namespace, followed by the region of the container instance, the AWS account ID of the container instance owner, the @container-instance@ namespace, and then the container instance ID. For example, @arn:aws:ecs:/region/ :/aws_account_id/ :container-instance//container_instance_ID/ @ .
dpeContainerInstance :: Lens' DiscoverPollEndpoint (Maybe Text)
dpeContainerInstance = lens _dpeContainerInstance (\ s a -> s{_dpeContainerInstance = a})

instance AWSRequest DiscoverPollEndpoint where
        type Rs DiscoverPollEndpoint =
             DiscoverPollEndpointResponse
        request = postJSON ecs
        response
          = receiveJSON
              (\ s h x ->
                 DiscoverPollEndpointResponse' <$>
                   (x .?> "telemetryEndpoint") <*> (x .?> "endpoint")
                     <*> (pure (fromEnum s)))

instance Hashable DiscoverPollEndpoint where

instance NFData DiscoverPollEndpoint where

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
              (catMaybes
                 [("cluster" .=) <$> _dpeCluster,
                  ("containerInstance" .=) <$> _dpeContainerInstance])

instance ToPath DiscoverPollEndpoint where
        toPath = const "/"

instance ToQuery DiscoverPollEndpoint where
        toQuery = const mempty

-- | /See:/ 'discoverPollEndpointResponse' smart constructor.
data DiscoverPollEndpointResponse = DiscoverPollEndpointResponse'
  { _dpersTelemetryEndpoint :: !(Maybe Text)
  , _dpersEndpoint          :: !(Maybe Text)
  , _dpersResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DiscoverPollEndpointResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpersTelemetryEndpoint' - The telemetry endpoint for the Amazon ECS agent.
--
-- * 'dpersEndpoint' - The endpoint for the Amazon ECS agent to poll.
--
-- * 'dpersResponseStatus' - -- | The response status code.
discoverPollEndpointResponse
    :: Int -- ^ 'dpersResponseStatus'
    -> DiscoverPollEndpointResponse
discoverPollEndpointResponse pResponseStatus_ =
  DiscoverPollEndpointResponse'
    { _dpersTelemetryEndpoint = Nothing
    , _dpersEndpoint = Nothing
    , _dpersResponseStatus = pResponseStatus_
    }


-- | The telemetry endpoint for the Amazon ECS agent.
dpersTelemetryEndpoint :: Lens' DiscoverPollEndpointResponse (Maybe Text)
dpersTelemetryEndpoint = lens _dpersTelemetryEndpoint (\ s a -> s{_dpersTelemetryEndpoint = a})

-- | The endpoint for the Amazon ECS agent to poll.
dpersEndpoint :: Lens' DiscoverPollEndpointResponse (Maybe Text)
dpersEndpoint = lens _dpersEndpoint (\ s a -> s{_dpersEndpoint = a})

-- | -- | The response status code.
dpersResponseStatus :: Lens' DiscoverPollEndpointResponse Int
dpersResponseStatus = lens _dpersResponseStatus (\ s a -> s{_dpersResponseStatus = a})

instance NFData DiscoverPollEndpointResponse where
