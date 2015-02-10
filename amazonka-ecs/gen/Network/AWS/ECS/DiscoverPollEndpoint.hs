{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ECS.DiscoverPollEndpoint
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This action is only used by the Amazon EC2 Container Service agent, and it is
-- not intended for use outside of the agent.
--
-- Returns an endpoint for the Amazon EC2 Container Service agent to poll for
-- updates.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DiscoverPollEndpoint.html>
module Network.AWS.ECS.DiscoverPollEndpoint
    (
    -- * Request
      DiscoverPollEndpoint
    -- ** Request constructor
    , discoverPollEndpoint
    -- ** Request lenses
    , dpeContainerInstance

    -- * Response
    , DiscoverPollEndpointResponse
    -- ** Response constructor
    , discoverPollEndpointResponse
    -- ** Response lenses
    , dperEndpoint
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ECS.Types
import qualified GHC.Exts

newtype DiscoverPollEndpoint = DiscoverPollEndpoint
    { _dpeContainerInstance :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'DiscoverPollEndpoint' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dpeContainerInstance' @::@ 'Maybe' 'Text'
--
discoverPollEndpoint :: DiscoverPollEndpoint
discoverPollEndpoint = DiscoverPollEndpoint
    { _dpeContainerInstance = Nothing
    }

-- | The container instance UUID or full Amazon Resource Name (ARN) of the
-- container instance. The ARN contains the 'arn:aws:ecs' namespace, followed by
-- the region of the container instance, the AWS account ID of the container
-- instance owner, the 'container-instance' namespace, and then the container
-- instance UUID. For example, arn:aws:ecs:/region/:/aws_account_id/:container-instance/
-- /container_instance_UUID/.
dpeContainerInstance :: Lens' DiscoverPollEndpoint (Maybe Text)
dpeContainerInstance =
    lens _dpeContainerInstance (\s a -> s { _dpeContainerInstance = a })

newtype DiscoverPollEndpointResponse = DiscoverPollEndpointResponse
    { _dperEndpoint :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'DiscoverPollEndpointResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dperEndpoint' @::@ 'Maybe' 'Text'
--
discoverPollEndpointResponse :: DiscoverPollEndpointResponse
discoverPollEndpointResponse = DiscoverPollEndpointResponse
    { _dperEndpoint = Nothing
    }

-- | The endpoint for the Amazon ECS agent to poll.
dperEndpoint :: Lens' DiscoverPollEndpointResponse (Maybe Text)
dperEndpoint = lens _dperEndpoint (\s a -> s { _dperEndpoint = a })

instance ToPath DiscoverPollEndpoint where
    toPath = const "/"

instance ToQuery DiscoverPollEndpoint where
    toQuery DiscoverPollEndpoint{..} = mconcat
        [ "containerInstance" =? _dpeContainerInstance
        ]

instance ToHeaders DiscoverPollEndpoint

instance AWSRequest DiscoverPollEndpoint where
    type Sv DiscoverPollEndpoint = ECS
    type Rs DiscoverPollEndpoint = DiscoverPollEndpointResponse

    request  = post "DiscoverPollEndpoint"
    response = xmlResponse

instance FromXML DiscoverPollEndpointResponse where
    parseXML = withElement "DiscoverPollEndpointResult" $ \x -> DiscoverPollEndpointResponse
        <$> x .@? "endpoint"
