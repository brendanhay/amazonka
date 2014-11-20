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

-- Module      : Network.AWS.OpsWorks.AttachElasticLoadBalancer
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Attaches an Elastic Load Balancing load balancer to a specified layer. For
-- more information, see Elastic Load Balancing. Required Permissions: To use
-- this action, an IAM user must have a Manage permissions level for the
-- stack, or an attached policy that explicitly grants permissions. For more
-- information on user permissions, see Managing User Permissions.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_AttachElasticLoadBalancer.html>
module Network.AWS.OpsWorks.AttachElasticLoadBalancer
    (
    -- * Request
      AttachElasticLoadBalancer
    -- ** Request constructor
    , attachElasticLoadBalancer
    -- ** Request lenses
    , aelbElasticLoadBalancerName
    , aelbLayerId

    -- * Response
    , AttachElasticLoadBalancerResponse
    -- ** Response constructor
    , attachElasticLoadBalancerResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

data AttachElasticLoadBalancer = AttachElasticLoadBalancer
    { _aelbElasticLoadBalancerName :: Text
    , _aelbLayerId                 :: Text
    } deriving (Eq, Ord, Show)

-- | 'AttachElasticLoadBalancer' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aelbElasticLoadBalancerName' @::@ 'Text'
--
-- * 'aelbLayerId' @::@ 'Text'
--
attachElasticLoadBalancer :: Text -- ^ 'aelbElasticLoadBalancerName'
                          -> Text -- ^ 'aelbLayerId'
                          -> AttachElasticLoadBalancer
attachElasticLoadBalancer p1 p2 = AttachElasticLoadBalancer
    { _aelbElasticLoadBalancerName = p1
    , _aelbLayerId                 = p2
    }

-- | The Elastic Load Balancing instance's name.
aelbElasticLoadBalancerName :: Lens' AttachElasticLoadBalancer Text
aelbElasticLoadBalancerName =
    lens _aelbElasticLoadBalancerName
        (\s a -> s { _aelbElasticLoadBalancerName = a })

-- | The ID of the layer that the Elastic Load Balancing instance is to be
-- attached to.
aelbLayerId :: Lens' AttachElasticLoadBalancer Text
aelbLayerId = lens _aelbLayerId (\s a -> s { _aelbLayerId = a })

data AttachElasticLoadBalancerResponse = AttachElasticLoadBalancerResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'AttachElasticLoadBalancerResponse' constructor.
attachElasticLoadBalancerResponse :: AttachElasticLoadBalancerResponse
attachElasticLoadBalancerResponse = AttachElasticLoadBalancerResponse

instance ToPath AttachElasticLoadBalancer where
    toPath = const "/"

instance ToQuery AttachElasticLoadBalancer where
    toQuery = const mempty

instance ToHeaders AttachElasticLoadBalancer

instance ToJSON AttachElasticLoadBalancer where
    toJSON AttachElasticLoadBalancer{..} = object
        [ "ElasticLoadBalancerName" .= _aelbElasticLoadBalancerName
        , "LayerId"                 .= _aelbLayerId
        ]

json

instance AWSRequest AttachElasticLoadBalancer where
    type Sv AttachElasticLoadBalancer = OpsWorks
    type Rs AttachElasticLoadBalancer = AttachElasticLoadBalancerResponse

    request  = post "AttachElasticLoadBalancer"
    response = nullResponse AttachElasticLoadBalancerResponse
