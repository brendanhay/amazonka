{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.EC2.RebootInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Requests a reboot of one or more instances. This operation is asynchronous;
-- it only queues a request to reboot the specified instances. The operation
-- succeeds if the instances are valid and belong to you. Requests to reboot
-- terminated instances are ignored. If a Linux/Unix instance does not cleanly
-- shut down within four minutes, Amazon EC2 performs a hard reboot. For more
-- information about troubleshooting, see Getting Console Output and Rebooting
-- Instances in the Amazon Elastic Compute Cloud User Guide.
module Network.AWS.EC2.RebootInstances
    (
    -- * Request
      RebootInstances
    -- ** Request constructor
    , rebootInstances
    -- ** Request lenses
    , ri2DryRun
    , ri2InstanceIds

    -- * Response
    , RebootInstancesResponse
    -- ** Response constructor
    , rebootInstancesResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data RebootInstances = RebootInstances
    { _ri2DryRun      :: Maybe Bool
    , _ri2InstanceIds :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'RebootInstances' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ri2DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'ri2InstanceIds' @::@ ['Text']
--
rebootInstances :: RebootInstances
rebootInstances = RebootInstances
    { _ri2DryRun      = Nothing
    , _ri2InstanceIds = mempty
    }

ri2DryRun :: Lens' RebootInstances (Maybe Bool)
ri2DryRun = lens _ri2DryRun (\s a -> s { _ri2DryRun = a })

-- | One or more instance IDs.
ri2InstanceIds :: Lens' RebootInstances [Text]
ri2InstanceIds = lens _ri2InstanceIds (\s a -> s { _ri2InstanceIds = a })
instance ToQuery RebootInstances

instance ToPath RebootInstances where
    toPath = const "/"

data RebootInstancesResponse = RebootInstancesResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'RebootInstancesResponse' constructor.
rebootInstancesResponse :: RebootInstancesResponse
rebootInstancesResponse = RebootInstancesResponse
instance FromXML RebootInstancesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RebootInstancesResponse"

instance AWSRequest RebootInstances where
    type Sv RebootInstances = EC2
    type Rs RebootInstances = RebootInstancesResponse

    request  = post "RebootInstances"
    response = nullaryResponse RebootInstancesResponse
