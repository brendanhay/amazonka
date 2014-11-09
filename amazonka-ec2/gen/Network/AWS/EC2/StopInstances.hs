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

-- Module      : Network.AWS.EC2.StopInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Stops an Amazon EBS-backed instance. Each time you transition an instance
-- from stopped to started, Amazon EC2 charges a full instance hour, even if
-- transitions happen multiple times within a single hour. You can't start or
-- stop Spot Instances. Instances that use Amazon EBS volumes as their root
-- devices can be quickly stopped and started. When an instance is stopped,
-- the compute resources are released and you are not billed for hourly
-- instance usage. However, your root partition Amazon EBS volume remains,
-- continues to persist your data, and you are charged for Amazon EBS volume
-- usage. You can restart your instance at any time. Before stopping an
-- instance, make sure it is in a state from which it can be restarted.
-- Stopping an instance does not preserve data stored in RAM. Performing this
-- operation on an instance that uses an instance store as its root device
-- returns an error. You can stop, start, and terminate EBS-backed instances.
-- You can only terminate instance store-backed instances. What happens to an
-- instance differs if you stop it or terminate it. For example, when you stop
-- an instance, the root device and any other devices attached to the instance
-- persist. When you terminate an instance, the root device and any other
-- devices attached during the instance launch are automatically deleted. For
-- more information about the differences between stopping and terminating
-- instances, see Instance Lifecycle in the Amazon Elastic Compute Cloud User
-- Guide. For more information about troubleshooting, see Troubleshooting
-- Stopping Your Instance in the Amazon Elastic Compute Cloud User Guide.
module Network.AWS.EC2.StopInstances
    (
    -- * Request
      StopInstances
    -- ** Request constructor
    , stopInstances
    -- ** Request lenses
    , si1DryRun
    , si1Force
    , si1InstanceIds

    -- * Response
    , StopInstancesResult
    -- ** Response constructor
    , stopInstancesResult
    -- ** Response lenses
    , sirStoppingInstances
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data StopInstances = StopInstances
    { _si1DryRun      :: Maybe Bool
    , _si1Force       :: Maybe Bool
    , _si1InstanceIds :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'StopInstances' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'si1DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'si1Force' @::@ 'Maybe' 'Bool'
--
-- * 'si1InstanceIds' @::@ ['Text']
--
stopInstances :: StopInstances
stopInstances = StopInstances
    { _si1DryRun      = Nothing
    , _si1InstanceIds = mempty
    , _si1Force       = Nothing
    }

si1DryRun :: Lens' StopInstances (Maybe Bool)
si1DryRun = lens _si1DryRun (\s a -> s { _si1DryRun = a })

-- | Forces the instances to stop. The instances do not have an opportunity to
-- flush file system caches or file system metadata. If you use this option,
-- you must perform file system check and repair procedures. This option is
-- not recommended for Windows instances. Default: false.
si1Force :: Lens' StopInstances (Maybe Bool)
si1Force = lens _si1Force (\s a -> s { _si1Force = a })

-- | One or more instance IDs.
si1InstanceIds :: Lens' StopInstances [Text]
si1InstanceIds = lens _si1InstanceIds (\s a -> s { _si1InstanceIds = a })

instance ToPath StopInstances where
    toPath = const "/"

instance ToQuery StopInstances

newtype StopInstancesResult = StopInstancesResult
    { _sirStoppingInstances :: [InstanceStateChange]
    } deriving (Eq, Show, Generic, Monoid)

-- | 'StopInstancesResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sirStoppingInstances' @::@ ['InstanceStateChange']
--
stopInstancesResult :: StopInstancesResult
stopInstancesResult = StopInstancesResult
    { _sirStoppingInstances = mempty
    }

-- | Information about one or more stopped instances.
sirStoppingInstances :: Lens' StopInstancesResult [InstanceStateChange]
sirStoppingInstances =
    lens _sirStoppingInstances (\s a -> s { _sirStoppingInstances = a })

instance AWSRequest StopInstances where
    type Sv StopInstances = EC2
    type Rs StopInstances = StopInstancesResult

    request  = post "StopInstances"
    response = const . xmlResponse $ \h x -> StopInstancesResult
        <$> x %| "instancesSet"
