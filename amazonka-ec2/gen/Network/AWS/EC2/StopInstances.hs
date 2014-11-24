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
-- instances, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-lifecycle.html
-- Instance Lifecycle> in the /Amazon Elastic Compute Cloud User Guide/. For
-- more information about troubleshooting, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/TroubleshootingInstancesStopping.html
-- Troubleshooting Stopping Your Instance> in the /Amazon Elastic Compute
-- Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-StopInstances.html>
module Network.AWS.EC2.StopInstances
    (
    -- * Request
      StopInstances
    -- ** Request constructor
    , stopInstances
    -- ** Request lenses
    , siDryRun
    , siForce
    , siInstanceIds

    -- * Response
    , StopInstancesResponse
    -- ** Response constructor
    , stopInstancesResponse
    -- ** Response lenses
    , sirStoppingInstances
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data StopInstances = StopInstances
    { _siDryRun      :: Maybe Bool
    , _siForce       :: Maybe Bool
    , _siInstanceIds :: List "InstanceId" Text
    } deriving (Eq, Ord, Show)

-- | 'StopInstances' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'siDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'siForce' @::@ 'Maybe' 'Bool'
--
-- * 'siInstanceIds' @::@ ['Text']
--
stopInstances :: StopInstances
stopInstances = StopInstances
    { _siDryRun      = Nothing
    , _siInstanceIds = mempty
    , _siForce       = Nothing
    }

siDryRun :: Lens' StopInstances (Maybe Bool)
siDryRun = lens _siDryRun (\s a -> s { _siDryRun = a })

-- | Forces the instances to stop. The instances do not have an opportunity to
-- flush file system caches or file system metadata. If you use this option,
-- you must perform file system check and repair procedures. This option is
-- not recommended for Windows instances. Default: 'false'.
siForce :: Lens' StopInstances (Maybe Bool)
siForce = lens _siForce (\s a -> s { _siForce = a })

-- | One or more instance IDs.
siInstanceIds :: Lens' StopInstances [Text]
siInstanceIds = lens _siInstanceIds (\s a -> s { _siInstanceIds = a }) . _List

newtype StopInstancesResponse = StopInstancesResponse
    { _sirStoppingInstances :: List "item" InstanceStateChange
    } deriving (Eq, Show, Monoid, Semigroup)

instance GHC.Exts.IsList StopInstancesResponse where
    type Item StopInstancesResponse = InstanceStateChange

    fromList = StopInstancesResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _sirStoppingInstances

-- | 'StopInstancesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sirStoppingInstances' @::@ ['InstanceStateChange']
--
stopInstancesResponse :: StopInstancesResponse
stopInstancesResponse = StopInstancesResponse
    { _sirStoppingInstances = mempty
    }

-- | Information about one or more stopped instances.
sirStoppingInstances :: Lens' StopInstancesResponse [InstanceStateChange]
sirStoppingInstances =
    lens _sirStoppingInstances (\s a -> s { _sirStoppingInstances = a })
        . _List

instance ToPath StopInstances where
    toPath = const "/"

instance ToQuery StopInstances where
    toQuery StopInstances{..} = mconcat
        [ "dryRun"     =? _siDryRun
        , "force"      =? _siForce
        , "InstanceId" =? _siInstanceIds
        ]

instance ToHeaders StopInstances

instance AWSRequest StopInstances where
    type Sv StopInstances = EC2
    type Rs StopInstances = StopInstancesResponse

    request  = post "StopInstances"
    response = xmlResponse

instance FromXML StopInstancesResponse where
    parseXML x = StopInstancesResponse
        <$> x .@  "instancesSet"
