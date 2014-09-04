{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.StopInstances
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
-- Example This example stops the specified instance.
-- https://ec2.amazonaws.com/?Action=StopInstances
-- &amp;InstanceId.1=i-10a64379 &amp;AUTHPARAMS &lt;StopInstancesResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;instancesSet&gt; &lt;item&gt;
-- &lt;instanceId&gt;i-10a64379&lt;/instanceId&gt; &lt;currentState&gt;
-- &lt;code&gt;64&lt;/code&gt; &lt;name&gt;stopping&lt;/name&gt;
-- &lt;/currentState&gt; &lt;previousState&gt; &lt;code&gt;16&lt;/code&gt;
-- &lt;name&gt;running&lt;/name&gt; &lt;/previousState&gt;
-- &lt;/instancesSet&gt; &lt;/StopInstancesResponse&gt;.
module Network.AWS.EC2.V2014_06_15.StopInstances
    (
    -- * Request
      StopInstances
    -- ** Request constructor
    , stopInstances
    -- ** Request lenses
    , sisvInstanceIds
    , sisvForce

    -- * Response
    , StopInstancesResponse
    -- ** Response lenses
    , siswStoppingInstances
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'StopInstances' request.
stopInstances :: [Text] -- ^ 'sisvInstanceIds'
              -> StopInstances
stopInstances p1 = StopInstances
    { _sisvInstanceIds = p1
    , _sisvForce = Nothing
    }
{-# INLINE stopInstances #-}

data StopInstances = StopInstances
    { _sisvInstanceIds :: [Text]
      -- ^ One or more instance IDs.
    , _sisvForce :: Maybe Bool
      -- ^ Forces the instances to stop. The instances do not have an
      -- opportunity to flush file system caches or file system metadata.
      -- If you use this option, you must perform file system check and
      -- repair procedures. This option is not recommended for Windows
      -- instances. Default: false.
    } deriving (Show, Generic)

-- | One or more instance IDs.
sisvInstanceIds :: Lens' StopInstances [Text]
sisvInstanceIds f x =
    f (_sisvInstanceIds x) <&> \y -> x { _sisvInstanceIds = y }
{-# INLINE sisvInstanceIds #-}

-- | Forces the instances to stop. The instances do not have an opportunity to
-- flush file system caches or file system metadata. If you use this option,
-- you must perform file system check and repair procedures. This option is
-- not recommended for Windows instances. Default: false.
sisvForce :: Lens' StopInstances (Maybe Bool)
sisvForce f x =
    f (_sisvForce x) <&> \y -> x { _sisvForce = y }
{-# INLINE sisvForce #-}

instance ToQuery StopInstances where
    toQuery = genericQuery def

data StopInstancesResponse = StopInstancesResponse
    { _siswStoppingInstances :: [InstanceStateChange]
      -- ^ Information about one or more stopped instances.
    } deriving (Show, Generic)

-- | Information about one or more stopped instances.
siswStoppingInstances :: Lens' StopInstancesResponse [InstanceStateChange]
siswStoppingInstances f x =
    f (_siswStoppingInstances x) <&> \y -> x { _siswStoppingInstances = y }
{-# INLINE siswStoppingInstances #-}

instance FromXML StopInstancesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest StopInstances where
    type Sv StopInstances = EC2
    type Rs StopInstances = StopInstancesResponse

    request = post "StopInstances"
    response _ = xmlResponse
