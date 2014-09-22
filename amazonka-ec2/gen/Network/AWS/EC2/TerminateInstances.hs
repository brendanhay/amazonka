{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.TerminateInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Shuts down one or more instances. This operation is idempotent; if you
-- terminate an instance more than once, each call succeeds. Terminated
-- instances remain visible after termination (for approximately one hour). By
-- default, Amazon EC2 deletes all Amazon EBS volumes that were attached when
-- the instance launched. Volumes attached after instance launch continue
-- running. You can stop, start, and terminate EBS-backed instances. You can
-- only terminate instance store-backed instances. What happens to an instance
-- differs if you stop it or terminate it. For example, when you stop an
-- instance, the root device and any other devices attached to the instance
-- persist. When you terminate an instance, the root device and any other
-- devices attached during the instance launch are automatically deleted. For
-- more information about the differences between stopping and terminating
-- instances, see Instance Lifecycle in the Amazon Elastic Compute Cloud User
-- Guide. For more information about troubleshooting, see Troubleshooting
-- Terminating Your Instance in the Amazon Elastic Compute Cloud User Guide.
-- Example This example terminates the specified instance.
-- https://ec2.amazonaws.com/?Action=TerminateInstances
-- &amp;InstanceId.1=i-3ea74257 &amp;AUTHPARAMS &lt;TerminateInstancesResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;instancesSet&gt; &lt;item&gt;
-- &lt;instanceId&gt;i-3ea74257&lt;/instanceId&gt; &lt;currentState&gt;
-- &lt;code&gt;32&lt;/code&gt; &lt;name&gt;shutting-down&lt;/name&gt;
-- &lt;/currentState&gt; &lt;previousState&gt; &lt;code&gt;16&lt;/code&gt;
-- &lt;name&gt;running&lt;/name&gt; &lt;/previousState&gt; &lt;/item&gt;
-- &lt;/instancesSet&gt; &lt;/TerminateInstancesResponse&gt;.
module Network.AWS.EC2.TerminateInstances
    (
    -- * Request
      TerminateInstances
    -- ** Request constructor
    , terminateInstances
    -- ** Request lenses
    , tiInstanceId

    -- * Response
    , TerminateInstancesResponse
    -- ** Response constructor
    , terminateInstancesResponse
    -- ** Response lenses
    , tirItem
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

newtype TerminateInstances = TerminateInstances
    { _tiInstanceId :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'TerminateInstances' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InstanceId ::@ @[Text]@
--
terminateInstances :: [Text] -- ^ 'tiInstanceId'
                   -> TerminateInstances
terminateInstances p1 = TerminateInstances
    { _tiInstanceId = p1
    }

-- | One or more instance IDs.
tiInstanceId :: Lens' TerminateInstances [Text]
tiInstanceId = lens _tiInstanceId (\s a -> s { _tiInstanceId = a })

instance ToQuery TerminateInstances where
    toQuery = genericQuery def

newtype TerminateInstancesResponse = TerminateInstancesResponse
    { _tirItem :: [InstanceStateChange]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'TerminateInstancesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Item ::@ @[InstanceStateChange]@
--
terminateInstancesResponse :: TerminateInstancesResponse
terminateInstancesResponse = TerminateInstancesResponse
    { _tirItem = mempty
    }

-- | Information about one or more terminated instances.
tirItem :: Lens' TerminateInstancesResponse [InstanceStateChange]
tirItem = lens _tirItem (\s a -> s { _tirItem = a })

instance FromXML TerminateInstancesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest TerminateInstances where
    type Sv TerminateInstances = EC2
    type Rs TerminateInstances = TerminateInstancesResponse

    request = post "TerminateInstances"
    response _ = xmlResponse
