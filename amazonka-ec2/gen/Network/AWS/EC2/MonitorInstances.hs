{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.MonitorInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Enables monitoring for a running instance. For more information about
-- monitoring instances, see Monitoring Your Instances and Volumes in the
-- Amazon Elastic Compute Cloud User Guide. Example This example enables
-- monitoring for two instances.
-- https://ec2.amazonaws.com/?Action=MonitorInstances
-- &amp;InstanceId.1=i-43a4412a &amp;InstanceId.2=i-23a3397d &amp;AUTHPARAMS
-- &lt;MonitorInstancesResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;instancesSet&gt; &lt;item&gt;
-- &lt;instanceId&gt;i-43a4412a&lt;/instanceId&gt; &lt;monitoring&gt;
-- &lt;state&gt;pending&lt;/state&gt; &lt;/monitoring&gt; &lt;/item&gt;
-- &lt;item&gt; &lt;instanceId&gt;i-23a3397d&lt;/instanceId&gt;
-- &lt;monitoring&gt; &lt;state&gt;pending&lt;/state&gt; &lt;/monitoring&gt;
-- &lt;/item&gt; &lt;/instancesSet&gt; &lt;/MonitorInstancesResponse&gt;.
module Network.AWS.EC2.MonitorInstances
    (
    -- * Request
      MonitorInstances
    -- ** Request constructor
    , monitorInstances
    -- ** Request lenses
    , miInstanceId

    -- * Response
    , MonitorInstancesResponse
    -- ** Response constructor
    , monitorInstancesResponse
    -- ** Response lenses
    , mirItem
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

newtype MonitorInstances = MonitorInstances
    { _miInstanceId :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'MonitorInstances' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InstanceId ::@ @[Text]@
--
monitorInstances :: [Text] -- ^ 'miInstanceId'
                 -> MonitorInstances
monitorInstances p1 = MonitorInstances
    { _miInstanceId = p1
    }

-- | One or more instance IDs.
miInstanceId :: Lens' MonitorInstances [Text]
miInstanceId = lens _miInstanceId (\s a -> s { _miInstanceId = a })

instance ToQuery MonitorInstances where
    toQuery = genericQuery def

newtype MonitorInstancesResponse = MonitorInstancesResponse
    { _mirItem :: [InstanceMonitoring]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'MonitorInstancesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Item ::@ @[InstanceMonitoring]@
--
monitorInstancesResponse :: MonitorInstancesResponse
monitorInstancesResponse = MonitorInstancesResponse
    { _mirItem = mempty
    }

-- | Monitoring information for one or more instances.
mirItem :: Lens' MonitorInstancesResponse [InstanceMonitoring]
mirItem = lens _mirItem (\s a -> s { _mirItem = a })

instance FromXML MonitorInstancesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest MonitorInstances where
    type Sv MonitorInstances = EC2
    type Rs MonitorInstances = MonitorInstancesResponse

    request = post "MonitorInstances"
    response _ = xmlResponse
