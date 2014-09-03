{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.MonitorInstances
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
module Network.AWS.EC2.V2014_06_15.MonitorInstances
    (
    -- * Request
      MonitorInstances
    -- ** Default constructor
    , monitorInstances
    -- ** Accessors and lenses
    , _mirInstanceIds
    , mirInstanceIds

    -- * Response
    , MonitorInstancesResponse
    -- ** Accessors and lenses
    , _misInstanceMonitorings
    , misInstanceMonitorings
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'MonitorInstances' request.
monitorInstances :: [Text] -- ^ 'mirInstanceIds'
                 -> MonitorInstances
monitorInstances p1 = MonitorInstances
    { _mirInstanceIds = p1
    }

data MonitorInstances = MonitorInstances

makeSiglessLenses ''MonitorInstances

instance ToQuery MonitorInstances where
    toQuery = genericQuery def

data MonitorInstancesResponse = MonitorInstancesResponse
    { _misInstanceMonitorings :: [InstanceMonitoring]
      -- ^ Monitoring information for one or more instances.
    } deriving (Show, Generic)

makeSiglessLenses ''MonitorInstancesResponse

instance FromXML MonitorInstancesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest MonitorInstances where
    type Sv MonitorInstances = EC2
    type Rs MonitorInstances = MonitorInstancesResponse

    request = post "MonitorInstances"
    response _ = xmlResponse

-- | One or more instance IDs.
mirInstanceIds :: Lens' MonitorInstances ([Text])

-- | Monitoring information for one or more instances.
misInstanceMonitorings :: Lens' MonitorInstancesResponse ([InstanceMonitoring])
