{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.V2014_06_15.RebootInstances
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
-- Instances in the Amazon Elastic Compute Cloud User Guide. Example This
-- example reboots two instances.
-- https://ec2.amazonaws.com/?Action=RebootInstances
-- &amp;InstanceId.1=i-1a2b3c4d &amp;InstanceId.2=i-4d3acf62 &amp;AUTHPARAMS
-- &lt;RebootInstancesResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/RebootInstancesResponse&gt;.
module Network.AWS.EC2.V2014_06_15.RebootInstances where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'RebootInstances' request.
rebootInstances :: [Text] -- ^ '_riwInstanceIds'
                -> RebootInstances
rebootInstances p1 = RebootInstances
    { _riwInstanceIds = p1
    , _riwDryRun = Nothing
    }

data RebootInstances = RebootInstances
    { _riwInstanceIds :: [Text]
      -- ^ One or more instance IDs.
    , _riwDryRun :: Maybe Bool
      -- ^ 
    } deriving (Show, Generic)

makeLenses ''RebootInstances

instance ToQuery RebootInstances where
    toQuery = genericToQuery def

data RebootInstancesResponse = RebootInstancesResponse
    deriving (Eq, Show, Generic)

makeLenses ''RebootInstancesResponse

instance AWSRequest RebootInstances where
    type Sv RebootInstances = EC2
    type Rs RebootInstances = RebootInstancesResponse

    request = post "RebootInstances"
    response _ _ = return (Right RebootInstancesResponse)
