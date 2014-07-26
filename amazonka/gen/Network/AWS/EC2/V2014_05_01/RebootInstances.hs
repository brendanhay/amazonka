{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_05_01.RebootInstances
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
module Network.AWS.EC2.V2014_05_01.RebootInstances where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Maybe
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error)
import           Network.AWS.Request.Query
import           Network.AWS.EC2.V2014_05_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data RebootInstances = RebootInstances
    { _riuInstanceIds :: [Text]
      -- ^ One or more instance IDs.
    , _riuDryRun :: Bool
      -- ^ 
    } deriving (Generic)

instance ToQuery RebootInstances where
    toQuery = genericToQuery def

instance AWSRequest RebootInstances where
    type Sv RebootInstances = EC2
    type Rs RebootInstances = RebootInstancesResponse

    request = post "RebootInstances"

    response _ _ = return (Right RebootInstancesResponse)

data RebootInstancesResponse = RebootInstancesResponse
    deriving (Eq, Show, Generic)
