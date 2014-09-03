{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.GetConsoleOutput
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets the console output for the specified instance. Instances do not have a
-- physical monitor through which you can view their console output. They also
-- lack physical controls that allow you to power up, reboot, or shut them
-- down. To allow these actions, we provide them through the Amazon EC2 API
-- and command line interface. Instance console output is buffered and posted
-- shortly after instance boot, reboot, and termination. Amazon EC2 preserves
-- the most recent 64 KB output which is available for at least one hour after
-- the most recent post. For Linux/Unix instances, the instance console output
-- displays the exact console output that would normally be displayed on a
-- physical monitor attached to a machine. This output is buffered because the
-- instance produces it and then posts it to a store where the instance's
-- owner can retrieve it. For Windows instances, the instance console output
-- displays the last three system event log errors. Example This example
-- retrieves the console output for the specified instance.
-- https://ec2.amazonaws.com/?Action=GetConsoleOutput
-- &amp;InstanceId=i-10a64379 &amp;AUTHPARAMS &lt;GetConsoleOutputResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;instanceId&gt;i-28a64341&lt;/instanceId&gt;
-- &lt;timestamp&gt;2010-10-14T01:12:41.000Z&lt;/timestamp&gt;
-- &lt;output&gt;TGludXggdmVyc2lvbiAyLjYuMTYteGVuVSAoYnVpbGRlckBwYXRjaGJhdC5hbWF6b25zYSkgKGdj
-- 
-- YyB2ZXJzaW9uIDQuMC4xIDIwMDUwNzI3IChSZWQgSGF0IDQuMC4xLTUpKSAjMSBTTVAgVGh1IE9j
-- 
-- dCAyNiAwODo0MToyNiBTQVNUIDIwMDYKQklPUy1wcm92aWRlZCBwaHlzaWNhbCBSQU0gbWFwOgpY
-- 
-- ZW46IDAwMDAwMDAwMDAwMDAwMDAgLSAwMDAwMDAwMDZhNDAwMDAwICh1c2FibGUpCjk4ME1CIEhJ
-- 
-- R0hNRU0gYXZhaWxhYmxlLgo3MjdNQiBMT1dNRU0gYXZhaWxhYmxlLgpOWCAoRXhlY3V0ZSBEaXNh
-- 
-- YmxlKSBwcm90ZWN0aW9uOiBhY3RpdmUKSVJRIGxvY2t1cCBkZXRlY3Rpb24gZGlzYWJsZWQKQnVp
-- 
-- bHQgMSB6b25lbGlzdHMKS2VybmVsIGNvbW1hbmQgbGluZTogcm9vdD0vZGV2L3NkYTEgcm8gNApF
-- bmFibGluZyBmYXN0IEZQVSBzYXZlIGFuZCByZXN0b3JlLi4uIGRvbmUuCg==&lt;/output&gt;
-- &lt;/GetConsoleOutputResponse&gt;.
module Network.AWS.EC2.V2014_06_15.GetConsoleOutput where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'GetConsoleOutput' request.
getConsoleOutput :: Text -- ^ '_gcorInstanceId'
                 -> GetConsoleOutput
getConsoleOutput p1 = GetConsoleOutput
    { _gcorInstanceId = p1
    }

data GetConsoleOutput = GetConsoleOutput
    { _gcorInstanceId :: Text
      -- ^ The ID of the instance.
    } deriving (Show, Generic)

makeLenses ''GetConsoleOutput

instance ToQuery GetConsoleOutput where
    toQuery = genericQuery def

data GetConsoleOutputResponse = GetConsoleOutputResponse
    { _gcosTimestamp :: Maybe ISO8601
      -- ^ The time the output was last updated.
    , _gcosInstanceId :: Maybe Text
      -- ^ The ID of the instance.
    , _gcosOutput :: Maybe Text
      -- ^ The console output, Base64 encoded.
    } deriving (Show, Generic)

makeLenses ''GetConsoleOutputResponse

instance FromXML GetConsoleOutputResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetConsoleOutput where
    type Sv GetConsoleOutput = EC2
    type Rs GetConsoleOutput = GetConsoleOutputResponse

    request = post "GetConsoleOutput"
    response _ = xmlResponse
