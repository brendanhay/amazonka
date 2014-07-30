{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.AttachNetworkInterface
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Attaches a network interface to an instance. Example This example attaches
-- the specified network interface to the specified instance.
-- https://ec2.amazonaws.com/?Action=AttachNetworkInterface &amp;DeviceIndex=1
-- &amp;InstanceId=i-9cc316fe &amp;NetworkInterfaceId=eni-ffda3197
-- &amp;AUTHPARAMS &lt;AttachNetworkInterfaceResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;ace8cd1e-e685-4e44-90fb-92014d907212&lt;/requestId&gt;
-- &lt;attachmentId&gt;eni-attach-d94b09b0&lt;/attachmentId&gt;
-- &lt;/AttachNetworkInterfaceResponse&gt;.
module Network.AWS.EC2.V2014_06_15.AttachNetworkInterface where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.EC2.V2014_06_15.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

-- | Minimum specification for a 'AttachNetworkInterface' request.
attachNetworkInterface :: Integer -- ^ '_anirDeviceIndex'
                       -> Text -- ^ '_anirInstanceId'
                       -> Text -- ^ '_anirNetworkInterfaceId'
                       -> AttachNetworkInterface
attachNetworkInterface p1 p2 p3 = AttachNetworkInterface
    { _anirDeviceIndex = p1
    , _anirInstanceId = p2
    , _anirNetworkInterfaceId = p3
    , _anirDryRun = Nothing
    }

data AttachNetworkInterface = AttachNetworkInterface
    { _anirDeviceIndex :: Integer
      -- ^ The index of the device for the network interface attachment.
    , _anirInstanceId :: Text
      -- ^ The ID of the instance.
    , _anirNetworkInterfaceId :: Text
      -- ^ The ID of the network interface.
    , _anirDryRun :: Maybe Bool
      -- ^ 
    } deriving (Generic)

instance ToQuery AttachNetworkInterface where
    toQuery = genericToQuery def

instance AWSRequest AttachNetworkInterface where
    type Sv AttachNetworkInterface = EC2
    type Rs AttachNetworkInterface = AttachNetworkInterfaceResponse

    request = post "AttachNetworkInterface"
    response _ = xmlResponse

data AttachNetworkInterfaceResponse = AttachNetworkInterfaceResponse
    { _anisAttachmentId :: Maybe Text
      -- ^ The ID of the network interface attachment.
    } deriving (Generic)

instance FromXML AttachNetworkInterfaceResponse where
    fromXMLOptions = xmlOptions
