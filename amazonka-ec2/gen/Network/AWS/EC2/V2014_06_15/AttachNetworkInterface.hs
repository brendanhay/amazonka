{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
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
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;ace8cd1e-e685-4e44-90fb-92014d907212&lt;/requestId&gt;
-- &lt;attachmentId&gt;eni-attach-d94b09b0&lt;/attachmentId&gt;
-- &lt;/AttachNetworkInterfaceResponse&gt;.
module Network.AWS.EC2.V2014_06_15.AttachNetworkInterface where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

data AttachNetworkInterface = AttachNetworkInterface
    { _anirDeviceIndex :: Integer
      -- ^ The index of the device for the network interface attachment.
    , _anirNetworkInterfaceId :: Text
      -- ^ The ID of the network interface.
    , _anirInstanceId :: Text
      -- ^ The ID of the instance.
    } deriving (Show, Generic)

makeLenses ''AttachNetworkInterface

instance ToQuery AttachNetworkInterface where
    toQuery = genericQuery def

data AttachNetworkInterfaceResponse = AttachNetworkInterfaceResponse
    { _anisAttachmentId :: Maybe Text
      -- ^ The ID of the network interface attachment.
    } deriving (Show, Generic)

makeLenses ''AttachNetworkInterfaceResponse

instance FromXML AttachNetworkInterfaceResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest AttachNetworkInterface where
    type Sv AttachNetworkInterface = EC2
    type Rs AttachNetworkInterface = AttachNetworkInterfaceResponse

    request = post "AttachNetworkInterface"
    response _ = xmlResponse
