{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
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
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;ace8cd1e-e685-4e44-90fb-92014d907212&lt;/requestId&gt;
-- &lt;attachmentId&gt;eni-attach-d94b09b0&lt;/attachmentId&gt;
-- &lt;/AttachNetworkInterfaceResponse&gt;.
module Network.AWS.EC2.V2014_06_15.AttachNetworkInterface
    (
    -- * Request
      AttachNetworkInterface
    -- ** Request constructor
    , attachNetworkInterface
    -- ** Request lenses
    , anirDeviceIndex
    , anirNetworkInterfaceId
    , anirInstanceId

    -- * Response
    , AttachNetworkInterfaceResponse
    -- ** Response lenses
    , anisAttachmentId
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'AttachNetworkInterface' request.
attachNetworkInterface :: Integer -- ^ 'anirDeviceIndex'
                       -> Text -- ^ 'anirNetworkInterfaceId'
                       -> Text -- ^ 'anirInstanceId'
                       -> AttachNetworkInterface
attachNetworkInterface p1 p2 p3 = AttachNetworkInterface
    { _anirDeviceIndex = p1
    , _anirNetworkInterfaceId = p2
    , _anirInstanceId = p3
    }
{-# INLINE attachNetworkInterface #-}

data AttachNetworkInterface = AttachNetworkInterface
    { _anirDeviceIndex :: Integer
      -- ^ The index of the device for the network interface attachment.
    , _anirNetworkInterfaceId :: Text
      -- ^ The ID of the network interface.
    , _anirInstanceId :: Text
      -- ^ The ID of the instance.
    } deriving (Show, Generic)

-- | The index of the device for the network interface attachment.
anirDeviceIndex :: Lens' AttachNetworkInterface (Integer)
anirDeviceIndex f x =
    f (_anirDeviceIndex x)
        <&> \y -> x { _anirDeviceIndex = y }
{-# INLINE anirDeviceIndex #-}

-- | The ID of the network interface.
anirNetworkInterfaceId :: Lens' AttachNetworkInterface (Text)
anirNetworkInterfaceId f x =
    f (_anirNetworkInterfaceId x)
        <&> \y -> x { _anirNetworkInterfaceId = y }
{-# INLINE anirNetworkInterfaceId #-}

-- | The ID of the instance.
anirInstanceId :: Lens' AttachNetworkInterface (Text)
anirInstanceId f x =
    f (_anirInstanceId x)
        <&> \y -> x { _anirInstanceId = y }
{-# INLINE anirInstanceId #-}

instance ToQuery AttachNetworkInterface where
    toQuery = genericQuery def

data AttachNetworkInterfaceResponse = AttachNetworkInterfaceResponse
    { _anisAttachmentId :: Maybe Text
      -- ^ The ID of the network interface attachment.
    } deriving (Show, Generic)

-- | The ID of the network interface attachment.
anisAttachmentId :: Lens' AttachNetworkInterfaceResponse (Maybe Text)
anisAttachmentId f x =
    f (_anisAttachmentId x)
        <&> \y -> x { _anisAttachmentId = y }
{-# INLINE anisAttachmentId #-}

instance FromXML AttachNetworkInterfaceResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest AttachNetworkInterface where
    type Sv AttachNetworkInterface = EC2
    type Rs AttachNetworkInterface = AttachNetworkInterfaceResponse

    request = post "AttachNetworkInterface"
    response _ = xmlResponse
