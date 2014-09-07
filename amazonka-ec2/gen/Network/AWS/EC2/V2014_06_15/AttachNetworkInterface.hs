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
    , mkAttachNetworkInterface
    -- ** Request lenses
    , aniNetworkInterfaceId
    , aniInstanceId
    , aniDeviceIndex

    -- * Response
    , AttachNetworkInterfaceResponse
    -- ** Response lenses
    , anirsAttachmentId
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | 
data AttachNetworkInterface = AttachNetworkInterface
    { _aniNetworkInterfaceId :: Text
    , _aniInstanceId :: Text
    , _aniDeviceIndex :: Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AttachNetworkInterface' request.
mkAttachNetworkInterface :: Text -- ^ 'aniNetworkInterfaceId'
                         -> Text -- ^ 'aniInstanceId'
                         -> Integer -- ^ 'aniDeviceIndex'
                         -> AttachNetworkInterface
mkAttachNetworkInterface p1 p2 p3 = AttachNetworkInterface
    { _aniNetworkInterfaceId = p1
    , _aniInstanceId = p2
    , _aniDeviceIndex = p3
    }

-- | The ID of the network interface.
aniNetworkInterfaceId :: Lens' AttachNetworkInterface Text
aniNetworkInterfaceId =
    lens _aniNetworkInterfaceId (\s a -> s { _aniNetworkInterfaceId = a })

-- | The ID of the instance.
aniInstanceId :: Lens' AttachNetworkInterface Text
aniInstanceId = lens _aniInstanceId (\s a -> s { _aniInstanceId = a })

-- | The index of the device for the network interface attachment.
aniDeviceIndex :: Lens' AttachNetworkInterface Integer
aniDeviceIndex = lens _aniDeviceIndex (\s a -> s { _aniDeviceIndex = a })

instance ToQuery AttachNetworkInterface where
    toQuery = genericQuery def

-- | 
newtype AttachNetworkInterfaceResponse = AttachNetworkInterfaceResponse
    { _anirsAttachmentId :: Maybe Text
    } deriving (Show, Generic)

-- | The ID of the network interface attachment.
anirsAttachmentId :: Lens' AttachNetworkInterfaceResponse (Maybe Text)
anirsAttachmentId =
    lens _anirsAttachmentId (\s a -> s { _anirsAttachmentId = a })

instance FromXML AttachNetworkInterfaceResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest AttachNetworkInterface where
    type Sv AttachNetworkInterface = EC2
    type Rs AttachNetworkInterface = AttachNetworkInterfaceResponse

    request = post "AttachNetworkInterface"
    response _ = xmlResponse
