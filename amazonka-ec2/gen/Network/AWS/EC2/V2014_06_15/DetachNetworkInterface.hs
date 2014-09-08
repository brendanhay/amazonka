{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DetachNetworkInterface
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Detaches a network interface from an instance. Example This example
-- detaches the specified elastic network interface (ENI).
-- https://ec2.amazonaws.com/?Action=DetachNetworkInterface
-- &amp;AttachmentId=eni-attach-d94b09b0 &amp;AUTHPARAMS
-- &lt;DetachNetworkInterfaceResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;ce540707-0635-46bc-97da-33a8a362a0e8&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DetachNetworkInterfaceResponse&gt;.
module Network.AWS.EC2.V2014_06_15.DetachNetworkInterface
    (
    -- * Request
      DetachNetworkInterface
    -- ** Request constructor
    , mkDetachNetworkInterface
    -- ** Request lenses
    , dni2AttachmentId
    , dni2Force

    -- * Response
    , DetachNetworkInterfaceResponse
    -- ** Response constructor
    , mkDetachNetworkInterfaceResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | 
data DetachNetworkInterface = DetachNetworkInterface
    { _dni2AttachmentId :: Text
    , _dni2Force :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DetachNetworkInterface' request.
mkDetachNetworkInterface :: Text -- ^ 'dni2AttachmentId'
                         -> DetachNetworkInterface
mkDetachNetworkInterface p1 = DetachNetworkInterface
    { _dni2AttachmentId = p1
    , _dni2Force = Nothing
    }

-- | The ID of the attachment.
dni2AttachmentId :: Lens' DetachNetworkInterface Text
dni2AttachmentId =
    lens _dni2AttachmentId (\s a -> s { _dni2AttachmentId = a })

-- | Specifies whether to force a detachment.
dni2Force :: Lens' DetachNetworkInterface (Maybe Bool)
dni2Force = lens _dni2Force (\s a -> s { _dni2Force = a })

instance ToQuery DetachNetworkInterface where
    toQuery = genericQuery def

data DetachNetworkInterfaceResponse = DetachNetworkInterfaceResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DetachNetworkInterfaceResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDetachNetworkInterfaceResponse :: DetachNetworkInterfaceResponse
mkDetachNetworkInterfaceResponse = DetachNetworkInterfaceResponse

instance AWSRequest DetachNetworkInterface where
    type Sv DetachNetworkInterface = EC2
    type Rs DetachNetworkInterface = DetachNetworkInterfaceResponse

    request = post "DetachNetworkInterface"
    response _ = nullaryResponse DetachNetworkInterfaceResponse
