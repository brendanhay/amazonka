{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
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
    -- ** Default constructor
    , detachNetworkInterface
    -- ** Accessors and lenses
    , _dniuAttachmentId
    , dniuAttachmentId
    , _dniuForce
    , dniuForce

    -- * Response
    , DetachNetworkInterfaceResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DetachNetworkInterface' request.
detachNetworkInterface :: Text -- ^ 'dniuAttachmentId'
                       -> DetachNetworkInterface
detachNetworkInterface p1 = DetachNetworkInterface
    { _dniuAttachmentId = p1
    , _dniuForce = Nothing
    }

data DetachNetworkInterface = DetachNetworkInterface

makeSiglessLenses ''DetachNetworkInterface

instance ToQuery DetachNetworkInterface where
    toQuery = genericQuery def

data DetachNetworkInterfaceResponse = DetachNetworkInterfaceResponse
    deriving (Eq, Show, Generic)

makeSiglessLenses ''DetachNetworkInterfaceResponse

instance AWSRequest DetachNetworkInterface where
    type Sv DetachNetworkInterface = EC2
    type Rs DetachNetworkInterface = DetachNetworkInterfaceResponse

    request = post "DetachNetworkInterface"
    response _ = nullaryResponse DetachNetworkInterfaceResponse

-- | The ID of the attachment.
dniuAttachmentId :: Lens' DetachNetworkInterface (Text)

-- | Specifies whether to force a detachment.
dniuForce :: Lens' DetachNetworkInterface (Maybe Bool)
