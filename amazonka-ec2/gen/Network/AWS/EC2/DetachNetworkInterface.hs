{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.EC2.DetachNetworkInterface
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Detaches a network interface from an instance.
module Network.AWS.EC2.DetachNetworkInterface
    (
    -- * Request
      DetachNetworkInterface
    -- ** Request constructor
    , detachNetworkInterface
    -- ** Request lenses
    , dniAttachmentId
    , dniDryRun
    , dniForce

    -- * Response
    , DetachNetworkInterfaceResponse
    -- ** Response constructor
    , detachNetworkInterfaceResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data DetachNetworkInterface = DetachNetworkInterface
    { _dniAttachmentId :: Text
    , _dniDryRun       :: Maybe Bool
    , _dniForce        :: Maybe Bool
    } deriving (Eq, Ord, Show, Generic)

-- | 'DetachNetworkInterface' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dniAttachmentId' @::@ 'Text'
--
-- * 'dniDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dniForce' @::@ 'Maybe' 'Bool'
--
detachNetworkInterface :: Text -- ^ 'dniAttachmentId'
                       -> DetachNetworkInterface
detachNetworkInterface p1 = DetachNetworkInterface
    { _dniAttachmentId = p1
    , _dniDryRun       = Nothing
    , _dniForce        = Nothing
    }

-- | The ID of the attachment.
dniAttachmentId :: Lens' DetachNetworkInterface Text
dniAttachmentId = lens _dniAttachmentId (\s a -> s { _dniAttachmentId = a })

dniDryRun :: Lens' DetachNetworkInterface (Maybe Bool)
dniDryRun = lens _dniDryRun (\s a -> s { _dniDryRun = a })

-- | Specifies whether to force a detachment.
dniForce :: Lens' DetachNetworkInterface (Maybe Bool)
dniForce = lens _dniForce (\s a -> s { _dniForce = a })

instance ToQuery DetachNetworkInterface

instance ToPath DetachNetworkInterface where
    toPath = const "/"

data DetachNetworkInterfaceResponse = DetachNetworkInterfaceResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DetachNetworkInterfaceResponse' constructor.
detachNetworkInterfaceResponse :: DetachNetworkInterfaceResponse
detachNetworkInterfaceResponse = DetachNetworkInterfaceResponse

instance FromXML DetachNetworkInterfaceResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DetachNetworkInterfaceResponse"

instance AWSRequest DetachNetworkInterface where
    type Sv DetachNetworkInterface = EC2
    type Rs DetachNetworkInterface = DetachNetworkInterfaceResponse

    request  = post "DetachNetworkInterface"
    response = nullaryResponse DetachNetworkInterfaceResponse
