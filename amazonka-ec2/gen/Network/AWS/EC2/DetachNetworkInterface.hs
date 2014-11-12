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
    , dni2AttachmentId
    , dni2DryRun
    , dni2Force

    -- * Response
    , DetachNetworkInterfaceResponse
    -- ** Response constructor
    , detachNetworkInterfaceResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data DetachNetworkInterface = DetachNetworkInterface
    { _dni2AttachmentId :: Text
    , _dni2DryRun       :: Maybe Bool
    , _dni2Force        :: Maybe Bool
    } (Eq, Ord, Show, Generic)

-- | 'DetachNetworkInterface' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dni2AttachmentId' @::@ 'Text'
--
-- * 'dni2DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dni2Force' @::@ 'Maybe' 'Bool'
--
detachNetworkInterface :: Text -- ^ 'dni2AttachmentId'
                       -> DetachNetworkInterface
detachNetworkInterface p1 = DetachNetworkInterface
    { _dni2AttachmentId = p1
    , _dni2DryRun       = Nothing
    , _dni2Force        = Nothing
    }

-- | The ID of the attachment.
dni2AttachmentId :: Lens' DetachNetworkInterface Text
dni2AttachmentId = lens _dni2AttachmentId (\s a -> s { _dni2AttachmentId = a })

dni2DryRun :: Lens' DetachNetworkInterface (Maybe Bool)
dni2DryRun = lens _dni2DryRun (\s a -> s { _dni2DryRun = a })

-- | Specifies whether to force a detachment.
dni2Force :: Lens' DetachNetworkInterface (Maybe Bool)
dni2Force = lens _dni2Force (\s a -> s { _dni2Force = a })
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
