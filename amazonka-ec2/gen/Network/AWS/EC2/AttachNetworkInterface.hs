{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.AttachNetworkInterface
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Attaches a network interface to an instance.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AttachNetworkInterface.html>
module Network.AWS.EC2.AttachNetworkInterface
    (
    -- * Request
      AttachNetworkInterface
    -- ** Request constructor
    , attachNetworkInterface
    -- ** Request lenses
    , aniDeviceIndex
    , aniDryRun
    , aniInstanceId
    , aniNetworkInterfaceId

    -- * Response
    , AttachNetworkInterfaceResponse
    -- ** Response constructor
    , attachNetworkInterfaceResponse
    -- ** Response lenses
    , anirAttachmentId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data AttachNetworkInterface = AttachNetworkInterface
    { _aniDeviceIndex        :: Int
    , _aniDryRun             :: Maybe Bool
    , _aniInstanceId         :: Text
    , _aniNetworkInterfaceId :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'AttachNetworkInterface' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aniDeviceIndex' @::@ 'Int'
--
-- * 'aniDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'aniInstanceId' @::@ 'Text'
--
-- * 'aniNetworkInterfaceId' @::@ 'Text'
--
attachNetworkInterface :: Text -- ^ 'aniNetworkInterfaceId'
                       -> Text -- ^ 'aniInstanceId'
                       -> Int -- ^ 'aniDeviceIndex'
                       -> AttachNetworkInterface
attachNetworkInterface p1 p2 p3 = AttachNetworkInterface
    { _aniNetworkInterfaceId = p1
    , _aniInstanceId         = p2
    , _aniDeviceIndex        = p3
    , _aniDryRun             = Nothing
    }

-- | The index of the device for the network interface attachment.
aniDeviceIndex :: Lens' AttachNetworkInterface Int
aniDeviceIndex = lens _aniDeviceIndex (\s a -> s { _aniDeviceIndex = a })

aniDryRun :: Lens' AttachNetworkInterface (Maybe Bool)
aniDryRun = lens _aniDryRun (\s a -> s { _aniDryRun = a })

-- | The ID of the instance.
aniInstanceId :: Lens' AttachNetworkInterface Text
aniInstanceId = lens _aniInstanceId (\s a -> s { _aniInstanceId = a })

-- | The ID of the network interface.
aniNetworkInterfaceId :: Lens' AttachNetworkInterface Text
aniNetworkInterfaceId =
    lens _aniNetworkInterfaceId (\s a -> s { _aniNetworkInterfaceId = a })

newtype AttachNetworkInterfaceResponse = AttachNetworkInterfaceResponse
    { _anirAttachmentId :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'AttachNetworkInterfaceResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'anirAttachmentId' @::@ 'Maybe' 'Text'
--
attachNetworkInterfaceResponse :: AttachNetworkInterfaceResponse
attachNetworkInterfaceResponse = AttachNetworkInterfaceResponse
    { _anirAttachmentId = Nothing
    }

-- | The ID of the network interface attachment.
anirAttachmentId :: Lens' AttachNetworkInterfaceResponse (Maybe Text)
anirAttachmentId = lens _anirAttachmentId (\s a -> s { _anirAttachmentId = a })

instance ToPath AttachNetworkInterface where
    toPath = const "/"

instance ToQuery AttachNetworkInterface where
    toQuery AttachNetworkInterface{..} = mconcat
        [ "DeviceIndex"        =? _aniDeviceIndex
        , "DryRun"             =? _aniDryRun
        , "InstanceId"         =? _aniInstanceId
        , "NetworkInterfaceId" =? _aniNetworkInterfaceId
        ]

instance ToHeaders AttachNetworkInterface

instance AWSRequest AttachNetworkInterface where
    type Sv AttachNetworkInterface = EC2
    type Rs AttachNetworkInterface = AttachNetworkInterfaceResponse

    request  = post "AttachNetworkInterface"
    response = xmlResponse

instance FromXML AttachNetworkInterfaceResponse where
    parseXML x = AttachNetworkInterfaceResponse
        <$> x .@? "attachmentId"
