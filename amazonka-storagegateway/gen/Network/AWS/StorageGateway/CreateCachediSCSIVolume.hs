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

-- Module      : Network.AWS.StorageGateway.CreateCachediSCSIVolume
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation creates a cached volume on a specified cached gateway. This
-- operation is supported only for the gateway-cached volume architecture. In
-- the request, you must specify the gateway, size of the volume in bytes, the
-- iSCSI target name, an IP address on which to expose the target, and a
-- unique client token. In response, AWS Storage Gateway creates the volume
-- and returns information about it such as the volume Amazon Resource Name
-- (ARN), its size, and the iSCSI target ARN that initiators can use to
-- connect to the volume target.
module Network.AWS.StorageGateway.CreateCachediSCSIVolume
    (
    -- * Request
      CreateCachediSCSIVolume
    -- ** Request constructor
    , createCachediSCSIVolume
    -- ** Request lenses
    , ccscsivClientToken
    , ccscsivGatewayARN
    , ccscsivNetworkInterfaceId
    , ccscsivSnapshotId
    , ccscsivTargetName
    , ccscsivVolumeSizeInBytes

    -- * Response
    , CreateCachediSCSIVolumeResponse
    -- ** Response constructor
    , createCachediSCSIVolumeResponse
    -- ** Response lenses
    , ccscsivrTargetARN
    , ccscsivrVolumeARN
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.StorageGateway.Types

data CreateCachediSCSIVolume = CreateCachediSCSIVolume
    { _ccscsivClientToken        :: Text
    , _ccscsivGatewayARN         :: Text
    , _ccscsivNetworkInterfaceId :: Text
    , _ccscsivSnapshotId         :: Maybe Text
    , _ccscsivTargetName         :: Text
    , _ccscsivVolumeSizeInBytes  :: Integer
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateCachediSCSIVolume' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccscsivClientToken' @::@ 'Text'
--
-- * 'ccscsivGatewayARN' @::@ 'Text'
--
-- * 'ccscsivNetworkInterfaceId' @::@ 'Text'
--
-- * 'ccscsivSnapshotId' @::@ 'Maybe' 'Text'
--
-- * 'ccscsivTargetName' @::@ 'Text'
--
-- * 'ccscsivVolumeSizeInBytes' @::@ 'Integer'
--
createCachediSCSIVolume :: Text -- ^ 'ccscsivGatewayARN'
                        -> Integer -- ^ 'ccscsivVolumeSizeInBytes'
                        -> Text -- ^ 'ccscsivTargetName'
                        -> Text -- ^ 'ccscsivNetworkInterfaceId'
                        -> Text -- ^ 'ccscsivClientToken'
                        -> CreateCachediSCSIVolume
createCachediSCSIVolume p1 p2 p3 p4 p5 = CreateCachediSCSIVolume
    { _ccscsivGatewayARN         = p1
    , _ccscsivVolumeSizeInBytes  = p2
    , _ccscsivTargetName         = p3
    , _ccscsivNetworkInterfaceId = p4
    , _ccscsivClientToken        = p5
    , _ccscsivSnapshotId         = Nothing
    }

ccscsivClientToken :: Lens' CreateCachediSCSIVolume Text
ccscsivClientToken =
    lens _ccscsivClientToken (\s a -> s { _ccscsivClientToken = a })

ccscsivGatewayARN :: Lens' CreateCachediSCSIVolume Text
ccscsivGatewayARN =
    lens _ccscsivGatewayARN (\s a -> s { _ccscsivGatewayARN = a })

ccscsivNetworkInterfaceId :: Lens' CreateCachediSCSIVolume Text
ccscsivNetworkInterfaceId =
    lens _ccscsivNetworkInterfaceId
        (\s a -> s { _ccscsivNetworkInterfaceId = a })

ccscsivSnapshotId :: Lens' CreateCachediSCSIVolume (Maybe Text)
ccscsivSnapshotId =
    lens _ccscsivSnapshotId (\s a -> s { _ccscsivSnapshotId = a })

ccscsivTargetName :: Lens' CreateCachediSCSIVolume Text
ccscsivTargetName =
    lens _ccscsivTargetName (\s a -> s { _ccscsivTargetName = a })

ccscsivVolumeSizeInBytes :: Lens' CreateCachediSCSIVolume Integer
ccscsivVolumeSizeInBytes =
    lens _ccscsivVolumeSizeInBytes
        (\s a -> s { _ccscsivVolumeSizeInBytes = a })

instance ToPath CreateCachediSCSIVolume where
    toPath = const "/"

instance ToQuery CreateCachediSCSIVolume where
    toQuery = const mempty

instance ToHeaders CreateCachediSCSIVolume

instance ToBody CreateCachediSCSIVolume where
    toBody = toBody . encode . _ccscsivGatewayARN

data CreateCachediSCSIVolumeResponse = CreateCachediSCSIVolumeResponse
    { _ccscsivrTargetARN :: Maybe Text
    , _ccscsivrVolumeARN :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateCachediSCSIVolumeResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccscsivrTargetARN' @::@ 'Maybe' 'Text'
--
-- * 'ccscsivrVolumeARN' @::@ 'Maybe' 'Text'
--
createCachediSCSIVolumeResponse :: CreateCachediSCSIVolumeResponse
createCachediSCSIVolumeResponse = CreateCachediSCSIVolumeResponse
    { _ccscsivrVolumeARN = Nothing
    , _ccscsivrTargetARN = Nothing
    }

ccscsivrTargetARN :: Lens' CreateCachediSCSIVolumeResponse (Maybe Text)
ccscsivrTargetARN =
    lens _ccscsivrTargetARN (\s a -> s { _ccscsivrTargetARN = a })

ccscsivrVolumeARN :: Lens' CreateCachediSCSIVolumeResponse (Maybe Text)
ccscsivrVolumeARN =
    lens _ccscsivrVolumeARN (\s a -> s { _ccscsivrVolumeARN = a })

instance AWSRequest CreateCachediSCSIVolume where
    type Sv CreateCachediSCSIVolume = StorageGateway
    type Rs CreateCachediSCSIVolume = CreateCachediSCSIVolumeResponse

    request  = post
    response = jsonResponse $ \h o -> CreateCachediSCSIVolumeResponse
        <$> o .: "TargetARN"
        <*> o .: "VolumeARN"
