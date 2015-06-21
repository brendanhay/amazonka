{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.StorageGateway.CreateCachediSCSIVolume
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This operation creates a cached volume on a specified cached gateway.
-- This operation is supported only for the gateway-cached volume
-- architecture.
--
-- Cache storage must be allocated to the gateway before you can create a
-- cached volume. Use the AddCache operation to add cache storage to a
-- gateway.
--
-- In the request, you must specify the gateway, size of the volume in
-- bytes, the iSCSI target name, an IP address on which to expose the
-- target, and a unique client token. In response, AWS Storage Gateway
-- creates the volume and returns information about it such as the volume
-- Amazon Resource Name (ARN), its size, and the iSCSI target ARN that
-- initiators can use to connect to the volume target.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_CreateCachediSCSIVolume.html>
module Network.AWS.StorageGateway.CreateCachediSCSIVolume
    (
    -- * Request
      CreateCachediSCSIVolume
    -- ** Request constructor
    , createCachediSCSIVolume
    -- ** Request lenses
    , ccscsivSnapshotId
    , ccscsivGatewayARN
    , ccscsivVolumeSizeInBytes
    , ccscsivTargetName
    , ccscsivNetworkInterfaceId
    , ccscsivClientToken

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
import Network.AWS.Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'createCachediSCSIVolume' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccscsivSnapshotId'
--
-- * 'ccscsivGatewayARN'
--
-- * 'ccscsivVolumeSizeInBytes'
--
-- * 'ccscsivTargetName'
--
-- * 'ccscsivNetworkInterfaceId'
--
-- * 'ccscsivClientToken'
data CreateCachediSCSIVolume = CreateCachediSCSIVolume'{_ccscsivSnapshotId :: Maybe Text, _ccscsivGatewayARN :: Text, _ccscsivVolumeSizeInBytes :: Integer, _ccscsivTargetName :: Text, _ccscsivNetworkInterfaceId :: Text, _ccscsivClientToken :: Text} deriving (Eq, Read, Show)

-- | 'CreateCachediSCSIVolume' smart constructor.
createCachediSCSIVolume :: Text -> Integer -> Text -> Text -> Text -> CreateCachediSCSIVolume
createCachediSCSIVolume pGatewayARN pVolumeSizeInBytes pTargetName pNetworkInterfaceId pClientToken = CreateCachediSCSIVolume'{_ccscsivSnapshotId = Nothing, _ccscsivGatewayARN = pGatewayARN, _ccscsivVolumeSizeInBytes = pVolumeSizeInBytes, _ccscsivTargetName = pTargetName, _ccscsivNetworkInterfaceId = pNetworkInterfaceId, _ccscsivClientToken = pClientToken};

-- | FIXME: Undocumented member.
ccscsivSnapshotId :: Lens' CreateCachediSCSIVolume (Maybe Text)
ccscsivSnapshotId = lens _ccscsivSnapshotId (\ s a -> s{_ccscsivSnapshotId = a});

-- | FIXME: Undocumented member.
ccscsivGatewayARN :: Lens' CreateCachediSCSIVolume Text
ccscsivGatewayARN = lens _ccscsivGatewayARN (\ s a -> s{_ccscsivGatewayARN = a});

-- | FIXME: Undocumented member.
ccscsivVolumeSizeInBytes :: Lens' CreateCachediSCSIVolume Integer
ccscsivVolumeSizeInBytes = lens _ccscsivVolumeSizeInBytes (\ s a -> s{_ccscsivVolumeSizeInBytes = a});

-- | FIXME: Undocumented member.
ccscsivTargetName :: Lens' CreateCachediSCSIVolume Text
ccscsivTargetName = lens _ccscsivTargetName (\ s a -> s{_ccscsivTargetName = a});

-- | FIXME: Undocumented member.
ccscsivNetworkInterfaceId :: Lens' CreateCachediSCSIVolume Text
ccscsivNetworkInterfaceId = lens _ccscsivNetworkInterfaceId (\ s a -> s{_ccscsivNetworkInterfaceId = a});

-- | FIXME: Undocumented member.
ccscsivClientToken :: Lens' CreateCachediSCSIVolume Text
ccscsivClientToken = lens _ccscsivClientToken (\ s a -> s{_ccscsivClientToken = a});

instance AWSRequest CreateCachediSCSIVolume where
        type Sv CreateCachediSCSIVolume = StorageGateway
        type Rs CreateCachediSCSIVolume =
             CreateCachediSCSIVolumeResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateCachediSCSIVolumeResponse' <$>
                   (x .?> "TargetARN") <*> (x .?> "VolumeARN"))

instance ToHeaders CreateCachediSCSIVolume where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.CreateCachediSCSIVolume" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateCachediSCSIVolume where
        toJSON CreateCachediSCSIVolume'{..}
          = object
              ["SnapshotId" .= _ccscsivSnapshotId,
               "GatewayARN" .= _ccscsivGatewayARN,
               "VolumeSizeInBytes" .= _ccscsivVolumeSizeInBytes,
               "TargetName" .= _ccscsivTargetName,
               "NetworkInterfaceId" .= _ccscsivNetworkInterfaceId,
               "ClientToken" .= _ccscsivClientToken]

instance ToPath CreateCachediSCSIVolume where
        toPath = const "/"

instance ToQuery CreateCachediSCSIVolume where
        toQuery = const mempty

-- | /See:/ 'createCachediSCSIVolumeResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccscsivrTargetARN'
--
-- * 'ccscsivrVolumeARN'
data CreateCachediSCSIVolumeResponse = CreateCachediSCSIVolumeResponse'{_ccscsivrTargetARN :: Maybe Text, _ccscsivrVolumeARN :: Maybe Text} deriving (Eq, Read, Show)

-- | 'CreateCachediSCSIVolumeResponse' smart constructor.
createCachediSCSIVolumeResponse :: CreateCachediSCSIVolumeResponse
createCachediSCSIVolumeResponse = CreateCachediSCSIVolumeResponse'{_ccscsivrTargetARN = Nothing, _ccscsivrVolumeARN = Nothing};

-- | FIXME: Undocumented member.
ccscsivrTargetARN :: Lens' CreateCachediSCSIVolumeResponse (Maybe Text)
ccscsivrTargetARN = lens _ccscsivrTargetARN (\ s a -> s{_ccscsivrTargetARN = a});

-- | FIXME: Undocumented member.
ccscsivrVolumeARN :: Lens' CreateCachediSCSIVolumeResponse (Maybe Text)
ccscsivrVolumeARN = lens _ccscsivrVolumeARN (\ s a -> s{_ccscsivrVolumeARN = a});
