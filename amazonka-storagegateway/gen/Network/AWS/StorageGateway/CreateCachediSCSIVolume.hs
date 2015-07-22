{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.CreateCachediSCSIVolume
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation creates a cached volume on a specified cached gateway.
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
    , ccscsivrqSnapshotId
    , ccscsivrqGatewayARN
    , ccscsivrqVolumeSizeInBytes
    , ccscsivrqTargetName
    , ccscsivrqNetworkInterfaceId
    , ccscsivrqClientToken

    -- * Response
    , CreateCachediSCSIVolumeResponse
    -- ** Response constructor
    , createCachediSCSIVolumeResponse
    -- ** Response lenses
    , ccscsivrsTargetARN
    , ccscsivrsVolumeARN
    , ccscsivrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types

-- | /See:/ 'createCachediSCSIVolume' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccscsivrqSnapshotId'
--
-- * 'ccscsivrqGatewayARN'
--
-- * 'ccscsivrqVolumeSizeInBytes'
--
-- * 'ccscsivrqTargetName'
--
-- * 'ccscsivrqNetworkInterfaceId'
--
-- * 'ccscsivrqClientToken'
data CreateCachediSCSIVolume = CreateCachediSCSIVolume'
    { _ccscsivrqSnapshotId         :: !(Maybe Text)
    , _ccscsivrqGatewayARN         :: !Text
    , _ccscsivrqVolumeSizeInBytes  :: !Integer
    , _ccscsivrqTargetName         :: !Text
    , _ccscsivrqNetworkInterfaceId :: !Text
    , _ccscsivrqClientToken        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateCachediSCSIVolume' smart constructor.
createCachediSCSIVolume :: Text -> Integer -> Text -> Text -> Text -> CreateCachediSCSIVolume
createCachediSCSIVolume pGatewayARN pVolumeSizeInBytes pTargetName pNetworkInterfaceId pClientToken =
    CreateCachediSCSIVolume'
    { _ccscsivrqSnapshotId = Nothing
    , _ccscsivrqGatewayARN = pGatewayARN
    , _ccscsivrqVolumeSizeInBytes = pVolumeSizeInBytes
    , _ccscsivrqTargetName = pTargetName
    , _ccscsivrqNetworkInterfaceId = pNetworkInterfaceId
    , _ccscsivrqClientToken = pClientToken
    }

-- | FIXME: Undocumented member.
ccscsivrqSnapshotId :: Lens' CreateCachediSCSIVolume (Maybe Text)
ccscsivrqSnapshotId = lens _ccscsivrqSnapshotId (\ s a -> s{_ccscsivrqSnapshotId = a});

-- | FIXME: Undocumented member.
ccscsivrqGatewayARN :: Lens' CreateCachediSCSIVolume Text
ccscsivrqGatewayARN = lens _ccscsivrqGatewayARN (\ s a -> s{_ccscsivrqGatewayARN = a});

-- | FIXME: Undocumented member.
ccscsivrqVolumeSizeInBytes :: Lens' CreateCachediSCSIVolume Integer
ccscsivrqVolumeSizeInBytes = lens _ccscsivrqVolumeSizeInBytes (\ s a -> s{_ccscsivrqVolumeSizeInBytes = a});

-- | FIXME: Undocumented member.
ccscsivrqTargetName :: Lens' CreateCachediSCSIVolume Text
ccscsivrqTargetName = lens _ccscsivrqTargetName (\ s a -> s{_ccscsivrqTargetName = a});

-- | FIXME: Undocumented member.
ccscsivrqNetworkInterfaceId :: Lens' CreateCachediSCSIVolume Text
ccscsivrqNetworkInterfaceId = lens _ccscsivrqNetworkInterfaceId (\ s a -> s{_ccscsivrqNetworkInterfaceId = a});

-- | FIXME: Undocumented member.
ccscsivrqClientToken :: Lens' CreateCachediSCSIVolume Text
ccscsivrqClientToken = lens _ccscsivrqClientToken (\ s a -> s{_ccscsivrqClientToken = a});

instance AWSRequest CreateCachediSCSIVolume where
        type Sv CreateCachediSCSIVolume = StorageGateway
        type Rs CreateCachediSCSIVolume =
             CreateCachediSCSIVolumeResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateCachediSCSIVolumeResponse' <$>
                   (x .?> "TargetARN") <*> (x .?> "VolumeARN") <*>
                     (pure (fromEnum s)))

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
              ["SnapshotId" .= _ccscsivrqSnapshotId,
               "GatewayARN" .= _ccscsivrqGatewayARN,
               "VolumeSizeInBytes" .= _ccscsivrqVolumeSizeInBytes,
               "TargetName" .= _ccscsivrqTargetName,
               "NetworkInterfaceId" .= _ccscsivrqNetworkInterfaceId,
               "ClientToken" .= _ccscsivrqClientToken]

instance ToPath CreateCachediSCSIVolume where
        toPath = const "/"

instance ToQuery CreateCachediSCSIVolume where
        toQuery = const mempty

-- | /See:/ 'createCachediSCSIVolumeResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccscsivrsTargetARN'
--
-- * 'ccscsivrsVolumeARN'
--
-- * 'ccscsivrsStatus'
data CreateCachediSCSIVolumeResponse = CreateCachediSCSIVolumeResponse'
    { _ccscsivrsTargetARN :: !(Maybe Text)
    , _ccscsivrsVolumeARN :: !(Maybe Text)
    , _ccscsivrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateCachediSCSIVolumeResponse' smart constructor.
createCachediSCSIVolumeResponse :: Int -> CreateCachediSCSIVolumeResponse
createCachediSCSIVolumeResponse pStatus =
    CreateCachediSCSIVolumeResponse'
    { _ccscsivrsTargetARN = Nothing
    , _ccscsivrsVolumeARN = Nothing
    , _ccscsivrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
ccscsivrsTargetARN :: Lens' CreateCachediSCSIVolumeResponse (Maybe Text)
ccscsivrsTargetARN = lens _ccscsivrsTargetARN (\ s a -> s{_ccscsivrsTargetARN = a});

-- | FIXME: Undocumented member.
ccscsivrsVolumeARN :: Lens' CreateCachediSCSIVolumeResponse (Maybe Text)
ccscsivrsVolumeARN = lens _ccscsivrsVolumeARN (\ s a -> s{_ccscsivrsVolumeARN = a});

-- | FIXME: Undocumented member.
ccscsivrsStatus :: Lens' CreateCachediSCSIVolumeResponse Int
ccscsivrsStatus = lens _ccscsivrsStatus (\ s a -> s{_ccscsivrsStatus = a});
