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
data CreateCachediSCSIVolume = CreateCachediSCSIVolume'
    { _ccscsivSnapshotId         :: !(Maybe Text)
    , _ccscsivGatewayARN         :: !Text
    , _ccscsivVolumeSizeInBytes  :: !Integer
    , _ccscsivTargetName         :: !Text
    , _ccscsivNetworkInterfaceId :: !Text
    , _ccscsivClientToken        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateCachediSCSIVolume' smart constructor.
createCachediSCSIVolume :: Text -> Integer -> Text -> Text -> Text -> CreateCachediSCSIVolume
createCachediSCSIVolume pGatewayARN_ pVolumeSizeInBytes_ pTargetName_ pNetworkInterfaceId_ pClientToken_ =
    CreateCachediSCSIVolume'
    { _ccscsivSnapshotId = Nothing
    , _ccscsivGatewayARN = pGatewayARN_
    , _ccscsivVolumeSizeInBytes = pVolumeSizeInBytes_
    , _ccscsivTargetName = pTargetName_
    , _ccscsivNetworkInterfaceId = pNetworkInterfaceId_
    , _ccscsivClientToken = pClientToken_
    }

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
        request = postJSON "CreateCachediSCSIVolume"
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
createCachediSCSIVolumeResponse pStatus_ =
    CreateCachediSCSIVolumeResponse'
    { _ccscsivrsTargetARN = Nothing
    , _ccscsivrsVolumeARN = Nothing
    , _ccscsivrsStatus = pStatus_
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
