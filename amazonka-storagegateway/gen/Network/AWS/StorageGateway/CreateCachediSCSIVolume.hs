{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.CreateCachediSCSIVolume
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a cached volume on a specified cached gateway. This operation is supported only for the gateway-cached volume architecture.
--
-- Cache storage must be allocated to the gateway before you can create a cached volume. Use the < AddCache> operation to add cache storage to a gateway.
--
-- In the request, you must specify the gateway, size of the volume in bytes, the iSCSI target name, an IP address on which to expose the target, and a unique client token. In response, AWS Storage Gateway creates the volume and returns information about it such as the volume Amazon Resource Name (ARN), its size, and the iSCSI target ARN that initiators can use to connect to the volume target.
module Network.AWS.StorageGateway.CreateCachediSCSIVolume
    (
    -- * Creating a Request
      createCachediSCSIVolume
    , CreateCachediSCSIVolume
    -- * Request Lenses
    , ccscsivSnapshotId
    , ccscsivGatewayARN
    , ccscsivVolumeSizeInBytes
    , ccscsivTargetName
    , ccscsivNetworkInterfaceId
    , ccscsivClientToken

    -- * Destructuring the Response
    , createCachediSCSIVolumeResponse
    , CreateCachediSCSIVolumeResponse
    -- * Response Lenses
    , ccscsivrsTargetARN
    , ccscsivrsVolumeARN
    , ccscsivrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types
import           Network.AWS.StorageGateway.Types.Product

-- | /See:/ 'createCachediSCSIVolume' smart constructor.
data CreateCachediSCSIVolume = CreateCachediSCSIVolume'
    { _ccscsivSnapshotId         :: !(Maybe Text)
    , _ccscsivGatewayARN         :: !Text
    , _ccscsivVolumeSizeInBytes  :: !Integer
    , _ccscsivTargetName         :: !Text
    , _ccscsivNetworkInterfaceId :: !Text
    , _ccscsivClientToken        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateCachediSCSIVolume' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
createCachediSCSIVolume
    :: Text -- ^ 'ccscsivGatewayARN'
    -> Integer -- ^ 'ccscsivVolumeSizeInBytes'
    -> Text -- ^ 'ccscsivTargetName'
    -> Text -- ^ 'ccscsivNetworkInterfaceId'
    -> Text -- ^ 'ccscsivClientToken'
    -> CreateCachediSCSIVolume
createCachediSCSIVolume pGatewayARN_ pVolumeSizeInBytes_ pTargetName_ pNetworkInterfaceId_ pClientToken_ =
    CreateCachediSCSIVolume'
    { _ccscsivSnapshotId = Nothing
    , _ccscsivGatewayARN = pGatewayARN_
    , _ccscsivVolumeSizeInBytes = pVolumeSizeInBytes_
    , _ccscsivTargetName = pTargetName_
    , _ccscsivNetworkInterfaceId = pNetworkInterfaceId_
    , _ccscsivClientToken = pClientToken_
    }

-- | Undocumented member.
ccscsivSnapshotId :: Lens' CreateCachediSCSIVolume (Maybe Text)
ccscsivSnapshotId = lens _ccscsivSnapshotId (\ s a -> s{_ccscsivSnapshotId = a});

-- | Undocumented member.
ccscsivGatewayARN :: Lens' CreateCachediSCSIVolume Text
ccscsivGatewayARN = lens _ccscsivGatewayARN (\ s a -> s{_ccscsivGatewayARN = a});

-- | Undocumented member.
ccscsivVolumeSizeInBytes :: Lens' CreateCachediSCSIVolume Integer
ccscsivVolumeSizeInBytes = lens _ccscsivVolumeSizeInBytes (\ s a -> s{_ccscsivVolumeSizeInBytes = a});

-- | Undocumented member.
ccscsivTargetName :: Lens' CreateCachediSCSIVolume Text
ccscsivTargetName = lens _ccscsivTargetName (\ s a -> s{_ccscsivTargetName = a});

-- | Undocumented member.
ccscsivNetworkInterfaceId :: Lens' CreateCachediSCSIVolume Text
ccscsivNetworkInterfaceId = lens _ccscsivNetworkInterfaceId (\ s a -> s{_ccscsivNetworkInterfaceId = a});

-- | Undocumented member.
ccscsivClientToken :: Lens' CreateCachediSCSIVolume Text
ccscsivClientToken = lens _ccscsivClientToken (\ s a -> s{_ccscsivClientToken = a});

instance AWSRequest CreateCachediSCSIVolume where
        type Rs CreateCachediSCSIVolume =
             CreateCachediSCSIVolumeResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 CreateCachediSCSIVolumeResponse' <$>
                   (x .?> "TargetARN") <*> (x .?> "VolumeARN") <*>
                     (pure (fromEnum s)))

instance Hashable CreateCachediSCSIVolume

instance NFData CreateCachediSCSIVolume

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
              (catMaybes
                 [("SnapshotId" .=) <$> _ccscsivSnapshotId,
                  Just ("GatewayARN" .= _ccscsivGatewayARN),
                  Just
                    ("VolumeSizeInBytes" .= _ccscsivVolumeSizeInBytes),
                  Just ("TargetName" .= _ccscsivTargetName),
                  Just
                    ("NetworkInterfaceId" .= _ccscsivNetworkInterfaceId),
                  Just ("ClientToken" .= _ccscsivClientToken)])

instance ToPath CreateCachediSCSIVolume where
        toPath = const "/"

instance ToQuery CreateCachediSCSIVolume where
        toQuery = const mempty

-- | /See:/ 'createCachediSCSIVolumeResponse' smart constructor.
data CreateCachediSCSIVolumeResponse = CreateCachediSCSIVolumeResponse'
    { _ccscsivrsTargetARN      :: !(Maybe Text)
    , _ccscsivrsVolumeARN      :: !(Maybe Text)
    , _ccscsivrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateCachediSCSIVolumeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccscsivrsTargetARN'
--
-- * 'ccscsivrsVolumeARN'
--
-- * 'ccscsivrsResponseStatus'
createCachediSCSIVolumeResponse
    :: Int -- ^ 'ccscsivrsResponseStatus'
    -> CreateCachediSCSIVolumeResponse
createCachediSCSIVolumeResponse pResponseStatus_ =
    CreateCachediSCSIVolumeResponse'
    { _ccscsivrsTargetARN = Nothing
    , _ccscsivrsVolumeARN = Nothing
    , _ccscsivrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
ccscsivrsTargetARN :: Lens' CreateCachediSCSIVolumeResponse (Maybe Text)
ccscsivrsTargetARN = lens _ccscsivrsTargetARN (\ s a -> s{_ccscsivrsTargetARN = a});

-- | Undocumented member.
ccscsivrsVolumeARN :: Lens' CreateCachediSCSIVolumeResponse (Maybe Text)
ccscsivrsVolumeARN = lens _ccscsivrsVolumeARN (\ s a -> s{_ccscsivrsVolumeARN = a});

-- | The response status code.
ccscsivrsResponseStatus :: Lens' CreateCachediSCSIVolumeResponse Int
ccscsivrsResponseStatus = lens _ccscsivrsResponseStatus (\ s a -> s{_ccscsivrsResponseStatus = a});

instance NFData CreateCachediSCSIVolumeResponse
