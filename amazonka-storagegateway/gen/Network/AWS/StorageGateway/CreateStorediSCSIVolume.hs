{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.CreateStorediSCSIVolume
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation creates a volume on a specified gateway. This operation
-- is supported only for the gateway-stored volume architecture.
--
-- The size of the volume to create is inferred from the disk size. You can
-- choose to preserve existing data on the disk, create volume from an
-- existing snapshot, or create an empty volume. If you choose to create an
-- empty gateway volume, then any existing data on the disk is erased.
--
-- In the request you must specify the gateway and the disk information on
-- which you are creating the volume. In response, AWS Storage Gateway
-- creates the volume and returns volume information such as the volume
-- Amazon Resource Name (ARN), its size, and the iSCSI target ARN that
-- initiators can use to connect to the volume target.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_CreateStorediSCSIVolume.html>
module Network.AWS.StorageGateway.CreateStorediSCSIVolume
    (
    -- * Request
      CreateStorediSCSIVolume
    -- ** Request constructor
    , createStorediSCSIVolume
    -- ** Request lenses
    , csscsivSnapshotId
    , csscsivGatewayARN
    , csscsivDiskId
    , csscsivPreserveExistingData
    , csscsivTargetName
    , csscsivNetworkInterfaceId

    -- * Response
    , CreateStorediSCSIVolumeResponse
    -- ** Response constructor
    , createStorediSCSIVolumeResponse
    -- ** Response lenses
    , csscsivrsTargetARN
    , csscsivrsVolumeARN
    , csscsivrsVolumeSizeInBytes
    , csscsivrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types

-- | A JSON object containing one or more of the following fields:
--
-- -   CreateStorediSCSIVolumeInput$DiskId
-- -   CreateStorediSCSIVolumeInput$NetworkInterfaceId
-- -   CreateStorediSCSIVolumeInput$PreserveExistingData
-- -   CreateStorediSCSIVolumeInput$SnapshotId
-- -   CreateStorediSCSIVolumeInput$TargetName
--
-- /See:/ 'createStorediSCSIVolume' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csscsivSnapshotId'
--
-- * 'csscsivGatewayARN'
--
-- * 'csscsivDiskId'
--
-- * 'csscsivPreserveExistingData'
--
-- * 'csscsivTargetName'
--
-- * 'csscsivNetworkInterfaceId'
data CreateStorediSCSIVolume = CreateStorediSCSIVolume'
    { _csscsivSnapshotId           :: !(Maybe Text)
    , _csscsivGatewayARN           :: !Text
    , _csscsivDiskId               :: !Text
    , _csscsivPreserveExistingData :: !Bool
    , _csscsivTargetName           :: !Text
    , _csscsivNetworkInterfaceId   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateStorediSCSIVolume' smart constructor.
createStorediSCSIVolume :: Text -> Text -> Bool -> Text -> Text -> CreateStorediSCSIVolume
createStorediSCSIVolume pGatewayARN_ pDiskId_ pPreserveExistingData_ pTargetName_ pNetworkInterfaceId_ =
    CreateStorediSCSIVolume'
    { _csscsivSnapshotId = Nothing
    , _csscsivGatewayARN = pGatewayARN_
    , _csscsivDiskId = pDiskId_
    , _csscsivPreserveExistingData = pPreserveExistingData_
    , _csscsivTargetName = pTargetName_
    , _csscsivNetworkInterfaceId = pNetworkInterfaceId_
    }

-- | The snapshot ID (e.g. \"snap-1122aabb\") of the snapshot to restore as
-- the new stored volume. Specify this field if you want to create the
-- iSCSI storage volume from a snapshot otherwise do not include this
-- field. To list snapshots for your account use
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSnapshots.html DescribeSnapshots>
-- in the /Amazon Elastic Compute Cloud API Reference/.
csscsivSnapshotId :: Lens' CreateStorediSCSIVolume (Maybe Text)
csscsivSnapshotId = lens _csscsivSnapshotId (\ s a -> s{_csscsivSnapshotId = a});

-- | FIXME: Undocumented member.
csscsivGatewayARN :: Lens' CreateStorediSCSIVolume Text
csscsivGatewayARN = lens _csscsivGatewayARN (\ s a -> s{_csscsivGatewayARN = a});

-- | The unique identifier for the gateway local disk that is configured as a
-- stored volume. Use
-- <http://docs.aws.amazon.com/storagegateway/latest/userguide/API_ListLocalDisks.html ListLocalDisks>
-- to list disk IDs for a gateway.
csscsivDiskId :: Lens' CreateStorediSCSIVolume Text
csscsivDiskId = lens _csscsivDiskId (\ s a -> s{_csscsivDiskId = a});

-- | Specify this field as true if you want to preserve the data on the local
-- disk. Otherwise, specifying this field as false creates an empty volume.
--
-- /Valid Values/: true, false
csscsivPreserveExistingData :: Lens' CreateStorediSCSIVolume Bool
csscsivPreserveExistingData = lens _csscsivPreserveExistingData (\ s a -> s{_csscsivPreserveExistingData = a});

-- | The name of the iSCSI target used by initiators to connect to the target
-- and as a suffix for the target ARN. For example, specifying @TargetName@
-- as /myvolume/ results in the target ARN of
-- arn:aws:storagegateway:us-east-1:111122223333:gateway\/mygateway\/target\/iqn.1997-05.com.amazon:myvolume.
-- The target name must be unique across all volumes of a gateway.
csscsivTargetName :: Lens' CreateStorediSCSIVolume Text
csscsivTargetName = lens _csscsivTargetName (\ s a -> s{_csscsivTargetName = a});

-- | The network interface of the gateway on which to expose the iSCSI
-- target. Only IPv4 addresses are accepted. Use DescribeGatewayInformation
-- to get a list of the network interfaces available on a gateway.
--
-- /Valid Values/: A valid IP address.
csscsivNetworkInterfaceId :: Lens' CreateStorediSCSIVolume Text
csscsivNetworkInterfaceId = lens _csscsivNetworkInterfaceId (\ s a -> s{_csscsivNetworkInterfaceId = a});

instance AWSRequest CreateStorediSCSIVolume where
        type Sv CreateStorediSCSIVolume = StorageGateway
        type Rs CreateStorediSCSIVolume =
             CreateStorediSCSIVolumeResponse
        request = postJSON "CreateStorediSCSIVolume"
        response
          = receiveJSON
              (\ s h x ->
                 CreateStorediSCSIVolumeResponse' <$>
                   (x .?> "TargetARN") <*> (x .?> "VolumeARN") <*>
                     (x .?> "VolumeSizeInBytes")
                     <*> (pure (fromEnum s)))

instance ToHeaders CreateStorediSCSIVolume where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.CreateStorediSCSIVolume" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateStorediSCSIVolume where
        toJSON CreateStorediSCSIVolume'{..}
          = object
              ["SnapshotId" .= _csscsivSnapshotId,
               "GatewayARN" .= _csscsivGatewayARN,
               "DiskId" .= _csscsivDiskId,
               "PreserveExistingData" .=
                 _csscsivPreserveExistingData,
               "TargetName" .= _csscsivTargetName,
               "NetworkInterfaceId" .= _csscsivNetworkInterfaceId]

instance ToPath CreateStorediSCSIVolume where
        toPath = const "/"

instance ToQuery CreateStorediSCSIVolume where
        toQuery = const mempty

-- | A JSON object containing the following fields:
--
-- /See:/ 'createStorediSCSIVolumeResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csscsivrsTargetARN'
--
-- * 'csscsivrsVolumeARN'
--
-- * 'csscsivrsVolumeSizeInBytes'
--
-- * 'csscsivrsStatus'
data CreateStorediSCSIVolumeResponse = CreateStorediSCSIVolumeResponse'
    { _csscsivrsTargetARN         :: !(Maybe Text)
    , _csscsivrsVolumeARN         :: !(Maybe Text)
    , _csscsivrsVolumeSizeInBytes :: !(Maybe Integer)
    , _csscsivrsStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateStorediSCSIVolumeResponse' smart constructor.
createStorediSCSIVolumeResponse :: Int -> CreateStorediSCSIVolumeResponse
createStorediSCSIVolumeResponse pStatus_ =
    CreateStorediSCSIVolumeResponse'
    { _csscsivrsTargetARN = Nothing
    , _csscsivrsVolumeARN = Nothing
    , _csscsivrsVolumeSizeInBytes = Nothing
    , _csscsivrsStatus = pStatus_
    }

-- | he Amazon Resource Name (ARN) of the volume target that includes the
-- iSCSI name that initiators can use to connect to the target.
csscsivrsTargetARN :: Lens' CreateStorediSCSIVolumeResponse (Maybe Text)
csscsivrsTargetARN = lens _csscsivrsTargetARN (\ s a -> s{_csscsivrsTargetARN = a});

-- | The Amazon Resource Name (ARN) of the configured volume.
csscsivrsVolumeARN :: Lens' CreateStorediSCSIVolumeResponse (Maybe Text)
csscsivrsVolumeARN = lens _csscsivrsVolumeARN (\ s a -> s{_csscsivrsVolumeARN = a});

-- | The size of the volume in bytes.
csscsivrsVolumeSizeInBytes :: Lens' CreateStorediSCSIVolumeResponse (Maybe Integer)
csscsivrsVolumeSizeInBytes = lens _csscsivrsVolumeSizeInBytes (\ s a -> s{_csscsivrsVolumeSizeInBytes = a});

-- | FIXME: Undocumented member.
csscsivrsStatus :: Lens' CreateStorediSCSIVolumeResponse Int
csscsivrsStatus = lens _csscsivrsStatus (\ s a -> s{_csscsivrsStatus = a});
