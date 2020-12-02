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
-- Module      : Network.AWS.StorageGateway.CreateStorediSCSIVolume
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a volume on a specified gateway. This operation is only supported in the stored volume gateway type.
--
--
-- The size of the volume to create is inferred from the disk size. You can choose to preserve existing data on the disk, create volume from an existing snapshot, or create an empty volume. If you choose to create an empty gateway volume, then any existing data on the disk is erased.
--
-- In the request you must specify the gateway and the disk information on which you are creating the volume. In response, the gateway creates the volume and returns volume information such as the volume Amazon Resource Name (ARN), its size, and the iSCSI target ARN that initiators can use to connect to the volume target.
--
module Network.AWS.StorageGateway.CreateStorediSCSIVolume
    (
    -- * Creating a Request
      createStorediSCSIVolume
    , CreateStorediSCSIVolume
    -- * Request Lenses
    , csscsivSnapshotId
    , csscsivGatewayARN
    , csscsivDiskId
    , csscsivPreserveExistingData
    , csscsivTargetName
    , csscsivNetworkInterfaceId

    -- * Destructuring the Response
    , createStorediSCSIVolumeResponse
    , CreateStorediSCSIVolumeResponse
    -- * Response Lenses
    , csscsivrsTargetARN
    , csscsivrsVolumeARN
    , csscsivrsVolumeSizeInBytes
    , csscsivrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.Types.Product

-- | A JSON object containing one or more of the following fields:
--
--
--     * 'CreateStorediSCSIVolumeInput$DiskId'
--
--     * 'CreateStorediSCSIVolumeInput$NetworkInterfaceId'
--
--     * 'CreateStorediSCSIVolumeInput$PreserveExistingData'
--
--     * 'CreateStorediSCSIVolumeInput$SnapshotId'
--
--     * 'CreateStorediSCSIVolumeInput$TargetName'
--
--
--
--
-- /See:/ 'createStorediSCSIVolume' smart constructor.
data CreateStorediSCSIVolume = CreateStorediSCSIVolume'
  { _csscsivSnapshotId           :: !(Maybe Text)
  , _csscsivGatewayARN           :: !Text
  , _csscsivDiskId               :: !Text
  , _csscsivPreserveExistingData :: !Bool
  , _csscsivTargetName           :: !Text
  , _csscsivNetworkInterfaceId   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateStorediSCSIVolume' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csscsivSnapshotId' - The snapshot ID (e.g. "snap-1122aabb") of the snapshot to restore as the new stored volume. Specify this field if you want to create the iSCSI storage volume from a snapshot otherwise do not include this field. To list snapshots for your account use <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSnapshots.html DescribeSnapshots> in the /Amazon Elastic Compute Cloud API Reference/ .
--
-- * 'csscsivGatewayARN' - Undocumented member.
--
-- * 'csscsivDiskId' - The unique identifier for the gateway local disk that is configured as a stored volume. Use <http://docs.aws.amazon.com/storagegateway/latest/userguide/API_ListLocalDisks.html ListLocalDisks> to list disk IDs for a gateway.
--
-- * 'csscsivPreserveExistingData' - Specify this field as true if you want to preserve the data on the local disk. Otherwise, specifying this field as false creates an empty volume. Valid Values: true, false
--
-- * 'csscsivTargetName' - The name of the iSCSI target used by initiators to connect to the target and as a suffix for the target ARN. For example, specifying @TargetName@ as /myvolume/ results in the target ARN of arn:aws:storagegateway:us-east-2:111122223333:gateway/sgw-12A3456B/target/iqn.1997-05.com.amazon:myvolume. The target name must be unique across all volumes of a gateway.
--
-- * 'csscsivNetworkInterfaceId' - The network interface of the gateway on which to expose the iSCSI target. Only IPv4 addresses are accepted. Use 'DescribeGatewayInformation' to get a list of the network interfaces available on a gateway. Valid Values: A valid IP address.
createStorediSCSIVolume
    :: Text -- ^ 'csscsivGatewayARN'
    -> Text -- ^ 'csscsivDiskId'
    -> Bool -- ^ 'csscsivPreserveExistingData'
    -> Text -- ^ 'csscsivTargetName'
    -> Text -- ^ 'csscsivNetworkInterfaceId'
    -> CreateStorediSCSIVolume
createStorediSCSIVolume pGatewayARN_ pDiskId_ pPreserveExistingData_ pTargetName_ pNetworkInterfaceId_ =
  CreateStorediSCSIVolume'
    { _csscsivSnapshotId = Nothing
    , _csscsivGatewayARN = pGatewayARN_
    , _csscsivDiskId = pDiskId_
    , _csscsivPreserveExistingData = pPreserveExistingData_
    , _csscsivTargetName = pTargetName_
    , _csscsivNetworkInterfaceId = pNetworkInterfaceId_
    }


-- | The snapshot ID (e.g. "snap-1122aabb") of the snapshot to restore as the new stored volume. Specify this field if you want to create the iSCSI storage volume from a snapshot otherwise do not include this field. To list snapshots for your account use <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSnapshots.html DescribeSnapshots> in the /Amazon Elastic Compute Cloud API Reference/ .
csscsivSnapshotId :: Lens' CreateStorediSCSIVolume (Maybe Text)
csscsivSnapshotId = lens _csscsivSnapshotId (\ s a -> s{_csscsivSnapshotId = a})

-- | Undocumented member.
csscsivGatewayARN :: Lens' CreateStorediSCSIVolume Text
csscsivGatewayARN = lens _csscsivGatewayARN (\ s a -> s{_csscsivGatewayARN = a})

-- | The unique identifier for the gateway local disk that is configured as a stored volume. Use <http://docs.aws.amazon.com/storagegateway/latest/userguide/API_ListLocalDisks.html ListLocalDisks> to list disk IDs for a gateway.
csscsivDiskId :: Lens' CreateStorediSCSIVolume Text
csscsivDiskId = lens _csscsivDiskId (\ s a -> s{_csscsivDiskId = a})

-- | Specify this field as true if you want to preserve the data on the local disk. Otherwise, specifying this field as false creates an empty volume. Valid Values: true, false
csscsivPreserveExistingData :: Lens' CreateStorediSCSIVolume Bool
csscsivPreserveExistingData = lens _csscsivPreserveExistingData (\ s a -> s{_csscsivPreserveExistingData = a})

-- | The name of the iSCSI target used by initiators to connect to the target and as a suffix for the target ARN. For example, specifying @TargetName@ as /myvolume/ results in the target ARN of arn:aws:storagegateway:us-east-2:111122223333:gateway/sgw-12A3456B/target/iqn.1997-05.com.amazon:myvolume. The target name must be unique across all volumes of a gateway.
csscsivTargetName :: Lens' CreateStorediSCSIVolume Text
csscsivTargetName = lens _csscsivTargetName (\ s a -> s{_csscsivTargetName = a})

-- | The network interface of the gateway on which to expose the iSCSI target. Only IPv4 addresses are accepted. Use 'DescribeGatewayInformation' to get a list of the network interfaces available on a gateway. Valid Values: A valid IP address.
csscsivNetworkInterfaceId :: Lens' CreateStorediSCSIVolume Text
csscsivNetworkInterfaceId = lens _csscsivNetworkInterfaceId (\ s a -> s{_csscsivNetworkInterfaceId = a})

instance AWSRequest CreateStorediSCSIVolume where
        type Rs CreateStorediSCSIVolume =
             CreateStorediSCSIVolumeResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 CreateStorediSCSIVolumeResponse' <$>
                   (x .?> "TargetARN") <*> (x .?> "VolumeARN") <*>
                     (x .?> "VolumeSizeInBytes")
                     <*> (pure (fromEnum s)))

instance Hashable CreateStorediSCSIVolume where

instance NFData CreateStorediSCSIVolume where

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
              (catMaybes
                 [("SnapshotId" .=) <$> _csscsivSnapshotId,
                  Just ("GatewayARN" .= _csscsivGatewayARN),
                  Just ("DiskId" .= _csscsivDiskId),
                  Just
                    ("PreserveExistingData" .=
                       _csscsivPreserveExistingData),
                  Just ("TargetName" .= _csscsivTargetName),
                  Just
                    ("NetworkInterfaceId" .=
                       _csscsivNetworkInterfaceId)])

instance ToPath CreateStorediSCSIVolume where
        toPath = const "/"

instance ToQuery CreateStorediSCSIVolume where
        toQuery = const mempty

-- | A JSON object containing the following fields:
--
--
--
-- /See:/ 'createStorediSCSIVolumeResponse' smart constructor.
data CreateStorediSCSIVolumeResponse = CreateStorediSCSIVolumeResponse'
  { _csscsivrsTargetARN         :: !(Maybe Text)
  , _csscsivrsVolumeARN         :: !(Maybe Text)
  , _csscsivrsVolumeSizeInBytes :: !(Maybe Integer)
  , _csscsivrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateStorediSCSIVolumeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csscsivrsTargetARN' - he Amazon Resource Name (ARN) of the volume target that includes the iSCSI name that initiators can use to connect to the target.
--
-- * 'csscsivrsVolumeARN' - The Amazon Resource Name (ARN) of the configured volume.
--
-- * 'csscsivrsVolumeSizeInBytes' - The size of the volume in bytes.
--
-- * 'csscsivrsResponseStatus' - -- | The response status code.
createStorediSCSIVolumeResponse
    :: Int -- ^ 'csscsivrsResponseStatus'
    -> CreateStorediSCSIVolumeResponse
createStorediSCSIVolumeResponse pResponseStatus_ =
  CreateStorediSCSIVolumeResponse'
    { _csscsivrsTargetARN = Nothing
    , _csscsivrsVolumeARN = Nothing
    , _csscsivrsVolumeSizeInBytes = Nothing
    , _csscsivrsResponseStatus = pResponseStatus_
    }


-- | he Amazon Resource Name (ARN) of the volume target that includes the iSCSI name that initiators can use to connect to the target.
csscsivrsTargetARN :: Lens' CreateStorediSCSIVolumeResponse (Maybe Text)
csscsivrsTargetARN = lens _csscsivrsTargetARN (\ s a -> s{_csscsivrsTargetARN = a})

-- | The Amazon Resource Name (ARN) of the configured volume.
csscsivrsVolumeARN :: Lens' CreateStorediSCSIVolumeResponse (Maybe Text)
csscsivrsVolumeARN = lens _csscsivrsVolumeARN (\ s a -> s{_csscsivrsVolumeARN = a})

-- | The size of the volume in bytes.
csscsivrsVolumeSizeInBytes :: Lens' CreateStorediSCSIVolumeResponse (Maybe Integer)
csscsivrsVolumeSizeInBytes = lens _csscsivrsVolumeSizeInBytes (\ s a -> s{_csscsivrsVolumeSizeInBytes = a})

-- | -- | The response status code.
csscsivrsResponseStatus :: Lens' CreateStorediSCSIVolumeResponse Int
csscsivrsResponseStatus = lens _csscsivrsResponseStatus (\ s a -> s{_csscsivrsResponseStatus = a})

instance NFData CreateStorediSCSIVolumeResponse where
