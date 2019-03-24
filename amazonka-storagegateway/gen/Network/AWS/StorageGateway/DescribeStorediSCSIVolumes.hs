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
-- Module      : Network.AWS.StorageGateway.DescribeStorediSCSIVolumes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the description of the gateway volumes specified in the request. The list of gateway volumes in the request must be from one gateway. In the response Amazon Storage Gateway returns volume information sorted by volume ARNs. This operation is only supported in stored volume gateway type.
--
--
module Network.AWS.StorageGateway.DescribeStorediSCSIVolumes
    (
    -- * Creating a Request
      describeStorediSCSIVolumes
    , DescribeStorediSCSIVolumes
    -- * Request Lenses
    , dsscsivVolumeARNs

    -- * Destructuring the Response
    , describeStorediSCSIVolumesResponse
    , DescribeStorediSCSIVolumesResponse
    -- * Response Lenses
    , dsscsivrsStorediSCSIVolumes
    , dsscsivrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.Types.Product

-- | A JSON object containing a list of 'DescribeStorediSCSIVolumesInput$VolumeARNs' .
--
--
--
-- /See:/ 'describeStorediSCSIVolumes' smart constructor.
newtype DescribeStorediSCSIVolumes = DescribeStorediSCSIVolumes'
  { _dsscsivVolumeARNs :: [Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeStorediSCSIVolumes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsscsivVolumeARNs' - An array of strings where each string represents the Amazon Resource Name (ARN) of a stored volume. All of the specified stored volumes must from the same gateway. Use 'ListVolumes' to get volume ARNs for a gateway.
describeStorediSCSIVolumes
    :: DescribeStorediSCSIVolumes
describeStorediSCSIVolumes =
  DescribeStorediSCSIVolumes' {_dsscsivVolumeARNs = mempty}


-- | An array of strings where each string represents the Amazon Resource Name (ARN) of a stored volume. All of the specified stored volumes must from the same gateway. Use 'ListVolumes' to get volume ARNs for a gateway.
dsscsivVolumeARNs :: Lens' DescribeStorediSCSIVolumes [Text]
dsscsivVolumeARNs = lens _dsscsivVolumeARNs (\ s a -> s{_dsscsivVolumeARNs = a}) . _Coerce

instance AWSRequest DescribeStorediSCSIVolumes where
        type Rs DescribeStorediSCSIVolumes =
             DescribeStorediSCSIVolumesResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 DescribeStorediSCSIVolumesResponse' <$>
                   (x .?> "StorediSCSIVolumes" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DescribeStorediSCSIVolumes where

instance NFData DescribeStorediSCSIVolumes where

instance ToHeaders DescribeStorediSCSIVolumes where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.DescribeStorediSCSIVolumes"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeStorediSCSIVolumes where
        toJSON DescribeStorediSCSIVolumes'{..}
          = object
              (catMaybes
                 [Just ("VolumeARNs" .= _dsscsivVolumeARNs)])

instance ToPath DescribeStorediSCSIVolumes where
        toPath = const "/"

instance ToQuery DescribeStorediSCSIVolumes where
        toQuery = const mempty

-- | /See:/ 'describeStorediSCSIVolumesResponse' smart constructor.
data DescribeStorediSCSIVolumesResponse = DescribeStorediSCSIVolumesResponse'
  { _dsscsivrsStorediSCSIVolumes :: !(Maybe [StorediSCSIVolume])
  , _dsscsivrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeStorediSCSIVolumesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsscsivrsStorediSCSIVolumes' - Describes a single unit of output from 'DescribeStorediSCSIVolumes' . The following fields are returned:     * __ChapEnabled__ : Indicates whether mutual CHAP is enabled for the iSCSI target.     * __LunNumber__ : The logical disk number.     * __NetworkInterfaceId__ : The network interface ID of the stored volume that initiator use to map the stored volume as an iSCSI target.     * __NetworkInterfacePort__ : The port used to communicate with iSCSI targets.     * __PreservedExistingData__ : Indicates if when the stored volume was created, existing data on the underlying local disk was preserved.     * __SourceSnapshotId__ : If the stored volume was created from a snapshot, this field contains the snapshot ID used, e.g. snap-1122aabb. Otherwise, this field is not included.     * __StorediSCSIVolumes__ : An array of StorediSCSIVolume objects where each object contains metadata about one stored volume.     * __TargetARN__ : The Amazon Resource Name (ARN) of the volume target.     * __VolumeARN__ : The Amazon Resource Name (ARN) of the stored volume.     * __VolumeDiskId__ : The disk ID of the local disk that was specified in the 'CreateStorediSCSIVolume' operation.     * __VolumeId__ : The unique identifier of the storage volume, e.g. vol-1122AABB.     * __VolumeiSCSIAttributes__ : An 'VolumeiSCSIAttributes' object that represents a collection of iSCSI attributes for one stored volume.     * __VolumeProgress__ : Represents the percentage complete if the volume is restoring or bootstrapping that represents the percent of data transferred. This field does not appear in the response if the stored volume is not restoring or bootstrapping.     * __VolumeSizeInBytes__ : The size of the volume in bytes.     * __VolumeStatus__ : One of the @VolumeStatus@ values that indicates the state of the volume.     * __VolumeType__ : One of the enumeration values describing the type of the volume. Currently, on STORED volumes are supported.
--
-- * 'dsscsivrsResponseStatus' - -- | The response status code.
describeStorediSCSIVolumesResponse
    :: Int -- ^ 'dsscsivrsResponseStatus'
    -> DescribeStorediSCSIVolumesResponse
describeStorediSCSIVolumesResponse pResponseStatus_ =
  DescribeStorediSCSIVolumesResponse'
    { _dsscsivrsStorediSCSIVolumes = Nothing
    , _dsscsivrsResponseStatus = pResponseStatus_
    }


-- | Describes a single unit of output from 'DescribeStorediSCSIVolumes' . The following fields are returned:     * __ChapEnabled__ : Indicates whether mutual CHAP is enabled for the iSCSI target.     * __LunNumber__ : The logical disk number.     * __NetworkInterfaceId__ : The network interface ID of the stored volume that initiator use to map the stored volume as an iSCSI target.     * __NetworkInterfacePort__ : The port used to communicate with iSCSI targets.     * __PreservedExistingData__ : Indicates if when the stored volume was created, existing data on the underlying local disk was preserved.     * __SourceSnapshotId__ : If the stored volume was created from a snapshot, this field contains the snapshot ID used, e.g. snap-1122aabb. Otherwise, this field is not included.     * __StorediSCSIVolumes__ : An array of StorediSCSIVolume objects where each object contains metadata about one stored volume.     * __TargetARN__ : The Amazon Resource Name (ARN) of the volume target.     * __VolumeARN__ : The Amazon Resource Name (ARN) of the stored volume.     * __VolumeDiskId__ : The disk ID of the local disk that was specified in the 'CreateStorediSCSIVolume' operation.     * __VolumeId__ : The unique identifier of the storage volume, e.g. vol-1122AABB.     * __VolumeiSCSIAttributes__ : An 'VolumeiSCSIAttributes' object that represents a collection of iSCSI attributes for one stored volume.     * __VolumeProgress__ : Represents the percentage complete if the volume is restoring or bootstrapping that represents the percent of data transferred. This field does not appear in the response if the stored volume is not restoring or bootstrapping.     * __VolumeSizeInBytes__ : The size of the volume in bytes.     * __VolumeStatus__ : One of the @VolumeStatus@ values that indicates the state of the volume.     * __VolumeType__ : One of the enumeration values describing the type of the volume. Currently, on STORED volumes are supported.
dsscsivrsStorediSCSIVolumes :: Lens' DescribeStorediSCSIVolumesResponse [StorediSCSIVolume]
dsscsivrsStorediSCSIVolumes = lens _dsscsivrsStorediSCSIVolumes (\ s a -> s{_dsscsivrsStorediSCSIVolumes = a}) . _Default . _Coerce

-- | -- | The response status code.
dsscsivrsResponseStatus :: Lens' DescribeStorediSCSIVolumesResponse Int
dsscsivrsResponseStatus = lens _dsscsivrsResponseStatus (\ s a -> s{_dsscsivrsResponseStatus = a})

instance NFData DescribeStorediSCSIVolumesResponse
         where
