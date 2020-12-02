{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.VolumeInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.VolumeInfo where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a storage volume object.
--
--
--
-- /See:/ 'volumeInfo' smart constructor.
data VolumeInfo = VolumeInfo'
  { _viGatewayARN :: !(Maybe Text),
    _viVolumeAttachmentStatus :: !(Maybe Text),
    _viVolumeARN :: !(Maybe Text),
    _viVolumeSizeInBytes :: !(Maybe Integer),
    _viVolumeId :: !(Maybe Text),
    _viGatewayId :: !(Maybe Text),
    _viVolumeType :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VolumeInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'viGatewayARN' - Undocumented member.
--
-- * 'viVolumeAttachmentStatus' - One of the VolumeStatus values that indicates the state of the storage volume.
--
-- * 'viVolumeARN' - The Amazon Resource Name (ARN) for the storage volume. For example, the following is a valid ARN: @arn:aws:storagegateway:us-east-2:111122223333:gateway/sgw-12A3456B/volume/vol-1122AABB@  Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and hyphens (-).
--
-- * 'viVolumeSizeInBytes' - The size of the volume in bytes. Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and hyphens (-).
--
-- * 'viVolumeId' - The unique identifier assigned to the volume. This ID becomes part of the volume Amazon Resource Name (ARN), which you use as input for other operations. Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and hyphens (-).
--
-- * 'viGatewayId' - The unique identifier assigned to your gateway during activation. This ID becomes part of the gateway Amazon Resource Name (ARN), which you use as input for other operations. Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and hyphens (-).
--
-- * 'viVolumeType' - One of the VolumeType enumeration values describing the type of the volume.
volumeInfo ::
  VolumeInfo
volumeInfo =
  VolumeInfo'
    { _viGatewayARN = Nothing,
      _viVolumeAttachmentStatus = Nothing,
      _viVolumeARN = Nothing,
      _viVolumeSizeInBytes = Nothing,
      _viVolumeId = Nothing,
      _viGatewayId = Nothing,
      _viVolumeType = Nothing
    }

-- | Undocumented member.
viGatewayARN :: Lens' VolumeInfo (Maybe Text)
viGatewayARN = lens _viGatewayARN (\s a -> s {_viGatewayARN = a})

-- | One of the VolumeStatus values that indicates the state of the storage volume.
viVolumeAttachmentStatus :: Lens' VolumeInfo (Maybe Text)
viVolumeAttachmentStatus = lens _viVolumeAttachmentStatus (\s a -> s {_viVolumeAttachmentStatus = a})

-- | The Amazon Resource Name (ARN) for the storage volume. For example, the following is a valid ARN: @arn:aws:storagegateway:us-east-2:111122223333:gateway/sgw-12A3456B/volume/vol-1122AABB@  Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and hyphens (-).
viVolumeARN :: Lens' VolumeInfo (Maybe Text)
viVolumeARN = lens _viVolumeARN (\s a -> s {_viVolumeARN = a})

-- | The size of the volume in bytes. Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and hyphens (-).
viVolumeSizeInBytes :: Lens' VolumeInfo (Maybe Integer)
viVolumeSizeInBytes = lens _viVolumeSizeInBytes (\s a -> s {_viVolumeSizeInBytes = a})

-- | The unique identifier assigned to the volume. This ID becomes part of the volume Amazon Resource Name (ARN), which you use as input for other operations. Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and hyphens (-).
viVolumeId :: Lens' VolumeInfo (Maybe Text)
viVolumeId = lens _viVolumeId (\s a -> s {_viVolumeId = a})

-- | The unique identifier assigned to your gateway during activation. This ID becomes part of the gateway Amazon Resource Name (ARN), which you use as input for other operations. Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and hyphens (-).
viGatewayId :: Lens' VolumeInfo (Maybe Text)
viGatewayId = lens _viGatewayId (\s a -> s {_viGatewayId = a})

-- | One of the VolumeType enumeration values describing the type of the volume.
viVolumeType :: Lens' VolumeInfo (Maybe Text)
viVolumeType = lens _viVolumeType (\s a -> s {_viVolumeType = a})

instance FromJSON VolumeInfo where
  parseJSON =
    withObject
      "VolumeInfo"
      ( \x ->
          VolumeInfo'
            <$> (x .:? "GatewayARN")
            <*> (x .:? "VolumeAttachmentStatus")
            <*> (x .:? "VolumeARN")
            <*> (x .:? "VolumeSizeInBytes")
            <*> (x .:? "VolumeId")
            <*> (x .:? "GatewayId")
            <*> (x .:? "VolumeType")
      )

instance Hashable VolumeInfo

instance NFData VolumeInfo
