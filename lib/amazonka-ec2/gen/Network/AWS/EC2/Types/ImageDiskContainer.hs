{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ImageDiskContainer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ImageDiskContainer where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.UserBucket
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the disk container object for an import image task.
--
--
--
-- /See:/ 'imageDiskContainer' smart constructor.
data ImageDiskContainer = ImageDiskContainer'
  { _idcFormat ::
      !(Maybe Text),
    _idcURL :: !(Maybe Text),
    _idcDeviceName :: !(Maybe Text),
    _idcUserBucket :: !(Maybe UserBucket),
    _idcDescription :: !(Maybe Text),
    _idcSnapshotId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ImageDiskContainer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idcFormat' - The format of the disk image being imported. Valid values: @OVA@ | @VHD@ | @VHDX@ |@VMDK@
--
-- * 'idcURL' - The URL to the Amazon S3-based disk image being imported. The URL can either be a https URL (https://..) or an Amazon S3 URL (s3://..)
--
-- * 'idcDeviceName' - The block device mapping for the disk.
--
-- * 'idcUserBucket' - The S3 bucket for the disk image.
--
-- * 'idcDescription' - The description of the disk image.
--
-- * 'idcSnapshotId' - The ID of the EBS snapshot to be used for importing the snapshot.
imageDiskContainer ::
  ImageDiskContainer
imageDiskContainer =
  ImageDiskContainer'
    { _idcFormat = Nothing,
      _idcURL = Nothing,
      _idcDeviceName = Nothing,
      _idcUserBucket = Nothing,
      _idcDescription = Nothing,
      _idcSnapshotId = Nothing
    }

-- | The format of the disk image being imported. Valid values: @OVA@ | @VHD@ | @VHDX@ |@VMDK@
idcFormat :: Lens' ImageDiskContainer (Maybe Text)
idcFormat = lens _idcFormat (\s a -> s {_idcFormat = a})

-- | The URL to the Amazon S3-based disk image being imported. The URL can either be a https URL (https://..) or an Amazon S3 URL (s3://..)
idcURL :: Lens' ImageDiskContainer (Maybe Text)
idcURL = lens _idcURL (\s a -> s {_idcURL = a})

-- | The block device mapping for the disk.
idcDeviceName :: Lens' ImageDiskContainer (Maybe Text)
idcDeviceName = lens _idcDeviceName (\s a -> s {_idcDeviceName = a})

-- | The S3 bucket for the disk image.
idcUserBucket :: Lens' ImageDiskContainer (Maybe UserBucket)
idcUserBucket = lens _idcUserBucket (\s a -> s {_idcUserBucket = a})

-- | The description of the disk image.
idcDescription :: Lens' ImageDiskContainer (Maybe Text)
idcDescription = lens _idcDescription (\s a -> s {_idcDescription = a})

-- | The ID of the EBS snapshot to be used for importing the snapshot.
idcSnapshotId :: Lens' ImageDiskContainer (Maybe Text)
idcSnapshotId = lens _idcSnapshotId (\s a -> s {_idcSnapshotId = a})

instance Hashable ImageDiskContainer

instance NFData ImageDiskContainer

instance ToQuery ImageDiskContainer where
  toQuery ImageDiskContainer' {..} =
    mconcat
      [ "Format" =: _idcFormat,
        "Url" =: _idcURL,
        "DeviceName" =: _idcDeviceName,
        "UserBucket" =: _idcUserBucket,
        "Description" =: _idcDescription,
        "SnapshotId" =: _idcSnapshotId
      ]
