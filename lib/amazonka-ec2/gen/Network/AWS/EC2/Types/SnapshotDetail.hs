{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SnapshotDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SnapshotDetail where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.UserBucketDetails
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the snapshot created from the imported disk.
--
--
--
-- /See:/ 'snapshotDetail' smart constructor.
data SnapshotDetail = SnapshotDetail'
  { _sdStatus :: !(Maybe Text),
    _sdProgress :: !(Maybe Text),
    _sdFormat :: !(Maybe Text),
    _sdURL :: !(Maybe Text),
    _sdDeviceName :: !(Maybe Text),
    _sdStatusMessage :: !(Maybe Text),
    _sdUserBucket :: !(Maybe UserBucketDetails),
    _sdDiskImageSize :: !(Maybe Double),
    _sdDescription :: !(Maybe Text),
    _sdSnapshotId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SnapshotDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdStatus' - A brief status of the snapshot creation.
--
-- * 'sdProgress' - The percentage of progress for the task.
--
-- * 'sdFormat' - The format of the disk image from which the snapshot is created.
--
-- * 'sdURL' - The URL used to access the disk image.
--
-- * 'sdDeviceName' - The block device mapping for the snapshot.
--
-- * 'sdStatusMessage' - A detailed status message for the snapshot creation.
--
-- * 'sdUserBucket' - The Amazon S3 bucket for the disk image.
--
-- * 'sdDiskImageSize' - The size of the disk in the snapshot, in GiB.
--
-- * 'sdDescription' - A description for the snapshot.
--
-- * 'sdSnapshotId' - The snapshot ID of the disk being imported.
snapshotDetail ::
  SnapshotDetail
snapshotDetail =
  SnapshotDetail'
    { _sdStatus = Nothing,
      _sdProgress = Nothing,
      _sdFormat = Nothing,
      _sdURL = Nothing,
      _sdDeviceName = Nothing,
      _sdStatusMessage = Nothing,
      _sdUserBucket = Nothing,
      _sdDiskImageSize = Nothing,
      _sdDescription = Nothing,
      _sdSnapshotId = Nothing
    }

-- | A brief status of the snapshot creation.
sdStatus :: Lens' SnapshotDetail (Maybe Text)
sdStatus = lens _sdStatus (\s a -> s {_sdStatus = a})

-- | The percentage of progress for the task.
sdProgress :: Lens' SnapshotDetail (Maybe Text)
sdProgress = lens _sdProgress (\s a -> s {_sdProgress = a})

-- | The format of the disk image from which the snapshot is created.
sdFormat :: Lens' SnapshotDetail (Maybe Text)
sdFormat = lens _sdFormat (\s a -> s {_sdFormat = a})

-- | The URL used to access the disk image.
sdURL :: Lens' SnapshotDetail (Maybe Text)
sdURL = lens _sdURL (\s a -> s {_sdURL = a})

-- | The block device mapping for the snapshot.
sdDeviceName :: Lens' SnapshotDetail (Maybe Text)
sdDeviceName = lens _sdDeviceName (\s a -> s {_sdDeviceName = a})

-- | A detailed status message for the snapshot creation.
sdStatusMessage :: Lens' SnapshotDetail (Maybe Text)
sdStatusMessage = lens _sdStatusMessage (\s a -> s {_sdStatusMessage = a})

-- | The Amazon S3 bucket for the disk image.
sdUserBucket :: Lens' SnapshotDetail (Maybe UserBucketDetails)
sdUserBucket = lens _sdUserBucket (\s a -> s {_sdUserBucket = a})

-- | The size of the disk in the snapshot, in GiB.
sdDiskImageSize :: Lens' SnapshotDetail (Maybe Double)
sdDiskImageSize = lens _sdDiskImageSize (\s a -> s {_sdDiskImageSize = a})

-- | A description for the snapshot.
sdDescription :: Lens' SnapshotDetail (Maybe Text)
sdDescription = lens _sdDescription (\s a -> s {_sdDescription = a})

-- | The snapshot ID of the disk being imported.
sdSnapshotId :: Lens' SnapshotDetail (Maybe Text)
sdSnapshotId = lens _sdSnapshotId (\s a -> s {_sdSnapshotId = a})

instance FromXML SnapshotDetail where
  parseXML x =
    SnapshotDetail'
      <$> (x .@? "status")
      <*> (x .@? "progress")
      <*> (x .@? "format")
      <*> (x .@? "url")
      <*> (x .@? "deviceName")
      <*> (x .@? "statusMessage")
      <*> (x .@? "userBucket")
      <*> (x .@? "diskImageSize")
      <*> (x .@? "description")
      <*> (x .@? "snapshotId")

instance Hashable SnapshotDetail

instance NFData SnapshotDetail
