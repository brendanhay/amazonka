{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ExportSnapshotRecordSourceInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ExportSnapshotRecordSourceInfo where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.DiskSnapshotInfo
import Network.AWS.Lightsail.Types.ExportSnapshotRecordSourceType
import Network.AWS.Lightsail.Types.InstanceSnapshotInfo
import Network.AWS.Prelude

-- | Describes the source of an export snapshot record.
--
--
--
-- /See:/ 'exportSnapshotRecordSourceInfo' smart constructor.
data ExportSnapshotRecordSourceInfo = ExportSnapshotRecordSourceInfo'
  { _esrsiDiskSnapshotInfo ::
      !(Maybe DiskSnapshotInfo),
    _esrsiResourceType ::
      !( Maybe
           ExportSnapshotRecordSourceType
       ),
    _esrsiArn :: !(Maybe Text),
    _esrsiCreatedAt ::
      !(Maybe POSIX),
    _esrsiFromResourceARN ::
      !(Maybe Text),
    _esrsiName :: !(Maybe Text),
    _esrsiInstanceSnapshotInfo ::
      !(Maybe InstanceSnapshotInfo),
    _esrsiFromResourceName ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExportSnapshotRecordSourceInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esrsiDiskSnapshotInfo' - A list of objects describing a disk snapshot.
--
-- * 'esrsiResourceType' - The Lightsail resource type (e.g., @InstanceSnapshot@ or @DiskSnapshot@ ).
--
-- * 'esrsiArn' - The Amazon Resource Name (ARN) of the source instance or disk snapshot.
--
-- * 'esrsiCreatedAt' - The date when the source instance or disk snapshot was created.
--
-- * 'esrsiFromResourceARN' - The Amazon Resource Name (ARN) of the snapshot's source instance or disk.
--
-- * 'esrsiName' - The name of the source instance or disk snapshot.
--
-- * 'esrsiInstanceSnapshotInfo' - A list of objects describing an instance snapshot.
--
-- * 'esrsiFromResourceName' - The name of the snapshot's source instance or disk.
exportSnapshotRecordSourceInfo ::
  ExportSnapshotRecordSourceInfo
exportSnapshotRecordSourceInfo =
  ExportSnapshotRecordSourceInfo'
    { _esrsiDiskSnapshotInfo = Nothing,
      _esrsiResourceType = Nothing,
      _esrsiArn = Nothing,
      _esrsiCreatedAt = Nothing,
      _esrsiFromResourceARN = Nothing,
      _esrsiName = Nothing,
      _esrsiInstanceSnapshotInfo = Nothing,
      _esrsiFromResourceName = Nothing
    }

-- | A list of objects describing a disk snapshot.
esrsiDiskSnapshotInfo :: Lens' ExportSnapshotRecordSourceInfo (Maybe DiskSnapshotInfo)
esrsiDiskSnapshotInfo = lens _esrsiDiskSnapshotInfo (\s a -> s {_esrsiDiskSnapshotInfo = a})

-- | The Lightsail resource type (e.g., @InstanceSnapshot@ or @DiskSnapshot@ ).
esrsiResourceType :: Lens' ExportSnapshotRecordSourceInfo (Maybe ExportSnapshotRecordSourceType)
esrsiResourceType = lens _esrsiResourceType (\s a -> s {_esrsiResourceType = a})

-- | The Amazon Resource Name (ARN) of the source instance or disk snapshot.
esrsiArn :: Lens' ExportSnapshotRecordSourceInfo (Maybe Text)
esrsiArn = lens _esrsiArn (\s a -> s {_esrsiArn = a})

-- | The date when the source instance or disk snapshot was created.
esrsiCreatedAt :: Lens' ExportSnapshotRecordSourceInfo (Maybe UTCTime)
esrsiCreatedAt = lens _esrsiCreatedAt (\s a -> s {_esrsiCreatedAt = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the snapshot's source instance or disk.
esrsiFromResourceARN :: Lens' ExportSnapshotRecordSourceInfo (Maybe Text)
esrsiFromResourceARN = lens _esrsiFromResourceARN (\s a -> s {_esrsiFromResourceARN = a})

-- | The name of the source instance or disk snapshot.
esrsiName :: Lens' ExportSnapshotRecordSourceInfo (Maybe Text)
esrsiName = lens _esrsiName (\s a -> s {_esrsiName = a})

-- | A list of objects describing an instance snapshot.
esrsiInstanceSnapshotInfo :: Lens' ExportSnapshotRecordSourceInfo (Maybe InstanceSnapshotInfo)
esrsiInstanceSnapshotInfo = lens _esrsiInstanceSnapshotInfo (\s a -> s {_esrsiInstanceSnapshotInfo = a})

-- | The name of the snapshot's source instance or disk.
esrsiFromResourceName :: Lens' ExportSnapshotRecordSourceInfo (Maybe Text)
esrsiFromResourceName = lens _esrsiFromResourceName (\s a -> s {_esrsiFromResourceName = a})

instance FromJSON ExportSnapshotRecordSourceInfo where
  parseJSON =
    withObject
      "ExportSnapshotRecordSourceInfo"
      ( \x ->
          ExportSnapshotRecordSourceInfo'
            <$> (x .:? "diskSnapshotInfo")
            <*> (x .:? "resourceType")
            <*> (x .:? "arn")
            <*> (x .:? "createdAt")
            <*> (x .:? "fromResourceArn")
            <*> (x .:? "name")
            <*> (x .:? "instanceSnapshotInfo")
            <*> (x .:? "fromResourceName")
      )

instance Hashable ExportSnapshotRecordSourceInfo

instance NFData ExportSnapshotRecordSourceInfo
