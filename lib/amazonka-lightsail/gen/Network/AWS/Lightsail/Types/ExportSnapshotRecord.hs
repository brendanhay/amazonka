{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ExportSnapshotRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ExportSnapshotRecord where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.DestinationInfo
import Network.AWS.Lightsail.Types.ExportSnapshotRecordSourceInfo
import Network.AWS.Lightsail.Types.RecordState
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Prelude

-- | Describes an export snapshot record.
--
--
--
-- /See:/ 'exportSnapshotRecord' smart constructor.
data ExportSnapshotRecord = ExportSnapshotRecord'
  { _esrState ::
      !(Maybe RecordState),
    _esrDestinationInfo :: !(Maybe DestinationInfo),
    _esrResourceType :: !(Maybe ResourceType),
    _esrArn :: !(Maybe Text),
    _esrCreatedAt :: !(Maybe POSIX),
    _esrLocation :: !(Maybe ResourceLocation),
    _esrName :: !(Maybe Text),
    _esrSourceInfo ::
      !(Maybe ExportSnapshotRecordSourceInfo)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExportSnapshotRecord' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esrState' - The state of the export snapshot record.
--
-- * 'esrDestinationInfo' - A list of objects describing the destination of the export snapshot record.
--
-- * 'esrResourceType' - The Lightsail resource type (e.g., @ExportSnapshotRecord@ ).
--
-- * 'esrArn' - The Amazon Resource Name (ARN) of the export snapshot record.
--
-- * 'esrCreatedAt' - The date when the export snapshot record was created.
--
-- * 'esrLocation' - The AWS Region and Availability Zone where the export snapshot record is located.
--
-- * 'esrName' - The export snapshot record name.
--
-- * 'esrSourceInfo' - A list of objects describing the source of the export snapshot record.
exportSnapshotRecord ::
  ExportSnapshotRecord
exportSnapshotRecord =
  ExportSnapshotRecord'
    { _esrState = Nothing,
      _esrDestinationInfo = Nothing,
      _esrResourceType = Nothing,
      _esrArn = Nothing,
      _esrCreatedAt = Nothing,
      _esrLocation = Nothing,
      _esrName = Nothing,
      _esrSourceInfo = Nothing
    }

-- | The state of the export snapshot record.
esrState :: Lens' ExportSnapshotRecord (Maybe RecordState)
esrState = lens _esrState (\s a -> s {_esrState = a})

-- | A list of objects describing the destination of the export snapshot record.
esrDestinationInfo :: Lens' ExportSnapshotRecord (Maybe DestinationInfo)
esrDestinationInfo = lens _esrDestinationInfo (\s a -> s {_esrDestinationInfo = a})

-- | The Lightsail resource type (e.g., @ExportSnapshotRecord@ ).
esrResourceType :: Lens' ExportSnapshotRecord (Maybe ResourceType)
esrResourceType = lens _esrResourceType (\s a -> s {_esrResourceType = a})

-- | The Amazon Resource Name (ARN) of the export snapshot record.
esrArn :: Lens' ExportSnapshotRecord (Maybe Text)
esrArn = lens _esrArn (\s a -> s {_esrArn = a})

-- | The date when the export snapshot record was created.
esrCreatedAt :: Lens' ExportSnapshotRecord (Maybe UTCTime)
esrCreatedAt = lens _esrCreatedAt (\s a -> s {_esrCreatedAt = a}) . mapping _Time

-- | The AWS Region and Availability Zone where the export snapshot record is located.
esrLocation :: Lens' ExportSnapshotRecord (Maybe ResourceLocation)
esrLocation = lens _esrLocation (\s a -> s {_esrLocation = a})

-- | The export snapshot record name.
esrName :: Lens' ExportSnapshotRecord (Maybe Text)
esrName = lens _esrName (\s a -> s {_esrName = a})

-- | A list of objects describing the source of the export snapshot record.
esrSourceInfo :: Lens' ExportSnapshotRecord (Maybe ExportSnapshotRecordSourceInfo)
esrSourceInfo = lens _esrSourceInfo (\s a -> s {_esrSourceInfo = a})

instance FromJSON ExportSnapshotRecord where
  parseJSON =
    withObject
      "ExportSnapshotRecord"
      ( \x ->
          ExportSnapshotRecord'
            <$> (x .:? "state")
            <*> (x .:? "destinationInfo")
            <*> (x .:? "resourceType")
            <*> (x .:? "arn")
            <*> (x .:? "createdAt")
            <*> (x .:? "location")
            <*> (x .:? "name")
            <*> (x .:? "sourceInfo")
      )

instance Hashable ExportSnapshotRecord

instance NFData ExportSnapshotRecord
