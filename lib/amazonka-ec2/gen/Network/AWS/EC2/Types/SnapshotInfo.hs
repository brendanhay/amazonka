{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SnapshotInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SnapshotInfo where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.SnapshotState
import Network.AWS.EC2.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a snapshot.
--
--
--
-- /See:/ 'snapshotInfo' smart constructor.
data SnapshotInfo = SnapshotInfo'
  { _siState ::
      !(Maybe SnapshotState),
    _siProgress :: !(Maybe Text),
    _siStartTime :: !(Maybe ISO8601),
    _siVolumeSize :: !(Maybe Int),
    _siEncrypted :: !(Maybe Bool),
    _siOwnerId :: !(Maybe Text),
    _siVolumeId :: !(Maybe Text),
    _siDescription :: !(Maybe Text),
    _siTags :: !(Maybe [Tag]),
    _siSnapshotId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SnapshotInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siState' - Current state of the snapshot.
--
-- * 'siProgress' - Progress this snapshot has made towards completing.
--
-- * 'siStartTime' - Time this snapshot was started. This is the same for all snapshots initiated by the same request.
--
-- * 'siVolumeSize' - Size of the volume from which this snapshot was created.
--
-- * 'siEncrypted' - Indicates whether the snapshot is encrypted.
--
-- * 'siOwnerId' - Account id used when creating this snapshot.
--
-- * 'siVolumeId' - Source volume from which this snapshot was created.
--
-- * 'siDescription' - Description specified by the CreateSnapshotRequest that has been applied to all snapshots.
--
-- * 'siTags' - Tags associated with this snapshot.
--
-- * 'siSnapshotId' - Snapshot id that can be used to describe this snapshot.
snapshotInfo ::
  SnapshotInfo
snapshotInfo =
  SnapshotInfo'
    { _siState = Nothing,
      _siProgress = Nothing,
      _siStartTime = Nothing,
      _siVolumeSize = Nothing,
      _siEncrypted = Nothing,
      _siOwnerId = Nothing,
      _siVolumeId = Nothing,
      _siDescription = Nothing,
      _siTags = Nothing,
      _siSnapshotId = Nothing
    }

-- | Current state of the snapshot.
siState :: Lens' SnapshotInfo (Maybe SnapshotState)
siState = lens _siState (\s a -> s {_siState = a})

-- | Progress this snapshot has made towards completing.
siProgress :: Lens' SnapshotInfo (Maybe Text)
siProgress = lens _siProgress (\s a -> s {_siProgress = a})

-- | Time this snapshot was started. This is the same for all snapshots initiated by the same request.
siStartTime :: Lens' SnapshotInfo (Maybe UTCTime)
siStartTime = lens _siStartTime (\s a -> s {_siStartTime = a}) . mapping _Time

-- | Size of the volume from which this snapshot was created.
siVolumeSize :: Lens' SnapshotInfo (Maybe Int)
siVolumeSize = lens _siVolumeSize (\s a -> s {_siVolumeSize = a})

-- | Indicates whether the snapshot is encrypted.
siEncrypted :: Lens' SnapshotInfo (Maybe Bool)
siEncrypted = lens _siEncrypted (\s a -> s {_siEncrypted = a})

-- | Account id used when creating this snapshot.
siOwnerId :: Lens' SnapshotInfo (Maybe Text)
siOwnerId = lens _siOwnerId (\s a -> s {_siOwnerId = a})

-- | Source volume from which this snapshot was created.
siVolumeId :: Lens' SnapshotInfo (Maybe Text)
siVolumeId = lens _siVolumeId (\s a -> s {_siVolumeId = a})

-- | Description specified by the CreateSnapshotRequest that has been applied to all snapshots.
siDescription :: Lens' SnapshotInfo (Maybe Text)
siDescription = lens _siDescription (\s a -> s {_siDescription = a})

-- | Tags associated with this snapshot.
siTags :: Lens' SnapshotInfo [Tag]
siTags = lens _siTags (\s a -> s {_siTags = a}) . _Default . _Coerce

-- | Snapshot id that can be used to describe this snapshot.
siSnapshotId :: Lens' SnapshotInfo (Maybe Text)
siSnapshotId = lens _siSnapshotId (\s a -> s {_siSnapshotId = a})

instance FromXML SnapshotInfo where
  parseXML x =
    SnapshotInfo'
      <$> (x .@? "state")
      <*> (x .@? "progress")
      <*> (x .@? "startTime")
      <*> (x .@? "volumeSize")
      <*> (x .@? "encrypted")
      <*> (x .@? "ownerId")
      <*> (x .@? "volumeId")
      <*> (x .@? "description")
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "snapshotId")

instance Hashable SnapshotInfo

instance NFData SnapshotInfo
