{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.Snapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.Snapshot where

import Network.AWS.DirectoryService.Types.SnapshotStatus
import Network.AWS.DirectoryService.Types.SnapshotType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a directory snapshot.
--
--
--
-- /See:/ 'snapshot' smart constructor.
data Snapshot = Snapshot'
  { _sStatus :: !(Maybe SnapshotStatus),
    _sDirectoryId :: !(Maybe Text),
    _sStartTime :: !(Maybe POSIX),
    _sName :: !(Maybe Text),
    _sType :: !(Maybe SnapshotType),
    _sSnapshotId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Snapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sStatus' - The snapshot status.
--
-- * 'sDirectoryId' - The directory identifier.
--
-- * 'sStartTime' - The date and time that the snapshot was taken.
--
-- * 'sName' - The descriptive name of the snapshot.
--
-- * 'sType' - The snapshot type.
--
-- * 'sSnapshotId' - The snapshot identifier.
snapshot ::
  Snapshot
snapshot =
  Snapshot'
    { _sStatus = Nothing,
      _sDirectoryId = Nothing,
      _sStartTime = Nothing,
      _sName = Nothing,
      _sType = Nothing,
      _sSnapshotId = Nothing
    }

-- | The snapshot status.
sStatus :: Lens' Snapshot (Maybe SnapshotStatus)
sStatus = lens _sStatus (\s a -> s {_sStatus = a})

-- | The directory identifier.
sDirectoryId :: Lens' Snapshot (Maybe Text)
sDirectoryId = lens _sDirectoryId (\s a -> s {_sDirectoryId = a})

-- | The date and time that the snapshot was taken.
sStartTime :: Lens' Snapshot (Maybe UTCTime)
sStartTime = lens _sStartTime (\s a -> s {_sStartTime = a}) . mapping _Time

-- | The descriptive name of the snapshot.
sName :: Lens' Snapshot (Maybe Text)
sName = lens _sName (\s a -> s {_sName = a})

-- | The snapshot type.
sType :: Lens' Snapshot (Maybe SnapshotType)
sType = lens _sType (\s a -> s {_sType = a})

-- | The snapshot identifier.
sSnapshotId :: Lens' Snapshot (Maybe Text)
sSnapshotId = lens _sSnapshotId (\s a -> s {_sSnapshotId = a})

instance FromJSON Snapshot where
  parseJSON =
    withObject
      "Snapshot"
      ( \x ->
          Snapshot'
            <$> (x .:? "Status")
            <*> (x .:? "DirectoryId")
            <*> (x .:? "StartTime")
            <*> (x .:? "Name")
            <*> (x .:? "Type")
            <*> (x .:? "SnapshotId")
      )

instance Hashable Snapshot

instance NFData Snapshot
