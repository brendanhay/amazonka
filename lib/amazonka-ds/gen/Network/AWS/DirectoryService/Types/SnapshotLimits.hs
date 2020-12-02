{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.SnapshotLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.SnapshotLimits where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains manual snapshot limit information for a directory.
--
--
--
-- /See:/ 'snapshotLimits' smart constructor.
data SnapshotLimits = SnapshotLimits'
  { _slManualSnapshotsLimitReached ::
      !(Maybe Bool),
    _slManualSnapshotsCurrentCount :: !(Maybe Nat),
    _slManualSnapshotsLimit :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SnapshotLimits' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slManualSnapshotsLimitReached' - Indicates if the manual snapshot limit has been reached.
--
-- * 'slManualSnapshotsCurrentCount' - The current number of manual snapshots of the directory.
--
-- * 'slManualSnapshotsLimit' - The maximum number of manual snapshots allowed.
snapshotLimits ::
  SnapshotLimits
snapshotLimits =
  SnapshotLimits'
    { _slManualSnapshotsLimitReached = Nothing,
      _slManualSnapshotsCurrentCount = Nothing,
      _slManualSnapshotsLimit = Nothing
    }

-- | Indicates if the manual snapshot limit has been reached.
slManualSnapshotsLimitReached :: Lens' SnapshotLimits (Maybe Bool)
slManualSnapshotsLimitReached = lens _slManualSnapshotsLimitReached (\s a -> s {_slManualSnapshotsLimitReached = a})

-- | The current number of manual snapshots of the directory.
slManualSnapshotsCurrentCount :: Lens' SnapshotLimits (Maybe Natural)
slManualSnapshotsCurrentCount = lens _slManualSnapshotsCurrentCount (\s a -> s {_slManualSnapshotsCurrentCount = a}) . mapping _Nat

-- | The maximum number of manual snapshots allowed.
slManualSnapshotsLimit :: Lens' SnapshotLimits (Maybe Natural)
slManualSnapshotsLimit = lens _slManualSnapshotsLimit (\s a -> s {_slManualSnapshotsLimit = a}) . mapping _Nat

instance FromJSON SnapshotLimits where
  parseJSON =
    withObject
      "SnapshotLimits"
      ( \x ->
          SnapshotLimits'
            <$> (x .:? "ManualSnapshotsLimitReached")
            <*> (x .:? "ManualSnapshotsCurrentCount")
            <*> (x .:? "ManualSnapshotsLimit")
      )

instance Hashable SnapshotLimits

instance NFData SnapshotLimits
