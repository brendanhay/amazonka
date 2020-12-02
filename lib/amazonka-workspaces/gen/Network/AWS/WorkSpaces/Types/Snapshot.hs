{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.Snapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.Snapshot where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a snapshot.
--
--
--
-- /See:/ 'snapshot' smart constructor.
newtype Snapshot = Snapshot' {_sSnapshotTime :: Maybe POSIX}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Snapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sSnapshotTime' - The time when the snapshot was created.
snapshot ::
  Snapshot
snapshot = Snapshot' {_sSnapshotTime = Nothing}

-- | The time when the snapshot was created.
sSnapshotTime :: Lens' Snapshot (Maybe UTCTime)
sSnapshotTime = lens _sSnapshotTime (\s a -> s {_sSnapshotTime = a}) . mapping _Time

instance FromJSON Snapshot where
  parseJSON =
    withObject
      "Snapshot"
      (\x -> Snapshot' <$> (x .:? "SnapshotTime"))

instance Hashable Snapshot

instance NFData Snapshot
