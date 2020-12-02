{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DisableFastSnapshotRestoreErrorItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DisableFastSnapshotRestoreErrorItem where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.DisableFastSnapshotRestoreStateErrorItem
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the errors that occurred when disabling fast snapshot restores.
--
--
--
-- /See:/ 'disableFastSnapshotRestoreErrorItem' smart constructor.
data DisableFastSnapshotRestoreErrorItem = DisableFastSnapshotRestoreErrorItem'
  { _dfsreiFastSnapshotRestoreStateErrors ::
      !( Maybe
           [DisableFastSnapshotRestoreStateErrorItem]
       ),
    _dfsreiSnapshotId ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DisableFastSnapshotRestoreErrorItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfsreiFastSnapshotRestoreStateErrors' - The errors.
--
-- * 'dfsreiSnapshotId' - The ID of the snapshot.
disableFastSnapshotRestoreErrorItem ::
  DisableFastSnapshotRestoreErrorItem
disableFastSnapshotRestoreErrorItem =
  DisableFastSnapshotRestoreErrorItem'
    { _dfsreiFastSnapshotRestoreStateErrors =
        Nothing,
      _dfsreiSnapshotId = Nothing
    }

-- | The errors.
dfsreiFastSnapshotRestoreStateErrors :: Lens' DisableFastSnapshotRestoreErrorItem [DisableFastSnapshotRestoreStateErrorItem]
dfsreiFastSnapshotRestoreStateErrors = lens _dfsreiFastSnapshotRestoreStateErrors (\s a -> s {_dfsreiFastSnapshotRestoreStateErrors = a}) . _Default . _Coerce

-- | The ID of the snapshot.
dfsreiSnapshotId :: Lens' DisableFastSnapshotRestoreErrorItem (Maybe Text)
dfsreiSnapshotId = lens _dfsreiSnapshotId (\s a -> s {_dfsreiSnapshotId = a})

instance FromXML DisableFastSnapshotRestoreErrorItem where
  parseXML x =
    DisableFastSnapshotRestoreErrorItem'
      <$> ( x .@? "fastSnapshotRestoreStateErrorSet" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> (x .@? "snapshotId")

instance Hashable DisableFastSnapshotRestoreErrorItem

instance NFData DisableFastSnapshotRestoreErrorItem
