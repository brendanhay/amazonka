{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EnableFastSnapshotRestoreErrorItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EnableFastSnapshotRestoreErrorItem where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.EnableFastSnapshotRestoreStateErrorItem
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the errors that occurred when enabling fast snapshot restores.
--
--
--
-- /See:/ 'enableFastSnapshotRestoreErrorItem' smart constructor.
data EnableFastSnapshotRestoreErrorItem = EnableFastSnapshotRestoreErrorItem'
  { _efsreiFastSnapshotRestoreStateErrors ::
      !( Maybe
           [EnableFastSnapshotRestoreStateErrorItem]
       ),
    _efsreiSnapshotId ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EnableFastSnapshotRestoreErrorItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'efsreiFastSnapshotRestoreStateErrors' - The errors.
--
-- * 'efsreiSnapshotId' - The ID of the snapshot.
enableFastSnapshotRestoreErrorItem ::
  EnableFastSnapshotRestoreErrorItem
enableFastSnapshotRestoreErrorItem =
  EnableFastSnapshotRestoreErrorItem'
    { _efsreiFastSnapshotRestoreStateErrors =
        Nothing,
      _efsreiSnapshotId = Nothing
    }

-- | The errors.
efsreiFastSnapshotRestoreStateErrors :: Lens' EnableFastSnapshotRestoreErrorItem [EnableFastSnapshotRestoreStateErrorItem]
efsreiFastSnapshotRestoreStateErrors = lens _efsreiFastSnapshotRestoreStateErrors (\s a -> s {_efsreiFastSnapshotRestoreStateErrors = a}) . _Default . _Coerce

-- | The ID of the snapshot.
efsreiSnapshotId :: Lens' EnableFastSnapshotRestoreErrorItem (Maybe Text)
efsreiSnapshotId = lens _efsreiSnapshotId (\s a -> s {_efsreiSnapshotId = a})

instance FromXML EnableFastSnapshotRestoreErrorItem where
  parseXML x =
    EnableFastSnapshotRestoreErrorItem'
      <$> ( x .@? "fastSnapshotRestoreStateErrorSet" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> (x .@? "snapshotId")

instance Hashable EnableFastSnapshotRestoreErrorItem

instance NFData EnableFastSnapshotRestoreErrorItem
