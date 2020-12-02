{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EnableFastSnapshotRestoreStateErrorItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EnableFastSnapshotRestoreStateErrorItem where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.EnableFastSnapshotRestoreStateError
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about an error that occurred when enabling fast snapshot restores.
--
--
--
-- /See:/ 'enableFastSnapshotRestoreStateErrorItem' smart constructor.
data EnableFastSnapshotRestoreStateErrorItem = EnableFastSnapshotRestoreStateErrorItem'
  { _efsrseiError ::
      !( Maybe
           EnableFastSnapshotRestoreStateError
       ),
    _efsrseiAvailabilityZone ::
      !( Maybe
           Text
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EnableFastSnapshotRestoreStateErrorItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'efsrseiError' - The error.
--
-- * 'efsrseiAvailabilityZone' - The Availability Zone.
enableFastSnapshotRestoreStateErrorItem ::
  EnableFastSnapshotRestoreStateErrorItem
enableFastSnapshotRestoreStateErrorItem =
  EnableFastSnapshotRestoreStateErrorItem'
    { _efsrseiError = Nothing,
      _efsrseiAvailabilityZone = Nothing
    }

-- | The error.
efsrseiError :: Lens' EnableFastSnapshotRestoreStateErrorItem (Maybe EnableFastSnapshotRestoreStateError)
efsrseiError = lens _efsrseiError (\s a -> s {_efsrseiError = a})

-- | The Availability Zone.
efsrseiAvailabilityZone :: Lens' EnableFastSnapshotRestoreStateErrorItem (Maybe Text)
efsrseiAvailabilityZone = lens _efsrseiAvailabilityZone (\s a -> s {_efsrseiAvailabilityZone = a})

instance FromXML EnableFastSnapshotRestoreStateErrorItem where
  parseXML x =
    EnableFastSnapshotRestoreStateErrorItem'
      <$> (x .@? "error") <*> (x .@? "availabilityZone")

instance Hashable EnableFastSnapshotRestoreStateErrorItem

instance NFData EnableFastSnapshotRestoreStateErrorItem
