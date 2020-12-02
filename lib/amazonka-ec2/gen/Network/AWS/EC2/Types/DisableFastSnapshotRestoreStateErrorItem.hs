{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DisableFastSnapshotRestoreStateErrorItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DisableFastSnapshotRestoreStateErrorItem where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.DisableFastSnapshotRestoreStateError
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about an error that occurred when disabling fast snapshot restores.
--
--
--
-- /See:/ 'disableFastSnapshotRestoreStateErrorItem' smart constructor.
data DisableFastSnapshotRestoreStateErrorItem = DisableFastSnapshotRestoreStateErrorItem'
  { _dfsrseiError ::
      !( Maybe
           DisableFastSnapshotRestoreStateError
       ),
    _dfsrseiAvailabilityZone ::
      !( Maybe
           Text
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DisableFastSnapshotRestoreStateErrorItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfsrseiError' - The error.
--
-- * 'dfsrseiAvailabilityZone' - The Availability Zone.
disableFastSnapshotRestoreStateErrorItem ::
  DisableFastSnapshotRestoreStateErrorItem
disableFastSnapshotRestoreStateErrorItem =
  DisableFastSnapshotRestoreStateErrorItem'
    { _dfsrseiError =
        Nothing,
      _dfsrseiAvailabilityZone = Nothing
    }

-- | The error.
dfsrseiError :: Lens' DisableFastSnapshotRestoreStateErrorItem (Maybe DisableFastSnapshotRestoreStateError)
dfsrseiError = lens _dfsrseiError (\s a -> s {_dfsrseiError = a})

-- | The Availability Zone.
dfsrseiAvailabilityZone :: Lens' DisableFastSnapshotRestoreStateErrorItem (Maybe Text)
dfsrseiAvailabilityZone = lens _dfsrseiAvailabilityZone (\s a -> s {_dfsrseiAvailabilityZone = a})

instance FromXML DisableFastSnapshotRestoreStateErrorItem where
  parseXML x =
    DisableFastSnapshotRestoreStateErrorItem'
      <$> (x .@? "error") <*> (x .@? "availabilityZone")

instance Hashable DisableFastSnapshotRestoreStateErrorItem

instance NFData DisableFastSnapshotRestoreStateErrorItem
