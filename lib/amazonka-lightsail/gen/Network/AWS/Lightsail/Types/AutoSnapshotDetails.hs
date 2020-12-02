{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.AutoSnapshotDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.AutoSnapshotDetails where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.AttachedDisk
import Network.AWS.Lightsail.Types.AutoSnapshotStatus
import Network.AWS.Prelude

-- | Describes an automatic snapshot.
--
--
--
-- /See:/ 'autoSnapshotDetails' smart constructor.
data AutoSnapshotDetails = AutoSnapshotDetails'
  { _asdStatus ::
      !(Maybe AutoSnapshotStatus),
    _asdFromAttachedDisks :: !(Maybe [AttachedDisk]),
    _asdCreatedAt :: !(Maybe POSIX),
    _asdDate :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AutoSnapshotDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asdStatus' - The status of the automatic snapshot.
--
-- * 'asdFromAttachedDisks' - An array of objects that describe the block storage disks attached to the instance when the automatic snapshot was created.
--
-- * 'asdCreatedAt' - The timestamp when the automatic snapshot was created.
--
-- * 'asdDate' - The date of the automatic snapshot in @YYYY-MM-DD@ format.
autoSnapshotDetails ::
  AutoSnapshotDetails
autoSnapshotDetails =
  AutoSnapshotDetails'
    { _asdStatus = Nothing,
      _asdFromAttachedDisks = Nothing,
      _asdCreatedAt = Nothing,
      _asdDate = Nothing
    }

-- | The status of the automatic snapshot.
asdStatus :: Lens' AutoSnapshotDetails (Maybe AutoSnapshotStatus)
asdStatus = lens _asdStatus (\s a -> s {_asdStatus = a})

-- | An array of objects that describe the block storage disks attached to the instance when the automatic snapshot was created.
asdFromAttachedDisks :: Lens' AutoSnapshotDetails [AttachedDisk]
asdFromAttachedDisks = lens _asdFromAttachedDisks (\s a -> s {_asdFromAttachedDisks = a}) . _Default . _Coerce

-- | The timestamp when the automatic snapshot was created.
asdCreatedAt :: Lens' AutoSnapshotDetails (Maybe UTCTime)
asdCreatedAt = lens _asdCreatedAt (\s a -> s {_asdCreatedAt = a}) . mapping _Time

-- | The date of the automatic snapshot in @YYYY-MM-DD@ format.
asdDate :: Lens' AutoSnapshotDetails (Maybe Text)
asdDate = lens _asdDate (\s a -> s {_asdDate = a})

instance FromJSON AutoSnapshotDetails where
  parseJSON =
    withObject
      "AutoSnapshotDetails"
      ( \x ->
          AutoSnapshotDetails'
            <$> (x .:? "status")
            <*> (x .:? "fromAttachedDisks" .!= mempty)
            <*> (x .:? "createdAt")
            <*> (x .:? "date")
      )

instance Hashable AutoSnapshotDetails

instance NFData AutoSnapshotDetails
