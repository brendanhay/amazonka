{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.AddOn
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.AddOn where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an add-on that is enabled for an Amazon Lightsail resource.
--
--
--
-- /See:/ 'addOn' smart constructor.
data AddOn = AddOn'
  { _aoStatus :: !(Maybe Text),
    _aoNextSnapshotTimeOfDay :: !(Maybe Text),
    _aoSnapshotTimeOfDay :: !(Maybe Text),
    _aoName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AddOn' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aoStatus' - The status of the add-on.
--
-- * 'aoNextSnapshotTimeOfDay' - The next daily time an automatic snapshot will be created. The time shown is in @HH:00@ format, and in Coordinated Universal Time (UTC). The snapshot is automatically created between the time shown and up to 45 minutes after.
--
-- * 'aoSnapshotTimeOfDay' - The daily time when an automatic snapshot is created. The time shown is in @HH:00@ format, and in Coordinated Universal Time (UTC). The snapshot is automatically created between the time shown and up to 45 minutes after.
--
-- * 'aoName' - The name of the add-on.
addOn ::
  AddOn
addOn =
  AddOn'
    { _aoStatus = Nothing,
      _aoNextSnapshotTimeOfDay = Nothing,
      _aoSnapshotTimeOfDay = Nothing,
      _aoName = Nothing
    }

-- | The status of the add-on.
aoStatus :: Lens' AddOn (Maybe Text)
aoStatus = lens _aoStatus (\s a -> s {_aoStatus = a})

-- | The next daily time an automatic snapshot will be created. The time shown is in @HH:00@ format, and in Coordinated Universal Time (UTC). The snapshot is automatically created between the time shown and up to 45 minutes after.
aoNextSnapshotTimeOfDay :: Lens' AddOn (Maybe Text)
aoNextSnapshotTimeOfDay = lens _aoNextSnapshotTimeOfDay (\s a -> s {_aoNextSnapshotTimeOfDay = a})

-- | The daily time when an automatic snapshot is created. The time shown is in @HH:00@ format, and in Coordinated Universal Time (UTC). The snapshot is automatically created between the time shown and up to 45 minutes after.
aoSnapshotTimeOfDay :: Lens' AddOn (Maybe Text)
aoSnapshotTimeOfDay = lens _aoSnapshotTimeOfDay (\s a -> s {_aoSnapshotTimeOfDay = a})

-- | The name of the add-on.
aoName :: Lens' AddOn (Maybe Text)
aoName = lens _aoName (\s a -> s {_aoName = a})

instance FromJSON AddOn where
  parseJSON =
    withObject
      "AddOn"
      ( \x ->
          AddOn'
            <$> (x .:? "status")
            <*> (x .:? "nextSnapshotTimeOfDay")
            <*> (x .:? "snapshotTimeOfDay")
            <*> (x .:? "name")
      )

instance Hashable AddOn

instance NFData AddOn
