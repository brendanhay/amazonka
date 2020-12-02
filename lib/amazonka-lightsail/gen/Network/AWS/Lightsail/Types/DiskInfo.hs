{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.DiskInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.DiskInfo where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a disk.
--
--
--
-- /See:/ 'diskInfo' smart constructor.
data DiskInfo = DiskInfo'
  { _diPath :: !(Maybe Text),
    _diName :: !(Maybe Text),
    _diSizeInGb :: !(Maybe Int),
    _diIsSystemDisk :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DiskInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diPath' - The disk path.
--
-- * 'diName' - The disk name.
--
-- * 'diSizeInGb' - The size of the disk in GB (e.g., @32@ ).
--
-- * 'diIsSystemDisk' - A Boolean value indicating whether this disk is a system disk (has an operating system loaded on it).
diskInfo ::
  DiskInfo
diskInfo =
  DiskInfo'
    { _diPath = Nothing,
      _diName = Nothing,
      _diSizeInGb = Nothing,
      _diIsSystemDisk = Nothing
    }

-- | The disk path.
diPath :: Lens' DiskInfo (Maybe Text)
diPath = lens _diPath (\s a -> s {_diPath = a})

-- | The disk name.
diName :: Lens' DiskInfo (Maybe Text)
diName = lens _diName (\s a -> s {_diName = a})

-- | The size of the disk in GB (e.g., @32@ ).
diSizeInGb :: Lens' DiskInfo (Maybe Int)
diSizeInGb = lens _diSizeInGb (\s a -> s {_diSizeInGb = a})

-- | A Boolean value indicating whether this disk is a system disk (has an operating system loaded on it).
diIsSystemDisk :: Lens' DiskInfo (Maybe Bool)
diIsSystemDisk = lens _diIsSystemDisk (\s a -> s {_diIsSystemDisk = a})

instance FromJSON DiskInfo where
  parseJSON =
    withObject
      "DiskInfo"
      ( \x ->
          DiskInfo'
            <$> (x .:? "path")
            <*> (x .:? "name")
            <*> (x .:? "sizeInGb")
            <*> (x .:? "isSystemDisk")
      )

instance Hashable DiskInfo

instance NFData DiskInfo
