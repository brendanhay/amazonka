{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.DiskMap
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.DiskMap where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a block storage disk mapping.
--
--
--
-- /See:/ 'diskMap' smart constructor.
data DiskMap = DiskMap'
  { _dmNewDiskName :: !(Maybe Text),
    _dmOriginalDiskPath :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DiskMap' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmNewDiskName' - The new disk name (e.g., @my-new-disk@ ).
--
-- * 'dmOriginalDiskPath' - The original disk path exposed to the instance (for example, @/dev/sdh@ ).
diskMap ::
  DiskMap
diskMap =
  DiskMap' {_dmNewDiskName = Nothing, _dmOriginalDiskPath = Nothing}

-- | The new disk name (e.g., @my-new-disk@ ).
dmNewDiskName :: Lens' DiskMap (Maybe Text)
dmNewDiskName = lens _dmNewDiskName (\s a -> s {_dmNewDiskName = a})

-- | The original disk path exposed to the instance (for example, @/dev/sdh@ ).
dmOriginalDiskPath :: Lens' DiskMap (Maybe Text)
dmOriginalDiskPath = lens _dmOriginalDiskPath (\s a -> s {_dmOriginalDiskPath = a})

instance Hashable DiskMap

instance NFData DiskMap

instance ToJSON DiskMap where
  toJSON DiskMap' {..} =
    object
      ( catMaybes
          [ ("newDiskName" .=) <$> _dmNewDiskName,
            ("originalDiskPath" .=) <$> _dmOriginalDiskPath
          ]
      )
