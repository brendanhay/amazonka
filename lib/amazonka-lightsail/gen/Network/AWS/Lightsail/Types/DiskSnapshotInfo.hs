{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.DiskSnapshotInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.DiskSnapshotInfo where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a disk snapshot.
--
--
--
-- /See:/ 'diskSnapshotInfo' smart constructor.
newtype DiskSnapshotInfo = DiskSnapshotInfo'
  { _dsiSizeInGb ::
      Maybe Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DiskSnapshotInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsiSizeInGb' - The size of the disk in GB (e.g., @32@ ).
diskSnapshotInfo ::
  DiskSnapshotInfo
diskSnapshotInfo = DiskSnapshotInfo' {_dsiSizeInGb = Nothing}

-- | The size of the disk in GB (e.g., @32@ ).
dsiSizeInGb :: Lens' DiskSnapshotInfo (Maybe Int)
dsiSizeInGb = lens _dsiSizeInGb (\s a -> s {_dsiSizeInGb = a})

instance FromJSON DiskSnapshotInfo where
  parseJSON =
    withObject
      "DiskSnapshotInfo"
      (\x -> DiskSnapshotInfo' <$> (x .:? "sizeInGb"))

instance Hashable DiskSnapshotInfo

instance NFData DiskSnapshotInfo
