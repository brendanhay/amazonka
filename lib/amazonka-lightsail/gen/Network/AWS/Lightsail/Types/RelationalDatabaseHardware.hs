{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.RelationalDatabaseHardware
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RelationalDatabaseHardware where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the hardware of a database.
--
--
--
-- /See:/ 'relationalDatabaseHardware' smart constructor.
data RelationalDatabaseHardware = RelationalDatabaseHardware'
  { _rdhCpuCount ::
      !(Maybe Int),
    _rdhDiskSizeInGb :: !(Maybe Int),
    _rdhRamSizeInGb :: !(Maybe Double)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RelationalDatabaseHardware' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdhCpuCount' - The number of vCPUs for the database.
--
-- * 'rdhDiskSizeInGb' - The size of the disk for the database.
--
-- * 'rdhRamSizeInGb' - The amount of RAM in GB for the database.
relationalDatabaseHardware ::
  RelationalDatabaseHardware
relationalDatabaseHardware =
  RelationalDatabaseHardware'
    { _rdhCpuCount = Nothing,
      _rdhDiskSizeInGb = Nothing,
      _rdhRamSizeInGb = Nothing
    }

-- | The number of vCPUs for the database.
rdhCpuCount :: Lens' RelationalDatabaseHardware (Maybe Int)
rdhCpuCount = lens _rdhCpuCount (\s a -> s {_rdhCpuCount = a})

-- | The size of the disk for the database.
rdhDiskSizeInGb :: Lens' RelationalDatabaseHardware (Maybe Int)
rdhDiskSizeInGb = lens _rdhDiskSizeInGb (\s a -> s {_rdhDiskSizeInGb = a})

-- | The amount of RAM in GB for the database.
rdhRamSizeInGb :: Lens' RelationalDatabaseHardware (Maybe Double)
rdhRamSizeInGb = lens _rdhRamSizeInGb (\s a -> s {_rdhRamSizeInGb = a})

instance FromJSON RelationalDatabaseHardware where
  parseJSON =
    withObject
      "RelationalDatabaseHardware"
      ( \x ->
          RelationalDatabaseHardware'
            <$> (x .:? "cpuCount")
            <*> (x .:? "diskSizeInGb")
            <*> (x .:? "ramSizeInGb")
      )

instance Hashable RelationalDatabaseHardware

instance NFData RelationalDatabaseHardware
