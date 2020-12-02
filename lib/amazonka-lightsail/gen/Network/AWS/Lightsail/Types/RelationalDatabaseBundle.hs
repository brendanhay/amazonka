{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.RelationalDatabaseBundle
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RelationalDatabaseBundle where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a database bundle. A bundle describes the performance specifications of the database.
--
--
--
-- /See:/ 'relationalDatabaseBundle' smart constructor.
data RelationalDatabaseBundle = RelationalDatabaseBundle'
  { _rdbIsEncrypted ::
      !(Maybe Bool),
    _rdbCpuCount :: !(Maybe Int),
    _rdbTransferPerMonthInGb :: !(Maybe Int),
    _rdbBundleId :: !(Maybe Text),
    _rdbName :: !(Maybe Text),
    _rdbDiskSizeInGb :: !(Maybe Int),
    _rdbPrice :: !(Maybe Double),
    _rdbIsActive :: !(Maybe Bool),
    _rdbRamSizeInGb :: !(Maybe Double)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RelationalDatabaseBundle' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdbIsEncrypted' - A Boolean value indicating whether the database bundle is encrypted.
--
-- * 'rdbCpuCount' - The number of virtual CPUs (vCPUs) for the database bundle.
--
-- * 'rdbTransferPerMonthInGb' - The data transfer rate per month in GB for the database bundle.
--
-- * 'rdbBundleId' - The ID for the database bundle.
--
-- * 'rdbName' - The name for the database bundle.
--
-- * 'rdbDiskSizeInGb' - The size of the disk for the database bundle.
--
-- * 'rdbPrice' - The cost of the database bundle in US currency.
--
-- * 'rdbIsActive' - A Boolean value indicating whether the database bundle is active.
--
-- * 'rdbRamSizeInGb' - The amount of RAM in GB (for example, @2.0@ ) for the database bundle.
relationalDatabaseBundle ::
  RelationalDatabaseBundle
relationalDatabaseBundle =
  RelationalDatabaseBundle'
    { _rdbIsEncrypted = Nothing,
      _rdbCpuCount = Nothing,
      _rdbTransferPerMonthInGb = Nothing,
      _rdbBundleId = Nothing,
      _rdbName = Nothing,
      _rdbDiskSizeInGb = Nothing,
      _rdbPrice = Nothing,
      _rdbIsActive = Nothing,
      _rdbRamSizeInGb = Nothing
    }

-- | A Boolean value indicating whether the database bundle is encrypted.
rdbIsEncrypted :: Lens' RelationalDatabaseBundle (Maybe Bool)
rdbIsEncrypted = lens _rdbIsEncrypted (\s a -> s {_rdbIsEncrypted = a})

-- | The number of virtual CPUs (vCPUs) for the database bundle.
rdbCpuCount :: Lens' RelationalDatabaseBundle (Maybe Int)
rdbCpuCount = lens _rdbCpuCount (\s a -> s {_rdbCpuCount = a})

-- | The data transfer rate per month in GB for the database bundle.
rdbTransferPerMonthInGb :: Lens' RelationalDatabaseBundle (Maybe Int)
rdbTransferPerMonthInGb = lens _rdbTransferPerMonthInGb (\s a -> s {_rdbTransferPerMonthInGb = a})

-- | The ID for the database bundle.
rdbBundleId :: Lens' RelationalDatabaseBundle (Maybe Text)
rdbBundleId = lens _rdbBundleId (\s a -> s {_rdbBundleId = a})

-- | The name for the database bundle.
rdbName :: Lens' RelationalDatabaseBundle (Maybe Text)
rdbName = lens _rdbName (\s a -> s {_rdbName = a})

-- | The size of the disk for the database bundle.
rdbDiskSizeInGb :: Lens' RelationalDatabaseBundle (Maybe Int)
rdbDiskSizeInGb = lens _rdbDiskSizeInGb (\s a -> s {_rdbDiskSizeInGb = a})

-- | The cost of the database bundle in US currency.
rdbPrice :: Lens' RelationalDatabaseBundle (Maybe Double)
rdbPrice = lens _rdbPrice (\s a -> s {_rdbPrice = a})

-- | A Boolean value indicating whether the database bundle is active.
rdbIsActive :: Lens' RelationalDatabaseBundle (Maybe Bool)
rdbIsActive = lens _rdbIsActive (\s a -> s {_rdbIsActive = a})

-- | The amount of RAM in GB (for example, @2.0@ ) for the database bundle.
rdbRamSizeInGb :: Lens' RelationalDatabaseBundle (Maybe Double)
rdbRamSizeInGb = lens _rdbRamSizeInGb (\s a -> s {_rdbRamSizeInGb = a})

instance FromJSON RelationalDatabaseBundle where
  parseJSON =
    withObject
      "RelationalDatabaseBundle"
      ( \x ->
          RelationalDatabaseBundle'
            <$> (x .:? "isEncrypted")
            <*> (x .:? "cpuCount")
            <*> (x .:? "transferPerMonthInGb")
            <*> (x .:? "bundleId")
            <*> (x .:? "name")
            <*> (x .:? "diskSizeInGb")
            <*> (x .:? "price")
            <*> (x .:? "isActive")
            <*> (x .:? "ramSizeInGb")
      )

instance Hashable RelationalDatabaseBundle

instance NFData RelationalDatabaseBundle
