{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Partition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Partition where

import Network.AWS.Glue.Types.StorageDescriptor
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a slice of table data.
--
--
--
-- /See:/ 'partition' smart constructor.
data Partition = Partition'
  { _pCreationTime :: !(Maybe POSIX),
    _pValues :: !(Maybe [Text]),
    _pCatalogId :: !(Maybe Text),
    _pLastAnalyzedTime :: !(Maybe POSIX),
    _pStorageDescriptor :: !(Maybe StorageDescriptor),
    _pDatabaseName :: !(Maybe Text),
    _pParameters :: !(Maybe (Map Text (Text))),
    _pLastAccessTime :: !(Maybe POSIX),
    _pTableName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Partition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pCreationTime' - The time at which the partition was created.
--
-- * 'pValues' - The values of the partition.
--
-- * 'pCatalogId' - The ID of the Data Catalog in which the partition resides.
--
-- * 'pLastAnalyzedTime' - The last time at which column statistics were computed for this partition.
--
-- * 'pStorageDescriptor' - Provides information about the physical location where the partition is stored.
--
-- * 'pDatabaseName' - The name of the catalog database in which to create the partition.
--
-- * 'pParameters' - These key-value pairs define partition parameters.
--
-- * 'pLastAccessTime' - The last time at which the partition was accessed.
--
-- * 'pTableName' - The name of the database table in which to create the partition.
partition ::
  Partition
partition =
  Partition'
    { _pCreationTime = Nothing,
      _pValues = Nothing,
      _pCatalogId = Nothing,
      _pLastAnalyzedTime = Nothing,
      _pStorageDescriptor = Nothing,
      _pDatabaseName = Nothing,
      _pParameters = Nothing,
      _pLastAccessTime = Nothing,
      _pTableName = Nothing
    }

-- | The time at which the partition was created.
pCreationTime :: Lens' Partition (Maybe UTCTime)
pCreationTime = lens _pCreationTime (\s a -> s {_pCreationTime = a}) . mapping _Time

-- | The values of the partition.
pValues :: Lens' Partition [Text]
pValues = lens _pValues (\s a -> s {_pValues = a}) . _Default . _Coerce

-- | The ID of the Data Catalog in which the partition resides.
pCatalogId :: Lens' Partition (Maybe Text)
pCatalogId = lens _pCatalogId (\s a -> s {_pCatalogId = a})

-- | The last time at which column statistics were computed for this partition.
pLastAnalyzedTime :: Lens' Partition (Maybe UTCTime)
pLastAnalyzedTime = lens _pLastAnalyzedTime (\s a -> s {_pLastAnalyzedTime = a}) . mapping _Time

-- | Provides information about the physical location where the partition is stored.
pStorageDescriptor :: Lens' Partition (Maybe StorageDescriptor)
pStorageDescriptor = lens _pStorageDescriptor (\s a -> s {_pStorageDescriptor = a})

-- | The name of the catalog database in which to create the partition.
pDatabaseName :: Lens' Partition (Maybe Text)
pDatabaseName = lens _pDatabaseName (\s a -> s {_pDatabaseName = a})

-- | These key-value pairs define partition parameters.
pParameters :: Lens' Partition (HashMap Text (Text))
pParameters = lens _pParameters (\s a -> s {_pParameters = a}) . _Default . _Map

-- | The last time at which the partition was accessed.
pLastAccessTime :: Lens' Partition (Maybe UTCTime)
pLastAccessTime = lens _pLastAccessTime (\s a -> s {_pLastAccessTime = a}) . mapping _Time

-- | The name of the database table in which to create the partition.
pTableName :: Lens' Partition (Maybe Text)
pTableName = lens _pTableName (\s a -> s {_pTableName = a})

instance FromJSON Partition where
  parseJSON =
    withObject
      "Partition"
      ( \x ->
          Partition'
            <$> (x .:? "CreationTime")
            <*> (x .:? "Values" .!= mempty)
            <*> (x .:? "CatalogId")
            <*> (x .:? "LastAnalyzedTime")
            <*> (x .:? "StorageDescriptor")
            <*> (x .:? "DatabaseName")
            <*> (x .:? "Parameters" .!= mempty)
            <*> (x .:? "LastAccessTime")
            <*> (x .:? "TableName")
      )

instance Hashable Partition

instance NFData Partition
