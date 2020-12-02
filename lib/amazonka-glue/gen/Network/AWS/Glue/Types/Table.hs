{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Table
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Table where

import Network.AWS.Glue.Types.Column
import Network.AWS.Glue.Types.StorageDescriptor
import Network.AWS.Glue.Types.TableIdentifier
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a collection of related data organized in columns and rows.
--
--
--
-- /See:/ 'table' smart constructor.
data Table = Table'
  { _tRetention :: !(Maybe Nat),
    _tTargetTable :: !(Maybe TableIdentifier),
    _tIsRegisteredWithLakeFormation :: !(Maybe Bool),
    _tCreatedBy :: !(Maybe Text),
    _tTableType :: !(Maybe Text),
    _tCatalogId :: !(Maybe Text),
    _tOwner :: !(Maybe Text),
    _tViewOriginalText :: !(Maybe Text),
    _tUpdateTime :: !(Maybe POSIX),
    _tViewExpandedText :: !(Maybe Text),
    _tLastAnalyzedTime :: !(Maybe POSIX),
    _tStorageDescriptor :: !(Maybe StorageDescriptor),
    _tDatabaseName :: !(Maybe Text),
    _tParameters :: !(Maybe (Map Text (Text))),
    _tLastAccessTime :: !(Maybe POSIX),
    _tDescription :: !(Maybe Text),
    _tPartitionKeys :: !(Maybe [Column]),
    _tCreateTime :: !(Maybe POSIX),
    _tName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Table' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tRetention' - The retention time for this table.
--
-- * 'tTargetTable' - A @TableIdentifier@ structure that describes a target table for resource linking.
--
-- * 'tIsRegisteredWithLakeFormation' - Indicates whether the table has been registered with AWS Lake Formation.
--
-- * 'tCreatedBy' - The person or entity who created the table.
--
-- * 'tTableType' - The type of this table (@EXTERNAL_TABLE@ , @VIRTUAL_VIEW@ , etc.).
--
-- * 'tCatalogId' - The ID of the Data Catalog in which the table resides.
--
-- * 'tOwner' - The owner of the table.
--
-- * 'tViewOriginalText' - If the table is a view, the original text of the view; otherwise @null@ .
--
-- * 'tUpdateTime' - The last time that the table was updated.
--
-- * 'tViewExpandedText' - If the table is a view, the expanded text of the view; otherwise @null@ .
--
-- * 'tLastAnalyzedTime' - The last time that column statistics were computed for this table.
--
-- * 'tStorageDescriptor' - A storage descriptor containing information about the physical storage of this table.
--
-- * 'tDatabaseName' - The name of the database where the table metadata resides. For Hive compatibility, this must be all lowercase.
--
-- * 'tParameters' - These key-value pairs define properties associated with the table.
--
-- * 'tLastAccessTime' - The last time that the table was accessed. This is usually taken from HDFS, and might not be reliable.
--
-- * 'tDescription' - A description of the table.
--
-- * 'tPartitionKeys' - A list of columns by which the table is partitioned. Only primitive types are supported as partition keys. When you create a table used by Amazon Athena, and you do not specify any @partitionKeys@ , you must at least set the value of @partitionKeys@ to an empty list. For example: @"PartitionKeys": []@
--
-- * 'tCreateTime' - The time when the table definition was created in the Data Catalog.
--
-- * 'tName' - The table name. For Hive compatibility, this must be entirely lowercase.
table ::
  -- | 'tName'
  Text ->
  Table
table pName_ =
  Table'
    { _tRetention = Nothing,
      _tTargetTable = Nothing,
      _tIsRegisteredWithLakeFormation = Nothing,
      _tCreatedBy = Nothing,
      _tTableType = Nothing,
      _tCatalogId = Nothing,
      _tOwner = Nothing,
      _tViewOriginalText = Nothing,
      _tUpdateTime = Nothing,
      _tViewExpandedText = Nothing,
      _tLastAnalyzedTime = Nothing,
      _tStorageDescriptor = Nothing,
      _tDatabaseName = Nothing,
      _tParameters = Nothing,
      _tLastAccessTime = Nothing,
      _tDescription = Nothing,
      _tPartitionKeys = Nothing,
      _tCreateTime = Nothing,
      _tName = pName_
    }

-- | The retention time for this table.
tRetention :: Lens' Table (Maybe Natural)
tRetention = lens _tRetention (\s a -> s {_tRetention = a}) . mapping _Nat

-- | A @TableIdentifier@ structure that describes a target table for resource linking.
tTargetTable :: Lens' Table (Maybe TableIdentifier)
tTargetTable = lens _tTargetTable (\s a -> s {_tTargetTable = a})

-- | Indicates whether the table has been registered with AWS Lake Formation.
tIsRegisteredWithLakeFormation :: Lens' Table (Maybe Bool)
tIsRegisteredWithLakeFormation = lens _tIsRegisteredWithLakeFormation (\s a -> s {_tIsRegisteredWithLakeFormation = a})

-- | The person or entity who created the table.
tCreatedBy :: Lens' Table (Maybe Text)
tCreatedBy = lens _tCreatedBy (\s a -> s {_tCreatedBy = a})

-- | The type of this table (@EXTERNAL_TABLE@ , @VIRTUAL_VIEW@ , etc.).
tTableType :: Lens' Table (Maybe Text)
tTableType = lens _tTableType (\s a -> s {_tTableType = a})

-- | The ID of the Data Catalog in which the table resides.
tCatalogId :: Lens' Table (Maybe Text)
tCatalogId = lens _tCatalogId (\s a -> s {_tCatalogId = a})

-- | The owner of the table.
tOwner :: Lens' Table (Maybe Text)
tOwner = lens _tOwner (\s a -> s {_tOwner = a})

-- | If the table is a view, the original text of the view; otherwise @null@ .
tViewOriginalText :: Lens' Table (Maybe Text)
tViewOriginalText = lens _tViewOriginalText (\s a -> s {_tViewOriginalText = a})

-- | The last time that the table was updated.
tUpdateTime :: Lens' Table (Maybe UTCTime)
tUpdateTime = lens _tUpdateTime (\s a -> s {_tUpdateTime = a}) . mapping _Time

-- | If the table is a view, the expanded text of the view; otherwise @null@ .
tViewExpandedText :: Lens' Table (Maybe Text)
tViewExpandedText = lens _tViewExpandedText (\s a -> s {_tViewExpandedText = a})

-- | The last time that column statistics were computed for this table.
tLastAnalyzedTime :: Lens' Table (Maybe UTCTime)
tLastAnalyzedTime = lens _tLastAnalyzedTime (\s a -> s {_tLastAnalyzedTime = a}) . mapping _Time

-- | A storage descriptor containing information about the physical storage of this table.
tStorageDescriptor :: Lens' Table (Maybe StorageDescriptor)
tStorageDescriptor = lens _tStorageDescriptor (\s a -> s {_tStorageDescriptor = a})

-- | The name of the database where the table metadata resides. For Hive compatibility, this must be all lowercase.
tDatabaseName :: Lens' Table (Maybe Text)
tDatabaseName = lens _tDatabaseName (\s a -> s {_tDatabaseName = a})

-- | These key-value pairs define properties associated with the table.
tParameters :: Lens' Table (HashMap Text (Text))
tParameters = lens _tParameters (\s a -> s {_tParameters = a}) . _Default . _Map

-- | The last time that the table was accessed. This is usually taken from HDFS, and might not be reliable.
tLastAccessTime :: Lens' Table (Maybe UTCTime)
tLastAccessTime = lens _tLastAccessTime (\s a -> s {_tLastAccessTime = a}) . mapping _Time

-- | A description of the table.
tDescription :: Lens' Table (Maybe Text)
tDescription = lens _tDescription (\s a -> s {_tDescription = a})

-- | A list of columns by which the table is partitioned. Only primitive types are supported as partition keys. When you create a table used by Amazon Athena, and you do not specify any @partitionKeys@ , you must at least set the value of @partitionKeys@ to an empty list. For example: @"PartitionKeys": []@
tPartitionKeys :: Lens' Table [Column]
tPartitionKeys = lens _tPartitionKeys (\s a -> s {_tPartitionKeys = a}) . _Default . _Coerce

-- | The time when the table definition was created in the Data Catalog.
tCreateTime :: Lens' Table (Maybe UTCTime)
tCreateTime = lens _tCreateTime (\s a -> s {_tCreateTime = a}) . mapping _Time

-- | The table name. For Hive compatibility, this must be entirely lowercase.
tName :: Lens' Table Text
tName = lens _tName (\s a -> s {_tName = a})

instance FromJSON Table where
  parseJSON =
    withObject
      "Table"
      ( \x ->
          Table'
            <$> (x .:? "Retention")
            <*> (x .:? "TargetTable")
            <*> (x .:? "IsRegisteredWithLakeFormation")
            <*> (x .:? "CreatedBy")
            <*> (x .:? "TableType")
            <*> (x .:? "CatalogId")
            <*> (x .:? "Owner")
            <*> (x .:? "ViewOriginalText")
            <*> (x .:? "UpdateTime")
            <*> (x .:? "ViewExpandedText")
            <*> (x .:? "LastAnalyzedTime")
            <*> (x .:? "StorageDescriptor")
            <*> (x .:? "DatabaseName")
            <*> (x .:? "Parameters" .!= mempty)
            <*> (x .:? "LastAccessTime")
            <*> (x .:? "Description")
            <*> (x .:? "PartitionKeys" .!= mempty)
            <*> (x .:? "CreateTime")
            <*> (x .: "Name")
      )

instance Hashable Table

instance NFData Table
