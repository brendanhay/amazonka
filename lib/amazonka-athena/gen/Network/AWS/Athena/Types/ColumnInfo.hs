{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.ColumnInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.ColumnInfo where

import Network.AWS.Athena.Types.ColumnNullable
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the columns in a query execution result.
--
--
--
-- /See:/ 'columnInfo' smart constructor.
data ColumnInfo = ColumnInfo'
  { _ciScale :: !(Maybe Int),
    _ciPrecision :: !(Maybe Int),
    _ciSchemaName :: !(Maybe Text),
    _ciCatalogName :: !(Maybe Text),
    _ciCaseSensitive :: !(Maybe Bool),
    _ciLabel :: !(Maybe Text),
    _ciTableName :: !(Maybe Text),
    _ciNullable :: !(Maybe ColumnNullable),
    _ciName :: !Text,
    _ciType :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ColumnInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciScale' - For @DECIMAL@ data types, specifies the total number of digits in the fractional part of the value. Defaults to 0.
--
-- * 'ciPrecision' - For @DECIMAL@ data types, specifies the total number of digits, up to 38. For performance reasons, we recommend up to 18 digits.
--
-- * 'ciSchemaName' - The schema name (database name) to which the query results belong.
--
-- * 'ciCatalogName' - The catalog to which the query results belong.
--
-- * 'ciCaseSensitive' - Indicates whether values in the column are case-sensitive.
--
-- * 'ciLabel' - A column label.
--
-- * 'ciTableName' - The table name for the query results.
--
-- * 'ciNullable' - Indicates the column's nullable status.
--
-- * 'ciName' - The name of the column.
--
-- * 'ciType' - The data type of the column.
columnInfo ::
  -- | 'ciName'
  Text ->
  -- | 'ciType'
  Text ->
  ColumnInfo
columnInfo pName_ pType_ =
  ColumnInfo'
    { _ciScale = Nothing,
      _ciPrecision = Nothing,
      _ciSchemaName = Nothing,
      _ciCatalogName = Nothing,
      _ciCaseSensitive = Nothing,
      _ciLabel = Nothing,
      _ciTableName = Nothing,
      _ciNullable = Nothing,
      _ciName = pName_,
      _ciType = pType_
    }

-- | For @DECIMAL@ data types, specifies the total number of digits in the fractional part of the value. Defaults to 0.
ciScale :: Lens' ColumnInfo (Maybe Int)
ciScale = lens _ciScale (\s a -> s {_ciScale = a})

-- | For @DECIMAL@ data types, specifies the total number of digits, up to 38. For performance reasons, we recommend up to 18 digits.
ciPrecision :: Lens' ColumnInfo (Maybe Int)
ciPrecision = lens _ciPrecision (\s a -> s {_ciPrecision = a})

-- | The schema name (database name) to which the query results belong.
ciSchemaName :: Lens' ColumnInfo (Maybe Text)
ciSchemaName = lens _ciSchemaName (\s a -> s {_ciSchemaName = a})

-- | The catalog to which the query results belong.
ciCatalogName :: Lens' ColumnInfo (Maybe Text)
ciCatalogName = lens _ciCatalogName (\s a -> s {_ciCatalogName = a})

-- | Indicates whether values in the column are case-sensitive.
ciCaseSensitive :: Lens' ColumnInfo (Maybe Bool)
ciCaseSensitive = lens _ciCaseSensitive (\s a -> s {_ciCaseSensitive = a})

-- | A column label.
ciLabel :: Lens' ColumnInfo (Maybe Text)
ciLabel = lens _ciLabel (\s a -> s {_ciLabel = a})

-- | The table name for the query results.
ciTableName :: Lens' ColumnInfo (Maybe Text)
ciTableName = lens _ciTableName (\s a -> s {_ciTableName = a})

-- | Indicates the column's nullable status.
ciNullable :: Lens' ColumnInfo (Maybe ColumnNullable)
ciNullable = lens _ciNullable (\s a -> s {_ciNullable = a})

-- | The name of the column.
ciName :: Lens' ColumnInfo Text
ciName = lens _ciName (\s a -> s {_ciName = a})

-- | The data type of the column.
ciType :: Lens' ColumnInfo Text
ciType = lens _ciType (\s a -> s {_ciType = a})

instance FromJSON ColumnInfo where
  parseJSON =
    withObject
      "ColumnInfo"
      ( \x ->
          ColumnInfo'
            <$> (x .:? "Scale")
            <*> (x .:? "Precision")
            <*> (x .:? "SchemaName")
            <*> (x .:? "CatalogName")
            <*> (x .:? "CaseSensitive")
            <*> (x .:? "Label")
            <*> (x .:? "TableName")
            <*> (x .:? "Nullable")
            <*> (x .: "Name")
            <*> (x .: "Type")
      )

instance Hashable ColumnInfo

instance NFData ColumnInfo
