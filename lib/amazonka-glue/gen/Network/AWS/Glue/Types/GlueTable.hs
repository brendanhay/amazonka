{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.GlueTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.GlueTable where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The database and table in the AWS Glue Data Catalog that is used for input or output data.
--
--
--
-- /See:/ 'glueTable' smart constructor.
data GlueTable = GlueTable'
  { _gtCatalogId :: !(Maybe Text),
    _gtConnectionName :: !(Maybe Text),
    _gtDatabaseName :: !Text,
    _gtTableName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GlueTable' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtCatalogId' - A unique identifier for the AWS Glue Data Catalog.
--
-- * 'gtConnectionName' - The name of the connection to the AWS Glue Data Catalog.
--
-- * 'gtDatabaseName' - A database name in the AWS Glue Data Catalog.
--
-- * 'gtTableName' - A table name in the AWS Glue Data Catalog.
glueTable ::
  -- | 'gtDatabaseName'
  Text ->
  -- | 'gtTableName'
  Text ->
  GlueTable
glueTable pDatabaseName_ pTableName_ =
  GlueTable'
    { _gtCatalogId = Nothing,
      _gtConnectionName = Nothing,
      _gtDatabaseName = pDatabaseName_,
      _gtTableName = pTableName_
    }

-- | A unique identifier for the AWS Glue Data Catalog.
gtCatalogId :: Lens' GlueTable (Maybe Text)
gtCatalogId = lens _gtCatalogId (\s a -> s {_gtCatalogId = a})

-- | The name of the connection to the AWS Glue Data Catalog.
gtConnectionName :: Lens' GlueTable (Maybe Text)
gtConnectionName = lens _gtConnectionName (\s a -> s {_gtConnectionName = a})

-- | A database name in the AWS Glue Data Catalog.
gtDatabaseName :: Lens' GlueTable Text
gtDatabaseName = lens _gtDatabaseName (\s a -> s {_gtDatabaseName = a})

-- | A table name in the AWS Glue Data Catalog.
gtTableName :: Lens' GlueTable Text
gtTableName = lens _gtTableName (\s a -> s {_gtTableName = a})

instance FromJSON GlueTable where
  parseJSON =
    withObject
      "GlueTable"
      ( \x ->
          GlueTable'
            <$> (x .:? "CatalogId")
            <*> (x .:? "ConnectionName")
            <*> (x .: "DatabaseName")
            <*> (x .: "TableName")
      )

instance Hashable GlueTable

instance NFData GlueTable

instance ToJSON GlueTable where
  toJSON GlueTable' {..} =
    object
      ( catMaybes
          [ ("CatalogId" .=) <$> _gtCatalogId,
            ("ConnectionName" .=) <$> _gtConnectionName,
            Just ("DatabaseName" .= _gtDatabaseName),
            Just ("TableName" .= _gtTableName)
          ]
      )
