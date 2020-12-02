{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TableIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TableIdentifier where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A structure that describes a target table for resource linking.
--
--
--
-- /See:/ 'tableIdentifier' smart constructor.
data TableIdentifier = TableIdentifier'
  { _tiCatalogId ::
      !(Maybe Text),
    _tiName :: !(Maybe Text),
    _tiDatabaseName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TableIdentifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tiCatalogId' - The ID of the Data Catalog in which the table resides.
--
-- * 'tiName' - The name of the target table.
--
-- * 'tiDatabaseName' - The name of the catalog database that contains the target table.
tableIdentifier ::
  TableIdentifier
tableIdentifier =
  TableIdentifier'
    { _tiCatalogId = Nothing,
      _tiName = Nothing,
      _tiDatabaseName = Nothing
    }

-- | The ID of the Data Catalog in which the table resides.
tiCatalogId :: Lens' TableIdentifier (Maybe Text)
tiCatalogId = lens _tiCatalogId (\s a -> s {_tiCatalogId = a})

-- | The name of the target table.
tiName :: Lens' TableIdentifier (Maybe Text)
tiName = lens _tiName (\s a -> s {_tiName = a})

-- | The name of the catalog database that contains the target table.
tiDatabaseName :: Lens' TableIdentifier (Maybe Text)
tiDatabaseName = lens _tiDatabaseName (\s a -> s {_tiDatabaseName = a})

instance FromJSON TableIdentifier where
  parseJSON =
    withObject
      "TableIdentifier"
      ( \x ->
          TableIdentifier'
            <$> (x .:? "CatalogId") <*> (x .:? "Name") <*> (x .:? "DatabaseName")
      )

instance Hashable TableIdentifier

instance NFData TableIdentifier

instance ToJSON TableIdentifier where
  toJSON TableIdentifier' {..} =
    object
      ( catMaybes
          [ ("CatalogId" .=) <$> _tiCatalogId,
            ("Name" .=) <$> _tiName,
            ("DatabaseName" .=) <$> _tiDatabaseName
          ]
      )
