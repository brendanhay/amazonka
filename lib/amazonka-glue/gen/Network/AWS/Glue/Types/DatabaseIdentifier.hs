{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.DatabaseIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.DatabaseIdentifier where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A structure that describes a target database for resource linking.
--
--
--
-- /See:/ 'databaseIdentifier' smart constructor.
data DatabaseIdentifier = DatabaseIdentifier'
  { _diCatalogId ::
      !(Maybe Text),
    _diDatabaseName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DatabaseIdentifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diCatalogId' - The ID of the Data Catalog in which the database resides.
--
-- * 'diDatabaseName' - The name of the catalog database.
databaseIdentifier ::
  DatabaseIdentifier
databaseIdentifier =
  DatabaseIdentifier'
    { _diCatalogId = Nothing,
      _diDatabaseName = Nothing
    }

-- | The ID of the Data Catalog in which the database resides.
diCatalogId :: Lens' DatabaseIdentifier (Maybe Text)
diCatalogId = lens _diCatalogId (\s a -> s {_diCatalogId = a})

-- | The name of the catalog database.
diDatabaseName :: Lens' DatabaseIdentifier (Maybe Text)
diDatabaseName = lens _diDatabaseName (\s a -> s {_diDatabaseName = a})

instance FromJSON DatabaseIdentifier where
  parseJSON =
    withObject
      "DatabaseIdentifier"
      ( \x ->
          DatabaseIdentifier'
            <$> (x .:? "CatalogId") <*> (x .:? "DatabaseName")
      )

instance Hashable DatabaseIdentifier

instance NFData DatabaseIdentifier

instance ToJSON DatabaseIdentifier where
  toJSON DatabaseIdentifier' {..} =
    object
      ( catMaybes
          [ ("CatalogId" .=) <$> _diCatalogId,
            ("DatabaseName" .=) <$> _diDatabaseName
          ]
      )
