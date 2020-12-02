{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CatalogEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CatalogEntry where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies a table definition in the AWS Glue Data Catalog.
--
--
--
-- /See:/ 'catalogEntry' smart constructor.
data CatalogEntry = CatalogEntry'
  { _ceDatabaseName :: !Text,
    _ceTableName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CatalogEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ceDatabaseName' - The database in which the table metadata resides.
--
-- * 'ceTableName' - The name of the table in question.
catalogEntry ::
  -- | 'ceDatabaseName'
  Text ->
  -- | 'ceTableName'
  Text ->
  CatalogEntry
catalogEntry pDatabaseName_ pTableName_ =
  CatalogEntry'
    { _ceDatabaseName = pDatabaseName_,
      _ceTableName = pTableName_
    }

-- | The database in which the table metadata resides.
ceDatabaseName :: Lens' CatalogEntry Text
ceDatabaseName = lens _ceDatabaseName (\s a -> s {_ceDatabaseName = a})

-- | The name of the table in question.
ceTableName :: Lens' CatalogEntry Text
ceTableName = lens _ceTableName (\s a -> s {_ceTableName = a})

instance Hashable CatalogEntry

instance NFData CatalogEntry

instance ToJSON CatalogEntry where
  toJSON CatalogEntry' {..} =
    object
      ( catMaybes
          [ Just ("DatabaseName" .= _ceDatabaseName),
            Just ("TableName" .= _ceTableName)
          ]
      )
