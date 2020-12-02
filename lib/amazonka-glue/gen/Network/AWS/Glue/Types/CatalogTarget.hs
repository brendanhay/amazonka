{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CatalogTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CatalogTarget where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies an AWS Glue Data Catalog target.
--
--
--
-- /See:/ 'catalogTarget' smart constructor.
data CatalogTarget = CatalogTarget'
  { _ctDatabaseName :: !Text,
    _ctTables :: !(List1 Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CatalogTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctDatabaseName' - The name of the database to be synchronized.
--
-- * 'ctTables' - A list of the tables to be synchronized.
catalogTarget ::
  -- | 'ctDatabaseName'
  Text ->
  -- | 'ctTables'
  NonEmpty Text ->
  CatalogTarget
catalogTarget pDatabaseName_ pTables_ =
  CatalogTarget'
    { _ctDatabaseName = pDatabaseName_,
      _ctTables = _List1 # pTables_
    }

-- | The name of the database to be synchronized.
ctDatabaseName :: Lens' CatalogTarget Text
ctDatabaseName = lens _ctDatabaseName (\s a -> s {_ctDatabaseName = a})

-- | A list of the tables to be synchronized.
ctTables :: Lens' CatalogTarget (NonEmpty Text)
ctTables = lens _ctTables (\s a -> s {_ctTables = a}) . _List1

instance FromJSON CatalogTarget where
  parseJSON =
    withObject
      "CatalogTarget"
      ( \x ->
          CatalogTarget' <$> (x .: "DatabaseName") <*> (x .: "Tables")
      )

instance Hashable CatalogTarget

instance NFData CatalogTarget

instance ToJSON CatalogTarget where
  toJSON CatalogTarget' {..} =
    object
      ( catMaybes
          [ Just ("DatabaseName" .= _ctDatabaseName),
            Just ("Tables" .= _ctTables)
          ]
      )
