{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.TableToReload
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.TableToReload where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides the name of the schema and table to be reloaded.
--
--
--
-- /See:/ 'tableToReload' smart constructor.
data TableToReload = TableToReload'
  { _ttrSchemaName :: !Text,
    _ttrTableName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TableToReload' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ttrSchemaName' - The schema name of the table to be reloaded.
--
-- * 'ttrTableName' - The table name of the table to be reloaded.
tableToReload ::
  -- | 'ttrSchemaName'
  Text ->
  -- | 'ttrTableName'
  Text ->
  TableToReload
tableToReload pSchemaName_ pTableName_ =
  TableToReload'
    { _ttrSchemaName = pSchemaName_,
      _ttrTableName = pTableName_
    }

-- | The schema name of the table to be reloaded.
ttrSchemaName :: Lens' TableToReload Text
ttrSchemaName = lens _ttrSchemaName (\s a -> s {_ttrSchemaName = a})

-- | The table name of the table to be reloaded.
ttrTableName :: Lens' TableToReload Text
ttrTableName = lens _ttrTableName (\s a -> s {_ttrTableName = a})

instance Hashable TableToReload

instance NFData TableToReload

instance ToJSON TableToReload where
  toJSON TableToReload' {..} =
    object
      ( catMaybes
          [ Just ("SchemaName" .= _ttrSchemaName),
            Just ("TableName" .= _ttrTableName)
          ]
      )
