{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.ResultSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.ResultSet where

import Network.AWS.Athena.Types.ResultSetMetadata
import Network.AWS.Athena.Types.Row
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The metadata and rows that comprise a query result set. The metadata describes the column structure and data types. To return a @ResultSet@ object, use 'GetQueryResults' .
--
--
--
-- /See:/ 'resultSet' smart constructor.
data ResultSet = ResultSet'
  { _rsRows :: !(Maybe [Row]),
    _rsResultSetMetadata :: !(Maybe ResultSetMetadata)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResultSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsRows' - The rows in the table.
--
-- * 'rsResultSetMetadata' - The metadata that describes the column structure and data types of a table of query results.
resultSet ::
  ResultSet
resultSet =
  ResultSet' {_rsRows = Nothing, _rsResultSetMetadata = Nothing}

-- | The rows in the table.
rsRows :: Lens' ResultSet [Row]
rsRows = lens _rsRows (\s a -> s {_rsRows = a}) . _Default . _Coerce

-- | The metadata that describes the column structure and data types of a table of query results.
rsResultSetMetadata :: Lens' ResultSet (Maybe ResultSetMetadata)
rsResultSetMetadata = lens _rsResultSetMetadata (\s a -> s {_rsResultSetMetadata = a})

instance FromJSON ResultSet where
  parseJSON =
    withObject
      "ResultSet"
      ( \x ->
          ResultSet'
            <$> (x .:? "Rows" .!= mempty) <*> (x .:? "ResultSetMetadata")
      )

instance Hashable ResultSet

instance NFData ResultSet
