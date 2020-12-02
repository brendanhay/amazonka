{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.ResultSetMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.ResultSetMetadata where

import Network.AWS.Athena.Types.ColumnInfo
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The metadata that describes the column structure and data types of a table of query results. To return a @ResultSetMetadata@ object, use 'GetQueryResults' .
--
--
--
-- /See:/ 'resultSetMetadata' smart constructor.
newtype ResultSetMetadata = ResultSetMetadata'
  { _rsmColumnInfo ::
      Maybe [ColumnInfo]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResultSetMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsmColumnInfo' - Information about the columns returned in a query result metadata.
resultSetMetadata ::
  ResultSetMetadata
resultSetMetadata = ResultSetMetadata' {_rsmColumnInfo = Nothing}

-- | Information about the columns returned in a query result metadata.
rsmColumnInfo :: Lens' ResultSetMetadata [ColumnInfo]
rsmColumnInfo = lens _rsmColumnInfo (\s a -> s {_rsmColumnInfo = a}) . _Default . _Coerce

instance FromJSON ResultSetMetadata where
  parseJSON =
    withObject
      "ResultSetMetadata"
      (\x -> ResultSetMetadata' <$> (x .:? "ColumnInfo" .!= mempty))

instance Hashable ResultSetMetadata

instance NFData ResultSetMetadata
