{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetColumnStatisticsForTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves table statistics of columns.
--
--
-- The Identity and Access Management (IAM) permission required for this operation is @GetTable@ .
module Network.AWS.Glue.GetColumnStatisticsForTable
  ( -- * Creating a Request
    getColumnStatisticsForTable,
    GetColumnStatisticsForTable,

    -- * Request Lenses
    gcsftCatalogId,
    gcsftDatabaseName,
    gcsftTableName,
    gcsftColumnNames,

    -- * Destructuring the Response
    getColumnStatisticsForTableResponse,
    GetColumnStatisticsForTableResponse,

    -- * Response Lenses
    gcsftrsErrors,
    gcsftrsColumnStatisticsList,
    gcsftrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getColumnStatisticsForTable' smart constructor.
data GetColumnStatisticsForTable = GetColumnStatisticsForTable'
  { _gcsftCatalogId ::
      !(Maybe Text),
    _gcsftDatabaseName :: !Text,
    _gcsftTableName :: !Text,
    _gcsftColumnNames :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetColumnStatisticsForTable' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcsftCatalogId' - The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
--
-- * 'gcsftDatabaseName' - The name of the catalog database where the partitions reside.
--
-- * 'gcsftTableName' - The name of the partitions' table.
--
-- * 'gcsftColumnNames' - A list of the column names.
getColumnStatisticsForTable ::
  -- | 'gcsftDatabaseName'
  Text ->
  -- | 'gcsftTableName'
  Text ->
  GetColumnStatisticsForTable
getColumnStatisticsForTable pDatabaseName_ pTableName_ =
  GetColumnStatisticsForTable'
    { _gcsftCatalogId = Nothing,
      _gcsftDatabaseName = pDatabaseName_,
      _gcsftTableName = pTableName_,
      _gcsftColumnNames = mempty
    }

-- | The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
gcsftCatalogId :: Lens' GetColumnStatisticsForTable (Maybe Text)
gcsftCatalogId = lens _gcsftCatalogId (\s a -> s {_gcsftCatalogId = a})

-- | The name of the catalog database where the partitions reside.
gcsftDatabaseName :: Lens' GetColumnStatisticsForTable Text
gcsftDatabaseName = lens _gcsftDatabaseName (\s a -> s {_gcsftDatabaseName = a})

-- | The name of the partitions' table.
gcsftTableName :: Lens' GetColumnStatisticsForTable Text
gcsftTableName = lens _gcsftTableName (\s a -> s {_gcsftTableName = a})

-- | A list of the column names.
gcsftColumnNames :: Lens' GetColumnStatisticsForTable [Text]
gcsftColumnNames = lens _gcsftColumnNames (\s a -> s {_gcsftColumnNames = a}) . _Coerce

instance AWSRequest GetColumnStatisticsForTable where
  type
    Rs GetColumnStatisticsForTable =
      GetColumnStatisticsForTableResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          GetColumnStatisticsForTableResponse'
            <$> (x .?> "Errors" .!@ mempty)
            <*> (x .?> "ColumnStatisticsList" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable GetColumnStatisticsForTable

instance NFData GetColumnStatisticsForTable

instance ToHeaders GetColumnStatisticsForTable where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSGlue.GetColumnStatisticsForTable" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetColumnStatisticsForTable where
  toJSON GetColumnStatisticsForTable' {..} =
    object
      ( catMaybes
          [ ("CatalogId" .=) <$> _gcsftCatalogId,
            Just ("DatabaseName" .= _gcsftDatabaseName),
            Just ("TableName" .= _gcsftTableName),
            Just ("ColumnNames" .= _gcsftColumnNames)
          ]
      )

instance ToPath GetColumnStatisticsForTable where
  toPath = const "/"

instance ToQuery GetColumnStatisticsForTable where
  toQuery = const mempty

-- | /See:/ 'getColumnStatisticsForTableResponse' smart constructor.
data GetColumnStatisticsForTableResponse = GetColumnStatisticsForTableResponse'
  { _gcsftrsErrors ::
      !( Maybe
           [ColumnError]
       ),
    _gcsftrsColumnStatisticsList ::
      !( Maybe
           [ColumnStatistics]
       ),
    _gcsftrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetColumnStatisticsForTableResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcsftrsErrors' - List of ColumnStatistics that failed to be retrieved.
--
-- * 'gcsftrsColumnStatisticsList' - List of ColumnStatistics that failed to be retrieved.
--
-- * 'gcsftrsResponseStatus' - -- | The response status code.
getColumnStatisticsForTableResponse ::
  -- | 'gcsftrsResponseStatus'
  Int ->
  GetColumnStatisticsForTableResponse
getColumnStatisticsForTableResponse pResponseStatus_ =
  GetColumnStatisticsForTableResponse'
    { _gcsftrsErrors = Nothing,
      _gcsftrsColumnStatisticsList = Nothing,
      _gcsftrsResponseStatus = pResponseStatus_
    }

-- | List of ColumnStatistics that failed to be retrieved.
gcsftrsErrors :: Lens' GetColumnStatisticsForTableResponse [ColumnError]
gcsftrsErrors = lens _gcsftrsErrors (\s a -> s {_gcsftrsErrors = a}) . _Default . _Coerce

-- | List of ColumnStatistics that failed to be retrieved.
gcsftrsColumnStatisticsList :: Lens' GetColumnStatisticsForTableResponse [ColumnStatistics]
gcsftrsColumnStatisticsList = lens _gcsftrsColumnStatisticsList (\s a -> s {_gcsftrsColumnStatisticsList = a}) . _Default . _Coerce

-- | -- | The response status code.
gcsftrsResponseStatus :: Lens' GetColumnStatisticsForTableResponse Int
gcsftrsResponseStatus = lens _gcsftrsResponseStatus (\s a -> s {_gcsftrsResponseStatus = a})

instance NFData GetColumnStatisticsForTableResponse
