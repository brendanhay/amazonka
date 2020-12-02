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
-- Module      : Network.AWS.Glue.DeleteColumnStatisticsForTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves table statistics of columns.
--
--
-- The Identity and Access Management (IAM) permission required for this operation is @DeleteTable@ .
module Network.AWS.Glue.DeleteColumnStatisticsForTable
  ( -- * Creating a Request
    deleteColumnStatisticsForTable,
    DeleteColumnStatisticsForTable,

    -- * Request Lenses
    dcsftCatalogId,
    dcsftDatabaseName,
    dcsftTableName,
    dcsftColumnName,

    -- * Destructuring the Response
    deleteColumnStatisticsForTableResponse,
    DeleteColumnStatisticsForTableResponse,

    -- * Response Lenses
    dcsftrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteColumnStatisticsForTable' smart constructor.
data DeleteColumnStatisticsForTable = DeleteColumnStatisticsForTable'
  { _dcsftCatalogId ::
      !(Maybe Text),
    _dcsftDatabaseName :: !Text,
    _dcsftTableName :: !Text,
    _dcsftColumnName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteColumnStatisticsForTable' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsftCatalogId' - The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
--
-- * 'dcsftDatabaseName' - The name of the catalog database where the partitions reside.
--
-- * 'dcsftTableName' - The name of the partitions' table.
--
-- * 'dcsftColumnName' - The name of the column.
deleteColumnStatisticsForTable ::
  -- | 'dcsftDatabaseName'
  Text ->
  -- | 'dcsftTableName'
  Text ->
  -- | 'dcsftColumnName'
  Text ->
  DeleteColumnStatisticsForTable
deleteColumnStatisticsForTable
  pDatabaseName_
  pTableName_
  pColumnName_ =
    DeleteColumnStatisticsForTable'
      { _dcsftCatalogId = Nothing,
        _dcsftDatabaseName = pDatabaseName_,
        _dcsftTableName = pTableName_,
        _dcsftColumnName = pColumnName_
      }

-- | The ID of the Data Catalog where the partitions in question reside. If none is supplied, the AWS account ID is used by default.
dcsftCatalogId :: Lens' DeleteColumnStatisticsForTable (Maybe Text)
dcsftCatalogId = lens _dcsftCatalogId (\s a -> s {_dcsftCatalogId = a})

-- | The name of the catalog database where the partitions reside.
dcsftDatabaseName :: Lens' DeleteColumnStatisticsForTable Text
dcsftDatabaseName = lens _dcsftDatabaseName (\s a -> s {_dcsftDatabaseName = a})

-- | The name of the partitions' table.
dcsftTableName :: Lens' DeleteColumnStatisticsForTable Text
dcsftTableName = lens _dcsftTableName (\s a -> s {_dcsftTableName = a})

-- | The name of the column.
dcsftColumnName :: Lens' DeleteColumnStatisticsForTable Text
dcsftColumnName = lens _dcsftColumnName (\s a -> s {_dcsftColumnName = a})

instance AWSRequest DeleteColumnStatisticsForTable where
  type
    Rs DeleteColumnStatisticsForTable =
      DeleteColumnStatisticsForTableResponse
  request = postJSON glue
  response =
    receiveEmpty
      ( \s h x ->
          DeleteColumnStatisticsForTableResponse' <$> (pure (fromEnum s))
      )

instance Hashable DeleteColumnStatisticsForTable

instance NFData DeleteColumnStatisticsForTable

instance ToHeaders DeleteColumnStatisticsForTable where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSGlue.DeleteColumnStatisticsForTable" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteColumnStatisticsForTable where
  toJSON DeleteColumnStatisticsForTable' {..} =
    object
      ( catMaybes
          [ ("CatalogId" .=) <$> _dcsftCatalogId,
            Just ("DatabaseName" .= _dcsftDatabaseName),
            Just ("TableName" .= _dcsftTableName),
            Just ("ColumnName" .= _dcsftColumnName)
          ]
      )

instance ToPath DeleteColumnStatisticsForTable where
  toPath = const "/"

instance ToQuery DeleteColumnStatisticsForTable where
  toQuery = const mempty

-- | /See:/ 'deleteColumnStatisticsForTableResponse' smart constructor.
newtype DeleteColumnStatisticsForTableResponse = DeleteColumnStatisticsForTableResponse'
  { _dcsftrsResponseStatus ::
      Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DeleteColumnStatisticsForTableResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsftrsResponseStatus' - -- | The response status code.
deleteColumnStatisticsForTableResponse ::
  -- | 'dcsftrsResponseStatus'
  Int ->
  DeleteColumnStatisticsForTableResponse
deleteColumnStatisticsForTableResponse pResponseStatus_ =
  DeleteColumnStatisticsForTableResponse'
    { _dcsftrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
dcsftrsResponseStatus :: Lens' DeleteColumnStatisticsForTableResponse Int
dcsftrsResponseStatus = lens _dcsftrsResponseStatus (\s a -> s {_dcsftrsResponseStatus = a})

instance NFData DeleteColumnStatisticsForTableResponse
