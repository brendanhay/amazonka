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
-- Module      : Network.AWS.Glue.DeletePartitionIndex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified partition index from an existing table.
module Network.AWS.Glue.DeletePartitionIndex
  ( -- * Creating a Request
    deletePartitionIndex,
    DeletePartitionIndex,

    -- * Request Lenses
    dpiCatalogId,
    dpiDatabaseName,
    dpiTableName,
    dpiIndexName,

    -- * Destructuring the Response
    deletePartitionIndexResponse,
    DeletePartitionIndexResponse,

    -- * Response Lenses
    dpirsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deletePartitionIndex' smart constructor.
data DeletePartitionIndex = DeletePartitionIndex'
  { _dpiCatalogId ::
      !(Maybe Text),
    _dpiDatabaseName :: !Text,
    _dpiTableName :: !Text,
    _dpiIndexName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeletePartitionIndex' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpiCatalogId' - The catalog ID where the table resides.
--
-- * 'dpiDatabaseName' - Specifies the name of a database from which you want to delete a partition index.
--
-- * 'dpiTableName' - Specifies the name of a table from which you want to delete a partition index.
--
-- * 'dpiIndexName' - The name of the partition index to be deleted.
deletePartitionIndex ::
  -- | 'dpiDatabaseName'
  Text ->
  -- | 'dpiTableName'
  Text ->
  -- | 'dpiIndexName'
  Text ->
  DeletePartitionIndex
deletePartitionIndex pDatabaseName_ pTableName_ pIndexName_ =
  DeletePartitionIndex'
    { _dpiCatalogId = Nothing,
      _dpiDatabaseName = pDatabaseName_,
      _dpiTableName = pTableName_,
      _dpiIndexName = pIndexName_
    }

-- | The catalog ID where the table resides.
dpiCatalogId :: Lens' DeletePartitionIndex (Maybe Text)
dpiCatalogId = lens _dpiCatalogId (\s a -> s {_dpiCatalogId = a})

-- | Specifies the name of a database from which you want to delete a partition index.
dpiDatabaseName :: Lens' DeletePartitionIndex Text
dpiDatabaseName = lens _dpiDatabaseName (\s a -> s {_dpiDatabaseName = a})

-- | Specifies the name of a table from which you want to delete a partition index.
dpiTableName :: Lens' DeletePartitionIndex Text
dpiTableName = lens _dpiTableName (\s a -> s {_dpiTableName = a})

-- | The name of the partition index to be deleted.
dpiIndexName :: Lens' DeletePartitionIndex Text
dpiIndexName = lens _dpiIndexName (\s a -> s {_dpiIndexName = a})

instance AWSRequest DeletePartitionIndex where
  type Rs DeletePartitionIndex = DeletePartitionIndexResponse
  request = postJSON glue
  response =
    receiveEmpty
      (\s h x -> DeletePartitionIndexResponse' <$> (pure (fromEnum s)))

instance Hashable DeletePartitionIndex

instance NFData DeletePartitionIndex

instance ToHeaders DeletePartitionIndex where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.DeletePartitionIndex" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeletePartitionIndex where
  toJSON DeletePartitionIndex' {..} =
    object
      ( catMaybes
          [ ("CatalogId" .=) <$> _dpiCatalogId,
            Just ("DatabaseName" .= _dpiDatabaseName),
            Just ("TableName" .= _dpiTableName),
            Just ("IndexName" .= _dpiIndexName)
          ]
      )

instance ToPath DeletePartitionIndex where
  toPath = const "/"

instance ToQuery DeletePartitionIndex where
  toQuery = const mempty

-- | /See:/ 'deletePartitionIndexResponse' smart constructor.
newtype DeletePartitionIndexResponse = DeletePartitionIndexResponse'
  { _dpirsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeletePartitionIndexResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpirsResponseStatus' - -- | The response status code.
deletePartitionIndexResponse ::
  -- | 'dpirsResponseStatus'
  Int ->
  DeletePartitionIndexResponse
deletePartitionIndexResponse pResponseStatus_ =
  DeletePartitionIndexResponse'
    { _dpirsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
dpirsResponseStatus :: Lens' DeletePartitionIndexResponse Int
dpirsResponseStatus = lens _dpirsResponseStatus (\s a -> s {_dpirsResponseStatus = a})

instance NFData DeletePartitionIndexResponse
