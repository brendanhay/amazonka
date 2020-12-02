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
-- Module      : Network.AWS.Athena.GetTableMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns table metadata for the specified catalog, database, and table.
module Network.AWS.Athena.GetTableMetadata
  ( -- * Creating a Request
    getTableMetadata,
    GetTableMetadata,

    -- * Request Lenses
    gtmCatalogName,
    gtmDatabaseName,
    gtmTableName,

    -- * Destructuring the Response
    getTableMetadataResponse,
    GetTableMetadataResponse,

    -- * Response Lenses
    gtmrsTableMetadata,
    gtmrsResponseStatus,
  )
where

import Network.AWS.Athena.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getTableMetadata' smart constructor.
data GetTableMetadata = GetTableMetadata'
  { _gtmCatalogName :: !Text,
    _gtmDatabaseName :: !Text,
    _gtmTableName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetTableMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtmCatalogName' - The name of the data catalog that contains the database and table metadata to return.
--
-- * 'gtmDatabaseName' - The name of the database that contains the table metadata to return.
--
-- * 'gtmTableName' - The name of the table for which metadata is returned.
getTableMetadata ::
  -- | 'gtmCatalogName'
  Text ->
  -- | 'gtmDatabaseName'
  Text ->
  -- | 'gtmTableName'
  Text ->
  GetTableMetadata
getTableMetadata pCatalogName_ pDatabaseName_ pTableName_ =
  GetTableMetadata'
    { _gtmCatalogName = pCatalogName_,
      _gtmDatabaseName = pDatabaseName_,
      _gtmTableName = pTableName_
    }

-- | The name of the data catalog that contains the database and table metadata to return.
gtmCatalogName :: Lens' GetTableMetadata Text
gtmCatalogName = lens _gtmCatalogName (\s a -> s {_gtmCatalogName = a})

-- | The name of the database that contains the table metadata to return.
gtmDatabaseName :: Lens' GetTableMetadata Text
gtmDatabaseName = lens _gtmDatabaseName (\s a -> s {_gtmDatabaseName = a})

-- | The name of the table for which metadata is returned.
gtmTableName :: Lens' GetTableMetadata Text
gtmTableName = lens _gtmTableName (\s a -> s {_gtmTableName = a})

instance AWSRequest GetTableMetadata where
  type Rs GetTableMetadata = GetTableMetadataResponse
  request = postJSON athena
  response =
    receiveJSON
      ( \s h x ->
          GetTableMetadataResponse'
            <$> (x .?> "TableMetadata") <*> (pure (fromEnum s))
      )

instance Hashable GetTableMetadata

instance NFData GetTableMetadata

instance ToHeaders GetTableMetadata where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AmazonAthena.GetTableMetadata" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetTableMetadata where
  toJSON GetTableMetadata' {..} =
    object
      ( catMaybes
          [ Just ("CatalogName" .= _gtmCatalogName),
            Just ("DatabaseName" .= _gtmDatabaseName),
            Just ("TableName" .= _gtmTableName)
          ]
      )

instance ToPath GetTableMetadata where
  toPath = const "/"

instance ToQuery GetTableMetadata where
  toQuery = const mempty

-- | /See:/ 'getTableMetadataResponse' smart constructor.
data GetTableMetadataResponse = GetTableMetadataResponse'
  { _gtmrsTableMetadata ::
      !(Maybe TableMetadata),
    _gtmrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetTableMetadataResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtmrsTableMetadata' - An object that contains table metadata.
--
-- * 'gtmrsResponseStatus' - -- | The response status code.
getTableMetadataResponse ::
  -- | 'gtmrsResponseStatus'
  Int ->
  GetTableMetadataResponse
getTableMetadataResponse pResponseStatus_ =
  GetTableMetadataResponse'
    { _gtmrsTableMetadata = Nothing,
      _gtmrsResponseStatus = pResponseStatus_
    }

-- | An object that contains table metadata.
gtmrsTableMetadata :: Lens' GetTableMetadataResponse (Maybe TableMetadata)
gtmrsTableMetadata = lens _gtmrsTableMetadata (\s a -> s {_gtmrsTableMetadata = a})

-- | -- | The response status code.
gtmrsResponseStatus :: Lens' GetTableMetadataResponse Int
gtmrsResponseStatus = lens _gtmrsResponseStatus (\s a -> s {_gtmrsResponseStatus = a})

instance NFData GetTableMetadataResponse
