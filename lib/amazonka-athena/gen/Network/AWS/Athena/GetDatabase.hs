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
-- Module      : Network.AWS.Athena.GetDatabase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a database object for the specfied database and data catalog.
module Network.AWS.Athena.GetDatabase
  ( -- * Creating a Request
    getDatabase,
    GetDatabase,

    -- * Request Lenses
    gdCatalogName,
    gdDatabaseName,

    -- * Destructuring the Response
    getDatabaseResponse,
    GetDatabaseResponse,

    -- * Response Lenses
    gdrsDatabase,
    gdrsResponseStatus,
  )
where

import Network.AWS.Athena.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getDatabase' smart constructor.
data GetDatabase = GetDatabase'
  { _gdCatalogName :: !Text,
    _gdDatabaseName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetDatabase' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdCatalogName' - The name of the data catalog that contains the database to return.
--
-- * 'gdDatabaseName' - The name of the database to return.
getDatabase ::
  -- | 'gdCatalogName'
  Text ->
  -- | 'gdDatabaseName'
  Text ->
  GetDatabase
getDatabase pCatalogName_ pDatabaseName_ =
  GetDatabase'
    { _gdCatalogName = pCatalogName_,
      _gdDatabaseName = pDatabaseName_
    }

-- | The name of the data catalog that contains the database to return.
gdCatalogName :: Lens' GetDatabase Text
gdCatalogName = lens _gdCatalogName (\s a -> s {_gdCatalogName = a})

-- | The name of the database to return.
gdDatabaseName :: Lens' GetDatabase Text
gdDatabaseName = lens _gdDatabaseName (\s a -> s {_gdDatabaseName = a})

instance AWSRequest GetDatabase where
  type Rs GetDatabase = GetDatabaseResponse
  request = postJSON athena
  response =
    receiveJSON
      ( \s h x ->
          GetDatabaseResponse'
            <$> (x .?> "Database") <*> (pure (fromEnum s))
      )

instance Hashable GetDatabase

instance NFData GetDatabase

instance ToHeaders GetDatabase where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AmazonAthena.GetDatabase" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetDatabase where
  toJSON GetDatabase' {..} =
    object
      ( catMaybes
          [ Just ("CatalogName" .= _gdCatalogName),
            Just ("DatabaseName" .= _gdDatabaseName)
          ]
      )

instance ToPath GetDatabase where
  toPath = const "/"

instance ToQuery GetDatabase where
  toQuery = const mempty

-- | /See:/ 'getDatabaseResponse' smart constructor.
data GetDatabaseResponse = GetDatabaseResponse'
  { _gdrsDatabase ::
      !(Maybe Database),
    _gdrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetDatabaseResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdrsDatabase' - The database returned.
--
-- * 'gdrsResponseStatus' - -- | The response status code.
getDatabaseResponse ::
  -- | 'gdrsResponseStatus'
  Int ->
  GetDatabaseResponse
getDatabaseResponse pResponseStatus_ =
  GetDatabaseResponse'
    { _gdrsDatabase = Nothing,
      _gdrsResponseStatus = pResponseStatus_
    }

-- | The database returned.
gdrsDatabase :: Lens' GetDatabaseResponse (Maybe Database)
gdrsDatabase = lens _gdrsDatabase (\s a -> s {_gdrsDatabase = a})

-- | -- | The response status code.
gdrsResponseStatus :: Lens' GetDatabaseResponse Int
gdrsResponseStatus = lens _gdrsResponseStatus (\s a -> s {_gdrsResponseStatus = a})

instance NFData GetDatabaseResponse
