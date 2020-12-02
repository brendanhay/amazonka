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
-- Module      : Network.AWS.Athena.GetDataCatalog
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the specified data catalog.
module Network.AWS.Athena.GetDataCatalog
  ( -- * Creating a Request
    getDataCatalog,
    GetDataCatalog,

    -- * Request Lenses
    gdcName,

    -- * Destructuring the Response
    getDataCatalogResponse,
    GetDataCatalogResponse,

    -- * Response Lenses
    gdcrsDataCatalog,
    gdcrsResponseStatus,
  )
where

import Network.AWS.Athena.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getDataCatalog' smart constructor.
newtype GetDataCatalog = GetDataCatalog' {_gdcName :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetDataCatalog' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdcName' - The name of the data catalog to return.
getDataCatalog ::
  -- | 'gdcName'
  Text ->
  GetDataCatalog
getDataCatalog pName_ = GetDataCatalog' {_gdcName = pName_}

-- | The name of the data catalog to return.
gdcName :: Lens' GetDataCatalog Text
gdcName = lens _gdcName (\s a -> s {_gdcName = a})

instance AWSRequest GetDataCatalog where
  type Rs GetDataCatalog = GetDataCatalogResponse
  request = postJSON athena
  response =
    receiveJSON
      ( \s h x ->
          GetDataCatalogResponse'
            <$> (x .?> "DataCatalog") <*> (pure (fromEnum s))
      )

instance Hashable GetDataCatalog

instance NFData GetDataCatalog

instance ToHeaders GetDataCatalog where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AmazonAthena.GetDataCatalog" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetDataCatalog where
  toJSON GetDataCatalog' {..} =
    object (catMaybes [Just ("Name" .= _gdcName)])

instance ToPath GetDataCatalog where
  toPath = const "/"

instance ToQuery GetDataCatalog where
  toQuery = const mempty

-- | /See:/ 'getDataCatalogResponse' smart constructor.
data GetDataCatalogResponse = GetDataCatalogResponse'
  { _gdcrsDataCatalog ::
      !(Maybe DataCatalog),
    _gdcrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetDataCatalogResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdcrsDataCatalog' - The data catalog returned.
--
-- * 'gdcrsResponseStatus' - -- | The response status code.
getDataCatalogResponse ::
  -- | 'gdcrsResponseStatus'
  Int ->
  GetDataCatalogResponse
getDataCatalogResponse pResponseStatus_ =
  GetDataCatalogResponse'
    { _gdcrsDataCatalog = Nothing,
      _gdcrsResponseStatus = pResponseStatus_
    }

-- | The data catalog returned.
gdcrsDataCatalog :: Lens' GetDataCatalogResponse (Maybe DataCatalog)
gdcrsDataCatalog = lens _gdcrsDataCatalog (\s a -> s {_gdcrsDataCatalog = a})

-- | -- | The response status code.
gdcrsResponseStatus :: Lens' GetDataCatalogResponse Int
gdcrsResponseStatus = lens _gdcrsResponseStatus (\s a -> s {_gdcrsResponseStatus = a})

instance NFData GetDataCatalogResponse
