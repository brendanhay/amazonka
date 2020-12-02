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
-- Module      : Network.AWS.SMS.ImportServerCatalog
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gathers a complete list of on-premises servers. Connectors must be installed and monitoring all servers to import.
--
--
-- This call returns immediately, but might take additional time to retrieve all the servers.
module Network.AWS.SMS.ImportServerCatalog
  ( -- * Creating a Request
    importServerCatalog,
    ImportServerCatalog,

    -- * Destructuring the Response
    importServerCatalogResponse,
    ImportServerCatalogResponse,

    -- * Response Lenses
    iscrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SMS.Types

-- | /See:/ 'importServerCatalog' smart constructor.
data ImportServerCatalog = ImportServerCatalog'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ImportServerCatalog' with the minimum fields required to make a request.
importServerCatalog ::
  ImportServerCatalog
importServerCatalog = ImportServerCatalog'

instance AWSRequest ImportServerCatalog where
  type Rs ImportServerCatalog = ImportServerCatalogResponse
  request = postJSON sms
  response =
    receiveEmpty
      (\s h x -> ImportServerCatalogResponse' <$> (pure (fromEnum s)))

instance Hashable ImportServerCatalog

instance NFData ImportServerCatalog

instance ToHeaders ImportServerCatalog where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSServerMigrationService_V2016_10_24.ImportServerCatalog" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ImportServerCatalog where
  toJSON = const (Object mempty)

instance ToPath ImportServerCatalog where
  toPath = const "/"

instance ToQuery ImportServerCatalog where
  toQuery = const mempty

-- | /See:/ 'importServerCatalogResponse' smart constructor.
newtype ImportServerCatalogResponse = ImportServerCatalogResponse'
  { _iscrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ImportServerCatalogResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iscrsResponseStatus' - -- | The response status code.
importServerCatalogResponse ::
  -- | 'iscrsResponseStatus'
  Int ->
  ImportServerCatalogResponse
importServerCatalogResponse pResponseStatus_ =
  ImportServerCatalogResponse'
    { _iscrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
iscrsResponseStatus :: Lens' ImportServerCatalogResponse Int
iscrsResponseStatus = lens _iscrsResponseStatus (\s a -> s {_iscrsResponseStatus = a})

instance NFData ImportServerCatalogResponse
