{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.ImportServerCatalog
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The ImportServerCatalog API is used to gather the complete list of on-premises servers on your premises. This API call requires connectors to be installed and monitoring all servers you would like imported. This API call returns immediately, but may take some time to retrieve all of the servers.
module Network.AWS.SMS.ImportServerCatalog
    (
    -- * Creating a Request
      importServerCatalog
    , ImportServerCatalog

    -- * Destructuring the Response
    , importServerCatalogResponse
    , ImportServerCatalogResponse
    -- * Response Lenses
    , iscrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SMS.Types
import Network.AWS.SMS.Types.Product

-- | /See:/ 'importServerCatalog' smart constructor.
data ImportServerCatalog =
  ImportServerCatalog'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImportServerCatalog' with the minimum fields required to make a request.
--
importServerCatalog
    :: ImportServerCatalog
importServerCatalog = ImportServerCatalog'


instance AWSRequest ImportServerCatalog where
        type Rs ImportServerCatalog =
             ImportServerCatalogResponse
        request = postJSON sms
        response
          = receiveEmpty
              (\ s h x ->
                 ImportServerCatalogResponse' <$> (pure (fromEnum s)))

instance Hashable ImportServerCatalog where

instance NFData ImportServerCatalog where

instance ToHeaders ImportServerCatalog where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSServerMigrationService_V2016_10_24.ImportServerCatalog"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ImportServerCatalog where
        toJSON = const (Object mempty)

instance ToPath ImportServerCatalog where
        toPath = const "/"

instance ToQuery ImportServerCatalog where
        toQuery = const mempty

-- | /See:/ 'importServerCatalogResponse' smart constructor.
newtype ImportServerCatalogResponse = ImportServerCatalogResponse'
  { _iscrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImportServerCatalogResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iscrsResponseStatus' - -- | The response status code.
importServerCatalogResponse
    :: Int -- ^ 'iscrsResponseStatus'
    -> ImportServerCatalogResponse
importServerCatalogResponse pResponseStatus_ =
  ImportServerCatalogResponse' {_iscrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
iscrsResponseStatus :: Lens' ImportServerCatalogResponse Int
iscrsResponseStatus = lens _iscrsResponseStatus (\ s a -> s{_iscrsResponseStatus = a})

instance NFData ImportServerCatalogResponse where
