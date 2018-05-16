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
-- Module      : Network.AWS.SMS.DeleteServerCatalog
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The DeleteServerCatalog API clears all servers from your server catalog. This means that these servers will no longer be accessible to the Server Migration Service.
module Network.AWS.SMS.DeleteServerCatalog
    (
    -- * Creating a Request
      deleteServerCatalog
    , DeleteServerCatalog

    -- * Destructuring the Response
    , deleteServerCatalogResponse
    , DeleteServerCatalogResponse
    -- * Response Lenses
    , dscrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SMS.Types
import Network.AWS.SMS.Types.Product

-- | /See:/ 'deleteServerCatalog' smart constructor.
data DeleteServerCatalog =
  DeleteServerCatalog'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteServerCatalog' with the minimum fields required to make a request.
--
deleteServerCatalog
    :: DeleteServerCatalog
deleteServerCatalog = DeleteServerCatalog'


instance AWSRequest DeleteServerCatalog where
        type Rs DeleteServerCatalog =
             DeleteServerCatalogResponse
        request = postJSON sms
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteServerCatalogResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteServerCatalog where

instance NFData DeleteServerCatalog where

instance ToHeaders DeleteServerCatalog where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSServerMigrationService_V2016_10_24.DeleteServerCatalog"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteServerCatalog where
        toJSON = const (Object mempty)

instance ToPath DeleteServerCatalog where
        toPath = const "/"

instance ToQuery DeleteServerCatalog where
        toQuery = const mempty

-- | /See:/ 'deleteServerCatalogResponse' smart constructor.
newtype DeleteServerCatalogResponse = DeleteServerCatalogResponse'
  { _dscrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteServerCatalogResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscrsResponseStatus' - -- | The response status code.
deleteServerCatalogResponse
    :: Int -- ^ 'dscrsResponseStatus'
    -> DeleteServerCatalogResponse
deleteServerCatalogResponse pResponseStatus_ =
  DeleteServerCatalogResponse' {_dscrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dscrsResponseStatus :: Lens' DeleteServerCatalogResponse Int
dscrsResponseStatus = lens _dscrsResponseStatus (\ s a -> s{_dscrsResponseStatus = a})

instance NFData DeleteServerCatalogResponse where
