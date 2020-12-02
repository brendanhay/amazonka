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
-- Module      : Network.AWS.IoTAnalytics.DeleteDatastore
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified data store.
--
--
module Network.AWS.IoTAnalytics.DeleteDatastore
    (
    -- * Creating a Request
      deleteDatastore
    , DeleteDatastore
    -- * Request Lenses
    , ddDatastoreName

    -- * Destructuring the Response
    , deleteDatastoreResponse
    , DeleteDatastoreResponse
    ) where

import Network.AWS.IoTAnalytics.Types
import Network.AWS.IoTAnalytics.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteDatastore' smart constructor.
newtype DeleteDatastore = DeleteDatastore'
  { _ddDatastoreName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDatastore' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddDatastoreName' - The name of the data store to delete.
deleteDatastore
    :: Text -- ^ 'ddDatastoreName'
    -> DeleteDatastore
deleteDatastore pDatastoreName_ =
  DeleteDatastore' {_ddDatastoreName = pDatastoreName_}


-- | The name of the data store to delete.
ddDatastoreName :: Lens' DeleteDatastore Text
ddDatastoreName = lens _ddDatastoreName (\ s a -> s{_ddDatastoreName = a})

instance AWSRequest DeleteDatastore where
        type Rs DeleteDatastore = DeleteDatastoreResponse
        request = delete ioTAnalytics
        response = receiveNull DeleteDatastoreResponse'

instance Hashable DeleteDatastore where

instance NFData DeleteDatastore where

instance ToHeaders DeleteDatastore where
        toHeaders = const mempty

instance ToPath DeleteDatastore where
        toPath DeleteDatastore'{..}
          = mconcat ["/datastores/", toBS _ddDatastoreName]

instance ToQuery DeleteDatastore where
        toQuery = const mempty

-- | /See:/ 'deleteDatastoreResponse' smart constructor.
data DeleteDatastoreResponse =
  DeleteDatastoreResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDatastoreResponse' with the minimum fields required to make a request.
--
deleteDatastoreResponse
    :: DeleteDatastoreResponse
deleteDatastoreResponse = DeleteDatastoreResponse'


instance NFData DeleteDatastoreResponse where
