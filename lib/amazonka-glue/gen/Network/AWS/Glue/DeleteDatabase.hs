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
-- Module      : Network.AWS.Glue.DeleteDatabase
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a specified Database from a Data Catalog.
--
--
module Network.AWS.Glue.DeleteDatabase
    (
    -- * Creating a Request
      deleteDatabase
    , DeleteDatabase
    -- * Request Lenses
    , ddCatalogId
    , ddName

    -- * Destructuring the Response
    , deleteDatabaseResponse
    , DeleteDatabaseResponse
    -- * Response Lenses
    , ddrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteDatabase' smart constructor.
data DeleteDatabase = DeleteDatabase'
  { _ddCatalogId :: !(Maybe Text)
  , _ddName      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDatabase' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddCatalogId' - The ID of the Data Catalog in which the database resides. If none is supplied, the AWS account ID is used by default.
--
-- * 'ddName' - The name of the Database to delete. For Hive compatibility, this must be all lowercase.
deleteDatabase
    :: Text -- ^ 'ddName'
    -> DeleteDatabase
deleteDatabase pName_ =
  DeleteDatabase' {_ddCatalogId = Nothing, _ddName = pName_}


-- | The ID of the Data Catalog in which the database resides. If none is supplied, the AWS account ID is used by default.
ddCatalogId :: Lens' DeleteDatabase (Maybe Text)
ddCatalogId = lens _ddCatalogId (\ s a -> s{_ddCatalogId = a})

-- | The name of the Database to delete. For Hive compatibility, this must be all lowercase.
ddName :: Lens' DeleteDatabase Text
ddName = lens _ddName (\ s a -> s{_ddName = a})

instance AWSRequest DeleteDatabase where
        type Rs DeleteDatabase = DeleteDatabaseResponse
        request = postJSON glue
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteDatabaseResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteDatabase where

instance NFData DeleteDatabase where

instance ToHeaders DeleteDatabase where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.DeleteDatabase" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteDatabase where
        toJSON DeleteDatabase'{..}
          = object
              (catMaybes
                 [("CatalogId" .=) <$> _ddCatalogId,
                  Just ("Name" .= _ddName)])

instance ToPath DeleteDatabase where
        toPath = const "/"

instance ToQuery DeleteDatabase where
        toQuery = const mempty

-- | /See:/ 'deleteDatabaseResponse' smart constructor.
newtype DeleteDatabaseResponse = DeleteDatabaseResponse'
  { _ddrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDatabaseResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddrsResponseStatus' - -- | The response status code.
deleteDatabaseResponse
    :: Int -- ^ 'ddrsResponseStatus'
    -> DeleteDatabaseResponse
deleteDatabaseResponse pResponseStatus_ =
  DeleteDatabaseResponse' {_ddrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ddrsResponseStatus :: Lens' DeleteDatabaseResponse Int
ddrsResponseStatus = lens _ddrsResponseStatus (\ s a -> s{_ddrsResponseStatus = a})

instance NFData DeleteDatabaseResponse where
