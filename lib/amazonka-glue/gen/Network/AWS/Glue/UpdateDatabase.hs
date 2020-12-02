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
-- Module      : Network.AWS.Glue.UpdateDatabase
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing database definition in a Data Catalog.
--
--
module Network.AWS.Glue.UpdateDatabase
    (
    -- * Creating a Request
      updateDatabase
    , UpdateDatabase
    -- * Request Lenses
    , udCatalogId
    , udName
    , udDatabaseInput

    -- * Destructuring the Response
    , updateDatabaseResponse
    , UpdateDatabaseResponse
    -- * Response Lenses
    , udrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateDatabase' smart constructor.
data UpdateDatabase = UpdateDatabase'
  { _udCatalogId     :: !(Maybe Text)
  , _udName          :: !Text
  , _udDatabaseInput :: !DatabaseInput
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDatabase' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udCatalogId' - The ID of the Data Catalog in which the metadata database resides. If none is supplied, the AWS account ID is used by default.
--
-- * 'udName' - The name of the database to update in the catalog. For Hive compatibility, this is folded to lowercase.
--
-- * 'udDatabaseInput' - A @DatabaseInput@ object specifying the new definition of the metadata database in the catalog.
updateDatabase
    :: Text -- ^ 'udName'
    -> DatabaseInput -- ^ 'udDatabaseInput'
    -> UpdateDatabase
updateDatabase pName_ pDatabaseInput_ =
  UpdateDatabase'
    { _udCatalogId = Nothing
    , _udName = pName_
    , _udDatabaseInput = pDatabaseInput_
    }


-- | The ID of the Data Catalog in which the metadata database resides. If none is supplied, the AWS account ID is used by default.
udCatalogId :: Lens' UpdateDatabase (Maybe Text)
udCatalogId = lens _udCatalogId (\ s a -> s{_udCatalogId = a})

-- | The name of the database to update in the catalog. For Hive compatibility, this is folded to lowercase.
udName :: Lens' UpdateDatabase Text
udName = lens _udName (\ s a -> s{_udName = a})

-- | A @DatabaseInput@ object specifying the new definition of the metadata database in the catalog.
udDatabaseInput :: Lens' UpdateDatabase DatabaseInput
udDatabaseInput = lens _udDatabaseInput (\ s a -> s{_udDatabaseInput = a})

instance AWSRequest UpdateDatabase where
        type Rs UpdateDatabase = UpdateDatabaseResponse
        request = postJSON glue
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateDatabaseResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateDatabase where

instance NFData UpdateDatabase where

instance ToHeaders UpdateDatabase where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.UpdateDatabase" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateDatabase where
        toJSON UpdateDatabase'{..}
          = object
              (catMaybes
                 [("CatalogId" .=) <$> _udCatalogId,
                  Just ("Name" .= _udName),
                  Just ("DatabaseInput" .= _udDatabaseInput)])

instance ToPath UpdateDatabase where
        toPath = const "/"

instance ToQuery UpdateDatabase where
        toQuery = const mempty

-- | /See:/ 'updateDatabaseResponse' smart constructor.
newtype UpdateDatabaseResponse = UpdateDatabaseResponse'
  { _udrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDatabaseResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udrsResponseStatus' - -- | The response status code.
updateDatabaseResponse
    :: Int -- ^ 'udrsResponseStatus'
    -> UpdateDatabaseResponse
updateDatabaseResponse pResponseStatus_ =
  UpdateDatabaseResponse' {_udrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
udrsResponseStatus :: Lens' UpdateDatabaseResponse Int
udrsResponseStatus = lens _udrsResponseStatus (\ s a -> s{_udrsResponseStatus = a})

instance NFData UpdateDatabaseResponse where
