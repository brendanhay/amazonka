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
-- Module      : Network.AWS.Glue.BatchDeleteTable
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes multiple tables at once.
--
--
module Network.AWS.Glue.BatchDeleteTable
    (
    -- * Creating a Request
      batchDeleteTable
    , BatchDeleteTable
    -- * Request Lenses
    , bdtCatalogId
    , bdtDatabaseName
    , bdtTablesToDelete

    -- * Destructuring the Response
    , batchDeleteTableResponse
    , BatchDeleteTableResponse
    -- * Response Lenses
    , bdtrsErrors
    , bdtrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'batchDeleteTable' smart constructor.
data BatchDeleteTable = BatchDeleteTable'
  { _bdtCatalogId      :: !(Maybe Text)
  , _bdtDatabaseName   :: !Text
  , _bdtTablesToDelete :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchDeleteTable' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdtCatalogId' - The ID of the Data Catalog where the table resides. If none is supplied, the AWS account ID is used by default.
--
-- * 'bdtDatabaseName' - The name of the catalog database where the tables to delete reside. For Hive compatibility, this name is entirely lowercase.
--
-- * 'bdtTablesToDelete' - A list of the table to delete.
batchDeleteTable
    :: Text -- ^ 'bdtDatabaseName'
    -> BatchDeleteTable
batchDeleteTable pDatabaseName_ =
  BatchDeleteTable'
    { _bdtCatalogId = Nothing
    , _bdtDatabaseName = pDatabaseName_
    , _bdtTablesToDelete = mempty
    }


-- | The ID of the Data Catalog where the table resides. If none is supplied, the AWS account ID is used by default.
bdtCatalogId :: Lens' BatchDeleteTable (Maybe Text)
bdtCatalogId = lens _bdtCatalogId (\ s a -> s{_bdtCatalogId = a})

-- | The name of the catalog database where the tables to delete reside. For Hive compatibility, this name is entirely lowercase.
bdtDatabaseName :: Lens' BatchDeleteTable Text
bdtDatabaseName = lens _bdtDatabaseName (\ s a -> s{_bdtDatabaseName = a})

-- | A list of the table to delete.
bdtTablesToDelete :: Lens' BatchDeleteTable [Text]
bdtTablesToDelete = lens _bdtTablesToDelete (\ s a -> s{_bdtTablesToDelete = a}) . _Coerce

instance AWSRequest BatchDeleteTable where
        type Rs BatchDeleteTable = BatchDeleteTableResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 BatchDeleteTableResponse' <$>
                   (x .?> "Errors" .!@ mempty) <*> (pure (fromEnum s)))

instance Hashable BatchDeleteTable where

instance NFData BatchDeleteTable where

instance ToHeaders BatchDeleteTable where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.BatchDeleteTable" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON BatchDeleteTable where
        toJSON BatchDeleteTable'{..}
          = object
              (catMaybes
                 [("CatalogId" .=) <$> _bdtCatalogId,
                  Just ("DatabaseName" .= _bdtDatabaseName),
                  Just ("TablesToDelete" .= _bdtTablesToDelete)])

instance ToPath BatchDeleteTable where
        toPath = const "/"

instance ToQuery BatchDeleteTable where
        toQuery = const mempty

-- | /See:/ 'batchDeleteTableResponse' smart constructor.
data BatchDeleteTableResponse = BatchDeleteTableResponse'
  { _bdtrsErrors         :: !(Maybe [TableError])
  , _bdtrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchDeleteTableResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdtrsErrors' - A list of errors encountered in attempting to delete the specified tables.
--
-- * 'bdtrsResponseStatus' - -- | The response status code.
batchDeleteTableResponse
    :: Int -- ^ 'bdtrsResponseStatus'
    -> BatchDeleteTableResponse
batchDeleteTableResponse pResponseStatus_ =
  BatchDeleteTableResponse'
    {_bdtrsErrors = Nothing, _bdtrsResponseStatus = pResponseStatus_}


-- | A list of errors encountered in attempting to delete the specified tables.
bdtrsErrors :: Lens' BatchDeleteTableResponse [TableError]
bdtrsErrors = lens _bdtrsErrors (\ s a -> s{_bdtrsErrors = a}) . _Default . _Coerce

-- | -- | The response status code.
bdtrsResponseStatus :: Lens' BatchDeleteTableResponse Int
bdtrsResponseStatus = lens _bdtrsResponseStatus (\ s a -> s{_bdtrsResponseStatus = a})

instance NFData BatchDeleteTableResponse where
