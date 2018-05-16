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
-- Module      : Network.AWS.Glue.BatchDeletePartition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more partitions in a batch operation.
--
--
module Network.AWS.Glue.BatchDeletePartition
    (
    -- * Creating a Request
      batchDeletePartition
    , BatchDeletePartition
    -- * Request Lenses
    , bdpCatalogId
    , bdpDatabaseName
    , bdpTableName
    , bdpPartitionsToDelete

    -- * Destructuring the Response
    , batchDeletePartitionResponse
    , BatchDeletePartitionResponse
    -- * Response Lenses
    , bdprsErrors
    , bdprsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'batchDeletePartition' smart constructor.
data BatchDeletePartition = BatchDeletePartition'
  { _bdpCatalogId          :: !(Maybe Text)
  , _bdpDatabaseName       :: !Text
  , _bdpTableName          :: !Text
  , _bdpPartitionsToDelete :: ![PartitionValueList]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchDeletePartition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdpCatalogId' - The ID of the Data Catalog where the partition to be deleted resides. If none is supplied, the AWS account ID is used by default.
--
-- * 'bdpDatabaseName' - The name of the catalog database in which the table in question resides.
--
-- * 'bdpTableName' - The name of the table where the partitions to be deleted is located.
--
-- * 'bdpPartitionsToDelete' - A list of @PartitionInput@ structures that define the partitions to be deleted.
batchDeletePartition
    :: Text -- ^ 'bdpDatabaseName'
    -> Text -- ^ 'bdpTableName'
    -> BatchDeletePartition
batchDeletePartition pDatabaseName_ pTableName_ =
  BatchDeletePartition'
    { _bdpCatalogId = Nothing
    , _bdpDatabaseName = pDatabaseName_
    , _bdpTableName = pTableName_
    , _bdpPartitionsToDelete = mempty
    }


-- | The ID of the Data Catalog where the partition to be deleted resides. If none is supplied, the AWS account ID is used by default.
bdpCatalogId :: Lens' BatchDeletePartition (Maybe Text)
bdpCatalogId = lens _bdpCatalogId (\ s a -> s{_bdpCatalogId = a})

-- | The name of the catalog database in which the table in question resides.
bdpDatabaseName :: Lens' BatchDeletePartition Text
bdpDatabaseName = lens _bdpDatabaseName (\ s a -> s{_bdpDatabaseName = a})

-- | The name of the table where the partitions to be deleted is located.
bdpTableName :: Lens' BatchDeletePartition Text
bdpTableName = lens _bdpTableName (\ s a -> s{_bdpTableName = a})

-- | A list of @PartitionInput@ structures that define the partitions to be deleted.
bdpPartitionsToDelete :: Lens' BatchDeletePartition [PartitionValueList]
bdpPartitionsToDelete = lens _bdpPartitionsToDelete (\ s a -> s{_bdpPartitionsToDelete = a}) . _Coerce

instance AWSRequest BatchDeletePartition where
        type Rs BatchDeletePartition =
             BatchDeletePartitionResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 BatchDeletePartitionResponse' <$>
                   (x .?> "Errors" .!@ mempty) <*> (pure (fromEnum s)))

instance Hashable BatchDeletePartition where

instance NFData BatchDeletePartition where

instance ToHeaders BatchDeletePartition where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.BatchDeletePartition" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON BatchDeletePartition where
        toJSON BatchDeletePartition'{..}
          = object
              (catMaybes
                 [("CatalogId" .=) <$> _bdpCatalogId,
                  Just ("DatabaseName" .= _bdpDatabaseName),
                  Just ("TableName" .= _bdpTableName),
                  Just
                    ("PartitionsToDelete" .= _bdpPartitionsToDelete)])

instance ToPath BatchDeletePartition where
        toPath = const "/"

instance ToQuery BatchDeletePartition where
        toQuery = const mempty

-- | /See:/ 'batchDeletePartitionResponse' smart constructor.
data BatchDeletePartitionResponse = BatchDeletePartitionResponse'
  { _bdprsErrors         :: !(Maybe [PartitionError])
  , _bdprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchDeletePartitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdprsErrors' - Errors encountered when trying to delete the requested partitions.
--
-- * 'bdprsResponseStatus' - -- | The response status code.
batchDeletePartitionResponse
    :: Int -- ^ 'bdprsResponseStatus'
    -> BatchDeletePartitionResponse
batchDeletePartitionResponse pResponseStatus_ =
  BatchDeletePartitionResponse'
    {_bdprsErrors = Nothing, _bdprsResponseStatus = pResponseStatus_}


-- | Errors encountered when trying to delete the requested partitions.
bdprsErrors :: Lens' BatchDeletePartitionResponse [PartitionError]
bdprsErrors = lens _bdprsErrors (\ s a -> s{_bdprsErrors = a}) . _Default . _Coerce

-- | -- | The response status code.
bdprsResponseStatus :: Lens' BatchDeletePartitionResponse Int
bdprsResponseStatus = lens _bdprsResponseStatus (\ s a -> s{_bdprsResponseStatus = a})

instance NFData BatchDeletePartitionResponse where
