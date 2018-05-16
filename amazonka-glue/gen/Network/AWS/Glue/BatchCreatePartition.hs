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
-- Module      : Network.AWS.Glue.BatchCreatePartition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates one or more partitions in a batch operation.
--
--
module Network.AWS.Glue.BatchCreatePartition
    (
    -- * Creating a Request
      batchCreatePartition
    , BatchCreatePartition
    -- * Request Lenses
    , bcpCatalogId
    , bcpDatabaseName
    , bcpTableName
    , bcpPartitionInputList

    -- * Destructuring the Response
    , batchCreatePartitionResponse
    , BatchCreatePartitionResponse
    -- * Response Lenses
    , bcprsErrors
    , bcprsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'batchCreatePartition' smart constructor.
data BatchCreatePartition = BatchCreatePartition'
  { _bcpCatalogId          :: !(Maybe Text)
  , _bcpDatabaseName       :: !Text
  , _bcpTableName          :: !Text
  , _bcpPartitionInputList :: ![PartitionInput]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchCreatePartition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bcpCatalogId' - The ID of the catalog in which the partion is to be created. Currently, this should be the AWS account ID.
--
-- * 'bcpDatabaseName' - The name of the metadata database in which the partition is to be created.
--
-- * 'bcpTableName' - The name of the metadata table in which the partition is to be created.
--
-- * 'bcpPartitionInputList' - A list of @PartitionInput@ structures that define the partitions to be created.
batchCreatePartition
    :: Text -- ^ 'bcpDatabaseName'
    -> Text -- ^ 'bcpTableName'
    -> BatchCreatePartition
batchCreatePartition pDatabaseName_ pTableName_ =
  BatchCreatePartition'
    { _bcpCatalogId = Nothing
    , _bcpDatabaseName = pDatabaseName_
    , _bcpTableName = pTableName_
    , _bcpPartitionInputList = mempty
    }


-- | The ID of the catalog in which the partion is to be created. Currently, this should be the AWS account ID.
bcpCatalogId :: Lens' BatchCreatePartition (Maybe Text)
bcpCatalogId = lens _bcpCatalogId (\ s a -> s{_bcpCatalogId = a})

-- | The name of the metadata database in which the partition is to be created.
bcpDatabaseName :: Lens' BatchCreatePartition Text
bcpDatabaseName = lens _bcpDatabaseName (\ s a -> s{_bcpDatabaseName = a})

-- | The name of the metadata table in which the partition is to be created.
bcpTableName :: Lens' BatchCreatePartition Text
bcpTableName = lens _bcpTableName (\ s a -> s{_bcpTableName = a})

-- | A list of @PartitionInput@ structures that define the partitions to be created.
bcpPartitionInputList :: Lens' BatchCreatePartition [PartitionInput]
bcpPartitionInputList = lens _bcpPartitionInputList (\ s a -> s{_bcpPartitionInputList = a}) . _Coerce

instance AWSRequest BatchCreatePartition where
        type Rs BatchCreatePartition =
             BatchCreatePartitionResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 BatchCreatePartitionResponse' <$>
                   (x .?> "Errors" .!@ mempty) <*> (pure (fromEnum s)))

instance Hashable BatchCreatePartition where

instance NFData BatchCreatePartition where

instance ToHeaders BatchCreatePartition where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.BatchCreatePartition" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON BatchCreatePartition where
        toJSON BatchCreatePartition'{..}
          = object
              (catMaybes
                 [("CatalogId" .=) <$> _bcpCatalogId,
                  Just ("DatabaseName" .= _bcpDatabaseName),
                  Just ("TableName" .= _bcpTableName),
                  Just
                    ("PartitionInputList" .= _bcpPartitionInputList)])

instance ToPath BatchCreatePartition where
        toPath = const "/"

instance ToQuery BatchCreatePartition where
        toQuery = const mempty

-- | /See:/ 'batchCreatePartitionResponse' smart constructor.
data BatchCreatePartitionResponse = BatchCreatePartitionResponse'
  { _bcprsErrors         :: !(Maybe [PartitionError])
  , _bcprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchCreatePartitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bcprsErrors' - Errors encountered when trying to create the requested partitions.
--
-- * 'bcprsResponseStatus' - -- | The response status code.
batchCreatePartitionResponse
    :: Int -- ^ 'bcprsResponseStatus'
    -> BatchCreatePartitionResponse
batchCreatePartitionResponse pResponseStatus_ =
  BatchCreatePartitionResponse'
    {_bcprsErrors = Nothing, _bcprsResponseStatus = pResponseStatus_}


-- | Errors encountered when trying to create the requested partitions.
bcprsErrors :: Lens' BatchCreatePartitionResponse [PartitionError]
bcprsErrors = lens _bcprsErrors (\ s a -> s{_bcprsErrors = a}) . _Default . _Coerce

-- | -- | The response status code.
bcprsResponseStatus :: Lens' BatchCreatePartitionResponse Int
bcprsResponseStatus = lens _bcprsResponseStatus (\ s a -> s{_bcprsResponseStatus = a})

instance NFData BatchCreatePartitionResponse where
