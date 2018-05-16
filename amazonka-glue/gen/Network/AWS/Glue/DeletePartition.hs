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
-- Module      : Network.AWS.Glue.DeletePartition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified partition.
--
--
module Network.AWS.Glue.DeletePartition
    (
    -- * Creating a Request
      deletePartition
    , DeletePartition
    -- * Request Lenses
    , dpCatalogId
    , dpDatabaseName
    , dpTableName
    , dpPartitionValues

    -- * Destructuring the Response
    , deletePartitionResponse
    , DeletePartitionResponse
    -- * Response Lenses
    , dprsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deletePartition' smart constructor.
data DeletePartition = DeletePartition'
  { _dpCatalogId       :: !(Maybe Text)
  , _dpDatabaseName    :: !Text
  , _dpTableName       :: !Text
  , _dpPartitionValues :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeletePartition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpCatalogId' - The ID of the Data Catalog where the partition to be deleted resides. If none is supplied, the AWS account ID is used by default.
--
-- * 'dpDatabaseName' - The name of the catalog database in which the table in question resides.
--
-- * 'dpTableName' - The name of the table where the partition to be deleted is located.
--
-- * 'dpPartitionValues' - The values that define the partition.
deletePartition
    :: Text -- ^ 'dpDatabaseName'
    -> Text -- ^ 'dpTableName'
    -> DeletePartition
deletePartition pDatabaseName_ pTableName_ =
  DeletePartition'
    { _dpCatalogId = Nothing
    , _dpDatabaseName = pDatabaseName_
    , _dpTableName = pTableName_
    , _dpPartitionValues = mempty
    }


-- | The ID of the Data Catalog where the partition to be deleted resides. If none is supplied, the AWS account ID is used by default.
dpCatalogId :: Lens' DeletePartition (Maybe Text)
dpCatalogId = lens _dpCatalogId (\ s a -> s{_dpCatalogId = a})

-- | The name of the catalog database in which the table in question resides.
dpDatabaseName :: Lens' DeletePartition Text
dpDatabaseName = lens _dpDatabaseName (\ s a -> s{_dpDatabaseName = a})

-- | The name of the table where the partition to be deleted is located.
dpTableName :: Lens' DeletePartition Text
dpTableName = lens _dpTableName (\ s a -> s{_dpTableName = a})

-- | The values that define the partition.
dpPartitionValues :: Lens' DeletePartition [Text]
dpPartitionValues = lens _dpPartitionValues (\ s a -> s{_dpPartitionValues = a}) . _Coerce

instance AWSRequest DeletePartition where
        type Rs DeletePartition = DeletePartitionResponse
        request = postJSON glue
        response
          = receiveEmpty
              (\ s h x ->
                 DeletePartitionResponse' <$> (pure (fromEnum s)))

instance Hashable DeletePartition where

instance NFData DeletePartition where

instance ToHeaders DeletePartition where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.DeletePartition" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeletePartition where
        toJSON DeletePartition'{..}
          = object
              (catMaybes
                 [("CatalogId" .=) <$> _dpCatalogId,
                  Just ("DatabaseName" .= _dpDatabaseName),
                  Just ("TableName" .= _dpTableName),
                  Just ("PartitionValues" .= _dpPartitionValues)])

instance ToPath DeletePartition where
        toPath = const "/"

instance ToQuery DeletePartition where
        toQuery = const mempty

-- | /See:/ 'deletePartitionResponse' smart constructor.
newtype DeletePartitionResponse = DeletePartitionResponse'
  { _dprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeletePartitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dprsResponseStatus' - -- | The response status code.
deletePartitionResponse
    :: Int -- ^ 'dprsResponseStatus'
    -> DeletePartitionResponse
deletePartitionResponse pResponseStatus_ =
  DeletePartitionResponse' {_dprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dprsResponseStatus :: Lens' DeletePartitionResponse Int
dprsResponseStatus = lens _dprsResponseStatus (\ s a -> s{_dprsResponseStatus = a})

instance NFData DeletePartitionResponse where
