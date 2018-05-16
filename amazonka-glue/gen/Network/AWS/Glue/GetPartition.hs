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
-- Module      : Network.AWS.Glue.GetPartition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a specified partition.
--
--
module Network.AWS.Glue.GetPartition
    (
    -- * Creating a Request
      getPartition
    , GetPartition
    -- * Request Lenses
    , gpCatalogId
    , gpDatabaseName
    , gpTableName
    , gpPartitionValues

    -- * Destructuring the Response
    , getPartitionResponse
    , GetPartitionResponse
    -- * Response Lenses
    , gprsPartition
    , gprsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getPartition' smart constructor.
data GetPartition = GetPartition'
  { _gpCatalogId       :: !(Maybe Text)
  , _gpDatabaseName    :: !Text
  , _gpTableName       :: !Text
  , _gpPartitionValues :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetPartition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpCatalogId' - The ID of the Data Catalog where the partition in question resides. If none is supplied, the AWS account ID is used by default.
--
-- * 'gpDatabaseName' - The name of the catalog database where the partition resides.
--
-- * 'gpTableName' - The name of the partition's table.
--
-- * 'gpPartitionValues' - The values that define the partition.
getPartition
    :: Text -- ^ 'gpDatabaseName'
    -> Text -- ^ 'gpTableName'
    -> GetPartition
getPartition pDatabaseName_ pTableName_ =
  GetPartition'
    { _gpCatalogId = Nothing
    , _gpDatabaseName = pDatabaseName_
    , _gpTableName = pTableName_
    , _gpPartitionValues = mempty
    }


-- | The ID of the Data Catalog where the partition in question resides. If none is supplied, the AWS account ID is used by default.
gpCatalogId :: Lens' GetPartition (Maybe Text)
gpCatalogId = lens _gpCatalogId (\ s a -> s{_gpCatalogId = a})

-- | The name of the catalog database where the partition resides.
gpDatabaseName :: Lens' GetPartition Text
gpDatabaseName = lens _gpDatabaseName (\ s a -> s{_gpDatabaseName = a})

-- | The name of the partition's table.
gpTableName :: Lens' GetPartition Text
gpTableName = lens _gpTableName (\ s a -> s{_gpTableName = a})

-- | The values that define the partition.
gpPartitionValues :: Lens' GetPartition [Text]
gpPartitionValues = lens _gpPartitionValues (\ s a -> s{_gpPartitionValues = a}) . _Coerce

instance AWSRequest GetPartition where
        type Rs GetPartition = GetPartitionResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 GetPartitionResponse' <$>
                   (x .?> "Partition") <*> (pure (fromEnum s)))

instance Hashable GetPartition where

instance NFData GetPartition where

instance ToHeaders GetPartition where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.GetPartition" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetPartition where
        toJSON GetPartition'{..}
          = object
              (catMaybes
                 [("CatalogId" .=) <$> _gpCatalogId,
                  Just ("DatabaseName" .= _gpDatabaseName),
                  Just ("TableName" .= _gpTableName),
                  Just ("PartitionValues" .= _gpPartitionValues)])

instance ToPath GetPartition where
        toPath = const "/"

instance ToQuery GetPartition where
        toQuery = const mempty

-- | /See:/ 'getPartitionResponse' smart constructor.
data GetPartitionResponse = GetPartitionResponse'
  { _gprsPartition      :: !(Maybe Partition)
  , _gprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetPartitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gprsPartition' - The requested information, in the form of a @Partition@ object.
--
-- * 'gprsResponseStatus' - -- | The response status code.
getPartitionResponse
    :: Int -- ^ 'gprsResponseStatus'
    -> GetPartitionResponse
getPartitionResponse pResponseStatus_ =
  GetPartitionResponse'
    {_gprsPartition = Nothing, _gprsResponseStatus = pResponseStatus_}


-- | The requested information, in the form of a @Partition@ object.
gprsPartition :: Lens' GetPartitionResponse (Maybe Partition)
gprsPartition = lens _gprsPartition (\ s a -> s{_gprsPartition = a})

-- | -- | The response status code.
gprsResponseStatus :: Lens' GetPartitionResponse Int
gprsResponseStatus = lens _gprsResponseStatus (\ s a -> s{_gprsResponseStatus = a})

instance NFData GetPartitionResponse where
