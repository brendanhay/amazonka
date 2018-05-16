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
-- Module      : Network.AWS.Glue.CreatePartition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new partition.
--
--
module Network.AWS.Glue.CreatePartition
    (
    -- * Creating a Request
      createPartition
    , CreatePartition
    -- * Request Lenses
    , cpCatalogId
    , cpDatabaseName
    , cpTableName
    , cpPartitionInput

    -- * Destructuring the Response
    , createPartitionResponse
    , CreatePartitionResponse
    -- * Response Lenses
    , cprsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createPartition' smart constructor.
data CreatePartition = CreatePartition'
  { _cpCatalogId      :: !(Maybe Text)
  , _cpDatabaseName   :: !Text
  , _cpTableName      :: !Text
  , _cpPartitionInput :: !PartitionInput
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreatePartition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpCatalogId' - The ID of the catalog in which the partion is to be created. Currently, this should be the AWS account ID.
--
-- * 'cpDatabaseName' - The name of the metadata database in which the partition is to be created.
--
-- * 'cpTableName' - The name of the metadata table in which the partition is to be created.
--
-- * 'cpPartitionInput' - A @PartitionInput@ structure defining the partition to be created.
createPartition
    :: Text -- ^ 'cpDatabaseName'
    -> Text -- ^ 'cpTableName'
    -> PartitionInput -- ^ 'cpPartitionInput'
    -> CreatePartition
createPartition pDatabaseName_ pTableName_ pPartitionInput_ =
  CreatePartition'
    { _cpCatalogId = Nothing
    , _cpDatabaseName = pDatabaseName_
    , _cpTableName = pTableName_
    , _cpPartitionInput = pPartitionInput_
    }


-- | The ID of the catalog in which the partion is to be created. Currently, this should be the AWS account ID.
cpCatalogId :: Lens' CreatePartition (Maybe Text)
cpCatalogId = lens _cpCatalogId (\ s a -> s{_cpCatalogId = a})

-- | The name of the metadata database in which the partition is to be created.
cpDatabaseName :: Lens' CreatePartition Text
cpDatabaseName = lens _cpDatabaseName (\ s a -> s{_cpDatabaseName = a})

-- | The name of the metadata table in which the partition is to be created.
cpTableName :: Lens' CreatePartition Text
cpTableName = lens _cpTableName (\ s a -> s{_cpTableName = a})

-- | A @PartitionInput@ structure defining the partition to be created.
cpPartitionInput :: Lens' CreatePartition PartitionInput
cpPartitionInput = lens _cpPartitionInput (\ s a -> s{_cpPartitionInput = a})

instance AWSRequest CreatePartition where
        type Rs CreatePartition = CreatePartitionResponse
        request = postJSON glue
        response
          = receiveEmpty
              (\ s h x ->
                 CreatePartitionResponse' <$> (pure (fromEnum s)))

instance Hashable CreatePartition where

instance NFData CreatePartition where

instance ToHeaders CreatePartition where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.CreatePartition" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreatePartition where
        toJSON CreatePartition'{..}
          = object
              (catMaybes
                 [("CatalogId" .=) <$> _cpCatalogId,
                  Just ("DatabaseName" .= _cpDatabaseName),
                  Just ("TableName" .= _cpTableName),
                  Just ("PartitionInput" .= _cpPartitionInput)])

instance ToPath CreatePartition where
        toPath = const "/"

instance ToQuery CreatePartition where
        toQuery = const mempty

-- | /See:/ 'createPartitionResponse' smart constructor.
newtype CreatePartitionResponse = CreatePartitionResponse'
  { _cprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreatePartitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cprsResponseStatus' - -- | The response status code.
createPartitionResponse
    :: Int -- ^ 'cprsResponseStatus'
    -> CreatePartitionResponse
createPartitionResponse pResponseStatus_ =
  CreatePartitionResponse' {_cprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
cprsResponseStatus :: Lens' CreatePartitionResponse Int
cprsResponseStatus = lens _cprsResponseStatus (\ s a -> s{_cprsResponseStatus = a})

instance NFData CreatePartitionResponse where
