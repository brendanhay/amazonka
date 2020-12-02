{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.CreatePartitionIndex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a specified partition index in an existing table.
module Network.AWS.Glue.CreatePartitionIndex
  ( -- * Creating a Request
    createPartitionIndex,
    CreatePartitionIndex,

    -- * Request Lenses
    cpiCatalogId,
    cpiDatabaseName,
    cpiTableName,
    cpiPartitionIndex,

    -- * Destructuring the Response
    createPartitionIndexResponse,
    CreatePartitionIndexResponse,

    -- * Response Lenses
    cpirsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createPartitionIndex' smart constructor.
data CreatePartitionIndex = CreatePartitionIndex'
  { _cpiCatalogId ::
      !(Maybe Text),
    _cpiDatabaseName :: !Text,
    _cpiTableName :: !Text,
    _cpiPartitionIndex :: !PartitionIndex
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreatePartitionIndex' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpiCatalogId' - The catalog ID where the table resides.
--
-- * 'cpiDatabaseName' - Specifies the name of a database in which you want to create a partition index.
--
-- * 'cpiTableName' - Specifies the name of a table in which you want to create a partition index.
--
-- * 'cpiPartitionIndex' - Specifies a @PartitionIndex@ structure to create a partition index in an existing table.
createPartitionIndex ::
  -- | 'cpiDatabaseName'
  Text ->
  -- | 'cpiTableName'
  Text ->
  -- | 'cpiPartitionIndex'
  PartitionIndex ->
  CreatePartitionIndex
createPartitionIndex pDatabaseName_ pTableName_ pPartitionIndex_ =
  CreatePartitionIndex'
    { _cpiCatalogId = Nothing,
      _cpiDatabaseName = pDatabaseName_,
      _cpiTableName = pTableName_,
      _cpiPartitionIndex = pPartitionIndex_
    }

-- | The catalog ID where the table resides.
cpiCatalogId :: Lens' CreatePartitionIndex (Maybe Text)
cpiCatalogId = lens _cpiCatalogId (\s a -> s {_cpiCatalogId = a})

-- | Specifies the name of a database in which you want to create a partition index.
cpiDatabaseName :: Lens' CreatePartitionIndex Text
cpiDatabaseName = lens _cpiDatabaseName (\s a -> s {_cpiDatabaseName = a})

-- | Specifies the name of a table in which you want to create a partition index.
cpiTableName :: Lens' CreatePartitionIndex Text
cpiTableName = lens _cpiTableName (\s a -> s {_cpiTableName = a})

-- | Specifies a @PartitionIndex@ structure to create a partition index in an existing table.
cpiPartitionIndex :: Lens' CreatePartitionIndex PartitionIndex
cpiPartitionIndex = lens _cpiPartitionIndex (\s a -> s {_cpiPartitionIndex = a})

instance AWSRequest CreatePartitionIndex where
  type Rs CreatePartitionIndex = CreatePartitionIndexResponse
  request = postJSON glue
  response =
    receiveEmpty
      (\s h x -> CreatePartitionIndexResponse' <$> (pure (fromEnum s)))

instance Hashable CreatePartitionIndex

instance NFData CreatePartitionIndex

instance ToHeaders CreatePartitionIndex where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.CreatePartitionIndex" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreatePartitionIndex where
  toJSON CreatePartitionIndex' {..} =
    object
      ( catMaybes
          [ ("CatalogId" .=) <$> _cpiCatalogId,
            Just ("DatabaseName" .= _cpiDatabaseName),
            Just ("TableName" .= _cpiTableName),
            Just ("PartitionIndex" .= _cpiPartitionIndex)
          ]
      )

instance ToPath CreatePartitionIndex where
  toPath = const "/"

instance ToQuery CreatePartitionIndex where
  toQuery = const mempty

-- | /See:/ 'createPartitionIndexResponse' smart constructor.
newtype CreatePartitionIndexResponse = CreatePartitionIndexResponse'
  { _cpirsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreatePartitionIndexResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpirsResponseStatus' - -- | The response status code.
createPartitionIndexResponse ::
  -- | 'cpirsResponseStatus'
  Int ->
  CreatePartitionIndexResponse
createPartitionIndexResponse pResponseStatus_ =
  CreatePartitionIndexResponse'
    { _cpirsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
cpirsResponseStatus :: Lens' CreatePartitionIndexResponse Int
cpirsResponseStatus = lens _cpirsResponseStatus (\s a -> s {_cpirsResponseStatus = a})

instance NFData CreatePartitionIndexResponse
