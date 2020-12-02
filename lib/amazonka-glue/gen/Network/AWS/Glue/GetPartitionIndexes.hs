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
-- Module      : Network.AWS.Glue.GetPartitionIndexes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the partition indexes associated with a table.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetPartitionIndexes
  ( -- * Creating a Request
    getPartitionIndexes,
    GetPartitionIndexes,

    -- * Request Lenses
    gpiCatalogId,
    gpiNextToken,
    gpiDatabaseName,
    gpiTableName,

    -- * Destructuring the Response
    getPartitionIndexesResponse,
    GetPartitionIndexesResponse,

    -- * Response Lenses
    gpirsPartitionIndexDescriptorList,
    gpirsNextToken,
    gpirsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getPartitionIndexes' smart constructor.
data GetPartitionIndexes = GetPartitionIndexes'
  { _gpiCatalogId ::
      !(Maybe Text),
    _gpiNextToken :: !(Maybe Text),
    _gpiDatabaseName :: !Text,
    _gpiTableName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetPartitionIndexes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpiCatalogId' - The catalog ID where the table resides.
--
-- * 'gpiNextToken' - A continuation token, included if this is a continuation call.
--
-- * 'gpiDatabaseName' - Specifies the name of a database from which you want to retrieve partition indexes.
--
-- * 'gpiTableName' - Specifies the name of a table for which you want to retrieve the partition indexes.
getPartitionIndexes ::
  -- | 'gpiDatabaseName'
  Text ->
  -- | 'gpiTableName'
  Text ->
  GetPartitionIndexes
getPartitionIndexes pDatabaseName_ pTableName_ =
  GetPartitionIndexes'
    { _gpiCatalogId = Nothing,
      _gpiNextToken = Nothing,
      _gpiDatabaseName = pDatabaseName_,
      _gpiTableName = pTableName_
    }

-- | The catalog ID where the table resides.
gpiCatalogId :: Lens' GetPartitionIndexes (Maybe Text)
gpiCatalogId = lens _gpiCatalogId (\s a -> s {_gpiCatalogId = a})

-- | A continuation token, included if this is a continuation call.
gpiNextToken :: Lens' GetPartitionIndexes (Maybe Text)
gpiNextToken = lens _gpiNextToken (\s a -> s {_gpiNextToken = a})

-- | Specifies the name of a database from which you want to retrieve partition indexes.
gpiDatabaseName :: Lens' GetPartitionIndexes Text
gpiDatabaseName = lens _gpiDatabaseName (\s a -> s {_gpiDatabaseName = a})

-- | Specifies the name of a table for which you want to retrieve the partition indexes.
gpiTableName :: Lens' GetPartitionIndexes Text
gpiTableName = lens _gpiTableName (\s a -> s {_gpiTableName = a})

instance AWSPager GetPartitionIndexes where
  page rq rs
    | stop (rs ^. gpirsNextToken) = Nothing
    | stop (rs ^. gpirsPartitionIndexDescriptorList) = Nothing
    | otherwise = Just $ rq & gpiNextToken .~ rs ^. gpirsNextToken

instance AWSRequest GetPartitionIndexes where
  type Rs GetPartitionIndexes = GetPartitionIndexesResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          GetPartitionIndexesResponse'
            <$> (x .?> "PartitionIndexDescriptorList" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable GetPartitionIndexes

instance NFData GetPartitionIndexes

instance ToHeaders GetPartitionIndexes where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.GetPartitionIndexes" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetPartitionIndexes where
  toJSON GetPartitionIndexes' {..} =
    object
      ( catMaybes
          [ ("CatalogId" .=) <$> _gpiCatalogId,
            ("NextToken" .=) <$> _gpiNextToken,
            Just ("DatabaseName" .= _gpiDatabaseName),
            Just ("TableName" .= _gpiTableName)
          ]
      )

instance ToPath GetPartitionIndexes where
  toPath = const "/"

instance ToQuery GetPartitionIndexes where
  toQuery = const mempty

-- | /See:/ 'getPartitionIndexesResponse' smart constructor.
data GetPartitionIndexesResponse = GetPartitionIndexesResponse'
  { _gpirsPartitionIndexDescriptorList ::
      !(Maybe [PartitionIndexDescriptor]),
    _gpirsNextToken :: !(Maybe Text),
    _gpirsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetPartitionIndexesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpirsPartitionIndexDescriptorList' - A list of index descriptors.
--
-- * 'gpirsNextToken' - A continuation token, present if the current list segment is not the last.
--
-- * 'gpirsResponseStatus' - -- | The response status code.
getPartitionIndexesResponse ::
  -- | 'gpirsResponseStatus'
  Int ->
  GetPartitionIndexesResponse
getPartitionIndexesResponse pResponseStatus_ =
  GetPartitionIndexesResponse'
    { _gpirsPartitionIndexDescriptorList =
        Nothing,
      _gpirsNextToken = Nothing,
      _gpirsResponseStatus = pResponseStatus_
    }

-- | A list of index descriptors.
gpirsPartitionIndexDescriptorList :: Lens' GetPartitionIndexesResponse [PartitionIndexDescriptor]
gpirsPartitionIndexDescriptorList = lens _gpirsPartitionIndexDescriptorList (\s a -> s {_gpirsPartitionIndexDescriptorList = a}) . _Default . _Coerce

-- | A continuation token, present if the current list segment is not the last.
gpirsNextToken :: Lens' GetPartitionIndexesResponse (Maybe Text)
gpirsNextToken = lens _gpirsNextToken (\s a -> s {_gpirsNextToken = a})

-- | -- | The response status code.
gpirsResponseStatus :: Lens' GetPartitionIndexesResponse Int
gpirsResponseStatus = lens _gpirsResponseStatus (\s a -> s {_gpirsResponseStatus = a})

instance NFData GetPartitionIndexesResponse
