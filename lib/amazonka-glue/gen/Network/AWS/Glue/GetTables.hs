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
-- Module      : Network.AWS.Glue.GetTables
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the definitions of some or all of the tables in a given @Database@ .
--
--
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetTables
  ( -- * Creating a Request
    getTables,
    GetTables,

    -- * Request Lenses
    gtsCatalogId,
    gtsNextToken,
    gtsExpression,
    gtsMaxResults,
    gtsDatabaseName,

    -- * Destructuring the Response
    getTablesResponse,
    GetTablesResponse,

    -- * Response Lenses
    gtsrsTableList,
    gtsrsNextToken,
    gtsrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getTables' smart constructor.
data GetTables = GetTables'
  { _gtsCatalogId :: !(Maybe Text),
    _gtsNextToken :: !(Maybe Text),
    _gtsExpression :: !(Maybe Text),
    _gtsMaxResults :: !(Maybe Nat),
    _gtsDatabaseName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetTables' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtsCatalogId' - The ID of the Data Catalog where the tables reside. If none is provided, the AWS account ID is used by default.
--
-- * 'gtsNextToken' - A continuation token, included if this is a continuation call.
--
-- * 'gtsExpression' - A regular expression pattern. If present, only those tables whose names match the pattern are returned.
--
-- * 'gtsMaxResults' - The maximum number of tables to return in a single response.
--
-- * 'gtsDatabaseName' - The database in the catalog whose tables to list. For Hive compatibility, this name is entirely lowercase.
getTables ::
  -- | 'gtsDatabaseName'
  Text ->
  GetTables
getTables pDatabaseName_ =
  GetTables'
    { _gtsCatalogId = Nothing,
      _gtsNextToken = Nothing,
      _gtsExpression = Nothing,
      _gtsMaxResults = Nothing,
      _gtsDatabaseName = pDatabaseName_
    }

-- | The ID of the Data Catalog where the tables reside. If none is provided, the AWS account ID is used by default.
gtsCatalogId :: Lens' GetTables (Maybe Text)
gtsCatalogId = lens _gtsCatalogId (\s a -> s {_gtsCatalogId = a})

-- | A continuation token, included if this is a continuation call.
gtsNextToken :: Lens' GetTables (Maybe Text)
gtsNextToken = lens _gtsNextToken (\s a -> s {_gtsNextToken = a})

-- | A regular expression pattern. If present, only those tables whose names match the pattern are returned.
gtsExpression :: Lens' GetTables (Maybe Text)
gtsExpression = lens _gtsExpression (\s a -> s {_gtsExpression = a})

-- | The maximum number of tables to return in a single response.
gtsMaxResults :: Lens' GetTables (Maybe Natural)
gtsMaxResults = lens _gtsMaxResults (\s a -> s {_gtsMaxResults = a}) . mapping _Nat

-- | The database in the catalog whose tables to list. For Hive compatibility, this name is entirely lowercase.
gtsDatabaseName :: Lens' GetTables Text
gtsDatabaseName = lens _gtsDatabaseName (\s a -> s {_gtsDatabaseName = a})

instance AWSPager GetTables where
  page rq rs
    | stop (rs ^. gtsrsNextToken) = Nothing
    | stop (rs ^. gtsrsTableList) = Nothing
    | otherwise = Just $ rq & gtsNextToken .~ rs ^. gtsrsNextToken

instance AWSRequest GetTables where
  type Rs GetTables = GetTablesResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          GetTablesResponse'
            <$> (x .?> "TableList" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable GetTables

instance NFData GetTables

instance ToHeaders GetTables where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.GetTables" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetTables where
  toJSON GetTables' {..} =
    object
      ( catMaybes
          [ ("CatalogId" .=) <$> _gtsCatalogId,
            ("NextToken" .=) <$> _gtsNextToken,
            ("Expression" .=) <$> _gtsExpression,
            ("MaxResults" .=) <$> _gtsMaxResults,
            Just ("DatabaseName" .= _gtsDatabaseName)
          ]
      )

instance ToPath GetTables where
  toPath = const "/"

instance ToQuery GetTables where
  toQuery = const mempty

-- | /See:/ 'getTablesResponse' smart constructor.
data GetTablesResponse = GetTablesResponse'
  { _gtsrsTableList ::
      !(Maybe [Table]),
    _gtsrsNextToken :: !(Maybe Text),
    _gtsrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetTablesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtsrsTableList' - A list of the requested @Table@ objects.
--
-- * 'gtsrsNextToken' - A continuation token, present if the current list segment is not the last.
--
-- * 'gtsrsResponseStatus' - -- | The response status code.
getTablesResponse ::
  -- | 'gtsrsResponseStatus'
  Int ->
  GetTablesResponse
getTablesResponse pResponseStatus_ =
  GetTablesResponse'
    { _gtsrsTableList = Nothing,
      _gtsrsNextToken = Nothing,
      _gtsrsResponseStatus = pResponseStatus_
    }

-- | A list of the requested @Table@ objects.
gtsrsTableList :: Lens' GetTablesResponse [Table]
gtsrsTableList = lens _gtsrsTableList (\s a -> s {_gtsrsTableList = a}) . _Default . _Coerce

-- | A continuation token, present if the current list segment is not the last.
gtsrsNextToken :: Lens' GetTablesResponse (Maybe Text)
gtsrsNextToken = lens _gtsrsNextToken (\s a -> s {_gtsrsNextToken = a})

-- | -- | The response status code.
gtsrsResponseStatus :: Lens' GetTablesResponse Int
gtsrsResponseStatus = lens _gtsrsResponseStatus (\s a -> s {_gtsrsResponseStatus = a})

instance NFData GetTablesResponse
