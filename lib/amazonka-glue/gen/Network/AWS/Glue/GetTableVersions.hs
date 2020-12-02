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
-- Module      : Network.AWS.Glue.GetTableVersions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of strings that identify available versions of a specified table.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetTableVersions
    (
    -- * Creating a Request
      getTableVersions
    , GetTableVersions
    -- * Request Lenses
    , gtvsCatalogId
    , gtvsNextToken
    , gtvsMaxResults
    , gtvsDatabaseName
    , gtvsTableName

    -- * Destructuring the Response
    , getTableVersionsResponse
    , GetTableVersionsResponse
    -- * Response Lenses
    , gtvsrsTableVersions
    , gtvsrsNextToken
    , gtvsrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getTableVersions' smart constructor.
data GetTableVersions = GetTableVersions'
  { _gtvsCatalogId    :: !(Maybe Text)
  , _gtvsNextToken    :: !(Maybe Text)
  , _gtvsMaxResults   :: !(Maybe Nat)
  , _gtvsDatabaseName :: !Text
  , _gtvsTableName    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTableVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtvsCatalogId' - The ID of the Data Catalog where the tables reside. If none is supplied, the AWS account ID is used by default.
--
-- * 'gtvsNextToken' - A continuation token, if this is not the first call.
--
-- * 'gtvsMaxResults' - The maximum number of table versions to return in one response.
--
-- * 'gtvsDatabaseName' - The database in the catalog in which the table resides. For Hive compatibility, this name is entirely lowercase.
--
-- * 'gtvsTableName' - The name of the table. For Hive compatibility, this name is entirely lowercase.
getTableVersions
    :: Text -- ^ 'gtvsDatabaseName'
    -> Text -- ^ 'gtvsTableName'
    -> GetTableVersions
getTableVersions pDatabaseName_ pTableName_ =
  GetTableVersions'
    { _gtvsCatalogId = Nothing
    , _gtvsNextToken = Nothing
    , _gtvsMaxResults = Nothing
    , _gtvsDatabaseName = pDatabaseName_
    , _gtvsTableName = pTableName_
    }


-- | The ID of the Data Catalog where the tables reside. If none is supplied, the AWS account ID is used by default.
gtvsCatalogId :: Lens' GetTableVersions (Maybe Text)
gtvsCatalogId = lens _gtvsCatalogId (\ s a -> s{_gtvsCatalogId = a})

-- | A continuation token, if this is not the first call.
gtvsNextToken :: Lens' GetTableVersions (Maybe Text)
gtvsNextToken = lens _gtvsNextToken (\ s a -> s{_gtvsNextToken = a})

-- | The maximum number of table versions to return in one response.
gtvsMaxResults :: Lens' GetTableVersions (Maybe Natural)
gtvsMaxResults = lens _gtvsMaxResults (\ s a -> s{_gtvsMaxResults = a}) . mapping _Nat

-- | The database in the catalog in which the table resides. For Hive compatibility, this name is entirely lowercase.
gtvsDatabaseName :: Lens' GetTableVersions Text
gtvsDatabaseName = lens _gtvsDatabaseName (\ s a -> s{_gtvsDatabaseName = a})

-- | The name of the table. For Hive compatibility, this name is entirely lowercase.
gtvsTableName :: Lens' GetTableVersions Text
gtvsTableName = lens _gtvsTableName (\ s a -> s{_gtvsTableName = a})

instance AWSPager GetTableVersions where
        page rq rs
          | stop (rs ^. gtvsrsNextToken) = Nothing
          | stop (rs ^. gtvsrsTableVersions) = Nothing
          | otherwise =
            Just $ rq & gtvsNextToken .~ rs ^. gtvsrsNextToken

instance AWSRequest GetTableVersions where
        type Rs GetTableVersions = GetTableVersionsResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 GetTableVersionsResponse' <$>
                   (x .?> "TableVersions" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable GetTableVersions where

instance NFData GetTableVersions where

instance ToHeaders GetTableVersions where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.GetTableVersions" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetTableVersions where
        toJSON GetTableVersions'{..}
          = object
              (catMaybes
                 [("CatalogId" .=) <$> _gtvsCatalogId,
                  ("NextToken" .=) <$> _gtvsNextToken,
                  ("MaxResults" .=) <$> _gtvsMaxResults,
                  Just ("DatabaseName" .= _gtvsDatabaseName),
                  Just ("TableName" .= _gtvsTableName)])

instance ToPath GetTableVersions where
        toPath = const "/"

instance ToQuery GetTableVersions where
        toQuery = const mempty

-- | /See:/ 'getTableVersionsResponse' smart constructor.
data GetTableVersionsResponse = GetTableVersionsResponse'
  { _gtvsrsTableVersions  :: !(Maybe [TableVersion])
  , _gtvsrsNextToken      :: !(Maybe Text)
  , _gtvsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTableVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtvsrsTableVersions' - A list of strings identifying available versions of the specified table.
--
-- * 'gtvsrsNextToken' - A continuation token, if the list of available versions does not include the last one.
--
-- * 'gtvsrsResponseStatus' - -- | The response status code.
getTableVersionsResponse
    :: Int -- ^ 'gtvsrsResponseStatus'
    -> GetTableVersionsResponse
getTableVersionsResponse pResponseStatus_ =
  GetTableVersionsResponse'
    { _gtvsrsTableVersions = Nothing
    , _gtvsrsNextToken = Nothing
    , _gtvsrsResponseStatus = pResponseStatus_
    }


-- | A list of strings identifying available versions of the specified table.
gtvsrsTableVersions :: Lens' GetTableVersionsResponse [TableVersion]
gtvsrsTableVersions = lens _gtvsrsTableVersions (\ s a -> s{_gtvsrsTableVersions = a}) . _Default . _Coerce

-- | A continuation token, if the list of available versions does not include the last one.
gtvsrsNextToken :: Lens' GetTableVersionsResponse (Maybe Text)
gtvsrsNextToken = lens _gtvsrsNextToken (\ s a -> s{_gtvsrsNextToken = a})

-- | -- | The response status code.
gtvsrsResponseStatus :: Lens' GetTableVersionsResponse Int
gtvsrsResponseStatus = lens _gtvsrsResponseStatus (\ s a -> s{_gtvsrsResponseStatus = a})

instance NFData GetTableVersionsResponse where
