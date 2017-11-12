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
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of strings that identify available versions of a specified table.
--
--
module Network.AWS.Glue.GetTableVersions
    (
    -- * Creating a Request
      getTableVersions
    , GetTableVersions
    -- * Request Lenses
    , gtvCatalogId
    , gtvNextToken
    , gtvMaxResults
    , gtvDatabaseName
    , gtvTableName

    -- * Destructuring the Response
    , getTableVersionsResponse
    , GetTableVersionsResponse
    -- * Response Lenses
    , gtvrsTableVersions
    , gtvrsNextToken
    , gtvrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getTableVersions' smart constructor.
data GetTableVersions = GetTableVersions'
  { _gtvCatalogId    :: !(Maybe Text)
  , _gtvNextToken    :: !(Maybe Text)
  , _gtvMaxResults   :: !(Maybe Nat)
  , _gtvDatabaseName :: !Text
  , _gtvTableName    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTableVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtvCatalogId' - The ID of the Data Catalog where the tables reside. If none is supplied, the AWS account ID is used by default.
--
-- * 'gtvNextToken' - A continuation token, if this is not the first call.
--
-- * 'gtvMaxResults' - The maximum number of table versions to return in one response.
--
-- * 'gtvDatabaseName' - The database in the catalog in which the table resides.
--
-- * 'gtvTableName' - The name of the table.
getTableVersions
    :: Text -- ^ 'gtvDatabaseName'
    -> Text -- ^ 'gtvTableName'
    -> GetTableVersions
getTableVersions pDatabaseName_ pTableName_ =
  GetTableVersions'
  { _gtvCatalogId = Nothing
  , _gtvNextToken = Nothing
  , _gtvMaxResults = Nothing
  , _gtvDatabaseName = pDatabaseName_
  , _gtvTableName = pTableName_
  }


-- | The ID of the Data Catalog where the tables reside. If none is supplied, the AWS account ID is used by default.
gtvCatalogId :: Lens' GetTableVersions (Maybe Text)
gtvCatalogId = lens _gtvCatalogId (\ s a -> s{_gtvCatalogId = a});

-- | A continuation token, if this is not the first call.
gtvNextToken :: Lens' GetTableVersions (Maybe Text)
gtvNextToken = lens _gtvNextToken (\ s a -> s{_gtvNextToken = a});

-- | The maximum number of table versions to return in one response.
gtvMaxResults :: Lens' GetTableVersions (Maybe Natural)
gtvMaxResults = lens _gtvMaxResults (\ s a -> s{_gtvMaxResults = a}) . mapping _Nat;

-- | The database in the catalog in which the table resides.
gtvDatabaseName :: Lens' GetTableVersions Text
gtvDatabaseName = lens _gtvDatabaseName (\ s a -> s{_gtvDatabaseName = a});

-- | The name of the table.
gtvTableName :: Lens' GetTableVersions Text
gtvTableName = lens _gtvTableName (\ s a -> s{_gtvTableName = a});

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
                 [("CatalogId" .=) <$> _gtvCatalogId,
                  ("NextToken" .=) <$> _gtvNextToken,
                  ("MaxResults" .=) <$> _gtvMaxResults,
                  Just ("DatabaseName" .= _gtvDatabaseName),
                  Just ("TableName" .= _gtvTableName)])

instance ToPath GetTableVersions where
        toPath = const "/"

instance ToQuery GetTableVersions where
        toQuery = const mempty

-- | /See:/ 'getTableVersionsResponse' smart constructor.
data GetTableVersionsResponse = GetTableVersionsResponse'
  { _gtvrsTableVersions  :: !(Maybe [TableVersion])
  , _gtvrsNextToken      :: !(Maybe Text)
  , _gtvrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTableVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtvrsTableVersions' - A list of strings identifying available versions of the specified table.
--
-- * 'gtvrsNextToken' - A continuation token, if the list of available versions does not include the last one.
--
-- * 'gtvrsResponseStatus' - -- | The response status code.
getTableVersionsResponse
    :: Int -- ^ 'gtvrsResponseStatus'
    -> GetTableVersionsResponse
getTableVersionsResponse pResponseStatus_ =
  GetTableVersionsResponse'
  { _gtvrsTableVersions = Nothing
  , _gtvrsNextToken = Nothing
  , _gtvrsResponseStatus = pResponseStatus_
  }


-- | A list of strings identifying available versions of the specified table.
gtvrsTableVersions :: Lens' GetTableVersionsResponse [TableVersion]
gtvrsTableVersions = lens _gtvrsTableVersions (\ s a -> s{_gtvrsTableVersions = a}) . _Default . _Coerce;

-- | A continuation token, if the list of available versions does not include the last one.
gtvrsNextToken :: Lens' GetTableVersionsResponse (Maybe Text)
gtvrsNextToken = lens _gtvrsNextToken (\ s a -> s{_gtvrsNextToken = a});

-- | -- | The response status code.
gtvrsResponseStatus :: Lens' GetTableVersionsResponse Int
gtvrsResponseStatus = lens _gtvrsResponseStatus (\ s a -> s{_gtvrsResponseStatus = a});

instance NFData GetTableVersionsResponse where
