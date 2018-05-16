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
-- Module      : Network.AWS.Glue.GetDatabases
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all Databases defined in a given Data Catalog.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetDatabases
    (
    -- * Creating a Request
      getDatabases
    , GetDatabases
    -- * Request Lenses
    , gdCatalogId
    , gdNextToken
    , gdMaxResults

    -- * Destructuring the Response
    , getDatabasesResponse
    , GetDatabasesResponse
    -- * Response Lenses
    , gdsrsNextToken
    , gdsrsResponseStatus
    , gdsrsDatabaseList
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getDatabases' smart constructor.
data GetDatabases = GetDatabases'
  { _gdCatalogId  :: !(Maybe Text)
  , _gdNextToken  :: !(Maybe Text)
  , _gdMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDatabases' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdCatalogId' - The ID of the Data Catalog from which to retrieve @Databases@ . If none is supplied, the AWS account ID is used by default.
--
-- * 'gdNextToken' - A continuation token, if this is a continuation call.
--
-- * 'gdMaxResults' - The maximum number of databases to return in one response.
getDatabases
    :: GetDatabases
getDatabases =
  GetDatabases'
    {_gdCatalogId = Nothing, _gdNextToken = Nothing, _gdMaxResults = Nothing}


-- | The ID of the Data Catalog from which to retrieve @Databases@ . If none is supplied, the AWS account ID is used by default.
gdCatalogId :: Lens' GetDatabases (Maybe Text)
gdCatalogId = lens _gdCatalogId (\ s a -> s{_gdCatalogId = a})

-- | A continuation token, if this is a continuation call.
gdNextToken :: Lens' GetDatabases (Maybe Text)
gdNextToken = lens _gdNextToken (\ s a -> s{_gdNextToken = a})

-- | The maximum number of databases to return in one response.
gdMaxResults :: Lens' GetDatabases (Maybe Natural)
gdMaxResults = lens _gdMaxResults (\ s a -> s{_gdMaxResults = a}) . mapping _Nat

instance AWSPager GetDatabases where
        page rq rs
          | stop (rs ^. gdsrsNextToken) = Nothing
          | stop (rs ^. gdsrsDatabaseList) = Nothing
          | otherwise =
            Just $ rq & gdNextToken .~ rs ^. gdsrsNextToken

instance AWSRequest GetDatabases where
        type Rs GetDatabases = GetDatabasesResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 GetDatabasesResponse' <$>
                   (x .?> "NextToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "DatabaseList" .!@ mempty))

instance Hashable GetDatabases where

instance NFData GetDatabases where

instance ToHeaders GetDatabases where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.GetDatabases" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetDatabases where
        toJSON GetDatabases'{..}
          = object
              (catMaybes
                 [("CatalogId" .=) <$> _gdCatalogId,
                  ("NextToken" .=) <$> _gdNextToken,
                  ("MaxResults" .=) <$> _gdMaxResults])

instance ToPath GetDatabases where
        toPath = const "/"

instance ToQuery GetDatabases where
        toQuery = const mempty

-- | /See:/ 'getDatabasesResponse' smart constructor.
data GetDatabasesResponse = GetDatabasesResponse'
  { _gdsrsNextToken      :: !(Maybe Text)
  , _gdsrsResponseStatus :: !Int
  , _gdsrsDatabaseList   :: ![Database]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDatabasesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdsrsNextToken' - A continuation token for paginating the returned list of tokens, returned if the current segment of the list is not the last.
--
-- * 'gdsrsResponseStatus' - -- | The response status code.
--
-- * 'gdsrsDatabaseList' - A list of @Database@ objects from the specified catalog.
getDatabasesResponse
    :: Int -- ^ 'gdsrsResponseStatus'
    -> GetDatabasesResponse
getDatabasesResponse pResponseStatus_ =
  GetDatabasesResponse'
    { _gdsrsNextToken = Nothing
    , _gdsrsResponseStatus = pResponseStatus_
    , _gdsrsDatabaseList = mempty
    }


-- | A continuation token for paginating the returned list of tokens, returned if the current segment of the list is not the last.
gdsrsNextToken :: Lens' GetDatabasesResponse (Maybe Text)
gdsrsNextToken = lens _gdsrsNextToken (\ s a -> s{_gdsrsNextToken = a})

-- | -- | The response status code.
gdsrsResponseStatus :: Lens' GetDatabasesResponse Int
gdsrsResponseStatus = lens _gdsrsResponseStatus (\ s a -> s{_gdsrsResponseStatus = a})

-- | A list of @Database@ objects from the specified catalog.
gdsrsDatabaseList :: Lens' GetDatabasesResponse [Database]
gdsrsDatabaseList = lens _gdsrsDatabaseList (\ s a -> s{_gdsrsDatabaseList = a}) . _Coerce

instance NFData GetDatabasesResponse where
