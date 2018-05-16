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
-- Module      : Network.AWS.Glue.GetTableVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a specified version of a table.
--
--
module Network.AWS.Glue.GetTableVersion
    (
    -- * Creating a Request
      getTableVersion
    , GetTableVersion
    -- * Request Lenses
    , gtvVersionId
    , gtvCatalogId
    , gtvDatabaseName
    , gtvTableName

    -- * Destructuring the Response
    , getTableVersionResponse
    , GetTableVersionResponse
    -- * Response Lenses
    , gtvrsTableVersion
    , gtvrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getTableVersion' smart constructor.
data GetTableVersion = GetTableVersion'
  { _gtvVersionId    :: !(Maybe Text)
  , _gtvCatalogId    :: !(Maybe Text)
  , _gtvDatabaseName :: !Text
  , _gtvTableName    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTableVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtvVersionId' - The ID value of the table version to be retrieved.
--
-- * 'gtvCatalogId' - The ID of the Data Catalog where the tables reside. If none is supplied, the AWS account ID is used by default.
--
-- * 'gtvDatabaseName' - The database in the catalog in which the table resides. For Hive compatibility, this name is entirely lowercase.
--
-- * 'gtvTableName' - The name of the table. For Hive compatibility, this name is entirely lowercase.
getTableVersion
    :: Text -- ^ 'gtvDatabaseName'
    -> Text -- ^ 'gtvTableName'
    -> GetTableVersion
getTableVersion pDatabaseName_ pTableName_ =
  GetTableVersion'
    { _gtvVersionId = Nothing
    , _gtvCatalogId = Nothing
    , _gtvDatabaseName = pDatabaseName_
    , _gtvTableName = pTableName_
    }


-- | The ID value of the table version to be retrieved.
gtvVersionId :: Lens' GetTableVersion (Maybe Text)
gtvVersionId = lens _gtvVersionId (\ s a -> s{_gtvVersionId = a})

-- | The ID of the Data Catalog where the tables reside. If none is supplied, the AWS account ID is used by default.
gtvCatalogId :: Lens' GetTableVersion (Maybe Text)
gtvCatalogId = lens _gtvCatalogId (\ s a -> s{_gtvCatalogId = a})

-- | The database in the catalog in which the table resides. For Hive compatibility, this name is entirely lowercase.
gtvDatabaseName :: Lens' GetTableVersion Text
gtvDatabaseName = lens _gtvDatabaseName (\ s a -> s{_gtvDatabaseName = a})

-- | The name of the table. For Hive compatibility, this name is entirely lowercase.
gtvTableName :: Lens' GetTableVersion Text
gtvTableName = lens _gtvTableName (\ s a -> s{_gtvTableName = a})

instance AWSRequest GetTableVersion where
        type Rs GetTableVersion = GetTableVersionResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 GetTableVersionResponse' <$>
                   (x .?> "TableVersion") <*> (pure (fromEnum s)))

instance Hashable GetTableVersion where

instance NFData GetTableVersion where

instance ToHeaders GetTableVersion where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.GetTableVersion" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetTableVersion where
        toJSON GetTableVersion'{..}
          = object
              (catMaybes
                 [("VersionId" .=) <$> _gtvVersionId,
                  ("CatalogId" .=) <$> _gtvCatalogId,
                  Just ("DatabaseName" .= _gtvDatabaseName),
                  Just ("TableName" .= _gtvTableName)])

instance ToPath GetTableVersion where
        toPath = const "/"

instance ToQuery GetTableVersion where
        toQuery = const mempty

-- | /See:/ 'getTableVersionResponse' smart constructor.
data GetTableVersionResponse = GetTableVersionResponse'
  { _gtvrsTableVersion   :: !(Maybe TableVersion)
  , _gtvrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTableVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtvrsTableVersion' - The requested table version.
--
-- * 'gtvrsResponseStatus' - -- | The response status code.
getTableVersionResponse
    :: Int -- ^ 'gtvrsResponseStatus'
    -> GetTableVersionResponse
getTableVersionResponse pResponseStatus_ =
  GetTableVersionResponse'
    {_gtvrsTableVersion = Nothing, _gtvrsResponseStatus = pResponseStatus_}


-- | The requested table version.
gtvrsTableVersion :: Lens' GetTableVersionResponse (Maybe TableVersion)
gtvrsTableVersion = lens _gtvrsTableVersion (\ s a -> s{_gtvrsTableVersion = a})

-- | -- | The response status code.
gtvrsResponseStatus :: Lens' GetTableVersionResponse Int
gtvrsResponseStatus = lens _gtvrsResponseStatus (\ s a -> s{_gtvrsResponseStatus = a})

instance NFData GetTableVersionResponse where
