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
-- Module      : Network.AWS.Glue.BatchDeleteTableVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified batch of versions of a table.
--
--
module Network.AWS.Glue.BatchDeleteTableVersion
    (
    -- * Creating a Request
      batchDeleteTableVersion
    , BatchDeleteTableVersion
    -- * Request Lenses
    , bdtvCatalogId
    , bdtvDatabaseName
    , bdtvTableName
    , bdtvVersionIds

    -- * Destructuring the Response
    , batchDeleteTableVersionResponse
    , BatchDeleteTableVersionResponse
    -- * Response Lenses
    , bdtvrsErrors
    , bdtvrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'batchDeleteTableVersion' smart constructor.
data BatchDeleteTableVersion = BatchDeleteTableVersion'
  { _bdtvCatalogId    :: !(Maybe Text)
  , _bdtvDatabaseName :: !Text
  , _bdtvTableName    :: !Text
  , _bdtvVersionIds   :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchDeleteTableVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdtvCatalogId' - The ID of the Data Catalog where the tables reside. If none is supplied, the AWS account ID is used by default.
--
-- * 'bdtvDatabaseName' - The database in the catalog in which the table resides. For Hive compatibility, this name is entirely lowercase.
--
-- * 'bdtvTableName' - The name of the table. For Hive compatibility, this name is entirely lowercase.
--
-- * 'bdtvVersionIds' - A list of the IDs of versions to be deleted.
batchDeleteTableVersion
    :: Text -- ^ 'bdtvDatabaseName'
    -> Text -- ^ 'bdtvTableName'
    -> BatchDeleteTableVersion
batchDeleteTableVersion pDatabaseName_ pTableName_ =
  BatchDeleteTableVersion'
    { _bdtvCatalogId = Nothing
    , _bdtvDatabaseName = pDatabaseName_
    , _bdtvTableName = pTableName_
    , _bdtvVersionIds = mempty
    }


-- | The ID of the Data Catalog where the tables reside. If none is supplied, the AWS account ID is used by default.
bdtvCatalogId :: Lens' BatchDeleteTableVersion (Maybe Text)
bdtvCatalogId = lens _bdtvCatalogId (\ s a -> s{_bdtvCatalogId = a})

-- | The database in the catalog in which the table resides. For Hive compatibility, this name is entirely lowercase.
bdtvDatabaseName :: Lens' BatchDeleteTableVersion Text
bdtvDatabaseName = lens _bdtvDatabaseName (\ s a -> s{_bdtvDatabaseName = a})

-- | The name of the table. For Hive compatibility, this name is entirely lowercase.
bdtvTableName :: Lens' BatchDeleteTableVersion Text
bdtvTableName = lens _bdtvTableName (\ s a -> s{_bdtvTableName = a})

-- | A list of the IDs of versions to be deleted.
bdtvVersionIds :: Lens' BatchDeleteTableVersion [Text]
bdtvVersionIds = lens _bdtvVersionIds (\ s a -> s{_bdtvVersionIds = a}) . _Coerce

instance AWSRequest BatchDeleteTableVersion where
        type Rs BatchDeleteTableVersion =
             BatchDeleteTableVersionResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 BatchDeleteTableVersionResponse' <$>
                   (x .?> "Errors" .!@ mempty) <*> (pure (fromEnum s)))

instance Hashable BatchDeleteTableVersion where

instance NFData BatchDeleteTableVersion where

instance ToHeaders BatchDeleteTableVersion where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.BatchDeleteTableVersion" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON BatchDeleteTableVersion where
        toJSON BatchDeleteTableVersion'{..}
          = object
              (catMaybes
                 [("CatalogId" .=) <$> _bdtvCatalogId,
                  Just ("DatabaseName" .= _bdtvDatabaseName),
                  Just ("TableName" .= _bdtvTableName),
                  Just ("VersionIds" .= _bdtvVersionIds)])

instance ToPath BatchDeleteTableVersion where
        toPath = const "/"

instance ToQuery BatchDeleteTableVersion where
        toQuery = const mempty

-- | /See:/ 'batchDeleteTableVersionResponse' smart constructor.
data BatchDeleteTableVersionResponse = BatchDeleteTableVersionResponse'
  { _bdtvrsErrors         :: !(Maybe [TableVersionError])
  , _bdtvrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchDeleteTableVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdtvrsErrors' - A list of errors encountered while trying to delete the specified table versions.
--
-- * 'bdtvrsResponseStatus' - -- | The response status code.
batchDeleteTableVersionResponse
    :: Int -- ^ 'bdtvrsResponseStatus'
    -> BatchDeleteTableVersionResponse
batchDeleteTableVersionResponse pResponseStatus_ =
  BatchDeleteTableVersionResponse'
    {_bdtvrsErrors = Nothing, _bdtvrsResponseStatus = pResponseStatus_}


-- | A list of errors encountered while trying to delete the specified table versions.
bdtvrsErrors :: Lens' BatchDeleteTableVersionResponse [TableVersionError]
bdtvrsErrors = lens _bdtvrsErrors (\ s a -> s{_bdtvrsErrors = a}) . _Default . _Coerce

-- | -- | The response status code.
bdtvrsResponseStatus :: Lens' BatchDeleteTableVersionResponse Int
bdtvrsResponseStatus = lens _bdtvrsResponseStatus (\ s a -> s{_bdtvrsResponseStatus = a})

instance NFData BatchDeleteTableVersionResponse where
