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
-- Module      : Network.AWS.Glue.GetTable
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the @Table@ definition in a Data Catalog for a specified table.
--
--
module Network.AWS.Glue.GetTable
    (
    -- * Creating a Request
      getTable
    , GetTable
    -- * Request Lenses
    , gttCatalogId
    , gttDatabaseName
    , gttName

    -- * Destructuring the Response
    , getTableResponse
    , GetTableResponse
    -- * Response Lenses
    , ggrsTable
    , ggrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getTable' smart constructor.
data GetTable = GetTable'
  { _gttCatalogId    :: !(Maybe Text)
  , _gttDatabaseName :: !Text
  , _gttName         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTable' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gttCatalogId' - The ID of the Data Catalog where the table resides. If none is supplied, the AWS account ID is used by default.
--
-- * 'gttDatabaseName' - The name of the database in the catalog in which the table resides. For Hive compatibility, this name is entirely lowercase.
--
-- * 'gttName' - The name of the table for which to retrieve the definition. For Hive compatibility, this name is entirely lowercase.
getTable
    :: Text -- ^ 'gttDatabaseName'
    -> Text -- ^ 'gttName'
    -> GetTable
getTable pDatabaseName_ pName_ =
  GetTable'
    { _gttCatalogId = Nothing
    , _gttDatabaseName = pDatabaseName_
    , _gttName = pName_
    }


-- | The ID of the Data Catalog where the table resides. If none is supplied, the AWS account ID is used by default.
gttCatalogId :: Lens' GetTable (Maybe Text)
gttCatalogId = lens _gttCatalogId (\ s a -> s{_gttCatalogId = a})

-- | The name of the database in the catalog in which the table resides. For Hive compatibility, this name is entirely lowercase.
gttDatabaseName :: Lens' GetTable Text
gttDatabaseName = lens _gttDatabaseName (\ s a -> s{_gttDatabaseName = a})

-- | The name of the table for which to retrieve the definition. For Hive compatibility, this name is entirely lowercase.
gttName :: Lens' GetTable Text
gttName = lens _gttName (\ s a -> s{_gttName = a})

instance AWSRequest GetTable where
        type Rs GetTable = GetTableResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 GetTableResponse' <$>
                   (x .?> "Table") <*> (pure (fromEnum s)))

instance Hashable GetTable where

instance NFData GetTable where

instance ToHeaders GetTable where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.GetTable" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetTable where
        toJSON GetTable'{..}
          = object
              (catMaybes
                 [("CatalogId" .=) <$> _gttCatalogId,
                  Just ("DatabaseName" .= _gttDatabaseName),
                  Just ("Name" .= _gttName)])

instance ToPath GetTable where
        toPath = const "/"

instance ToQuery GetTable where
        toQuery = const mempty

-- | /See:/ 'getTableResponse' smart constructor.
data GetTableResponse = GetTableResponse'
  { _ggrsTable          :: !(Maybe Table)
  , _ggrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTableResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ggrsTable' - The @Table@ object that defines the specified table.
--
-- * 'ggrsResponseStatus' - -- | The response status code.
getTableResponse
    :: Int -- ^ 'ggrsResponseStatus'
    -> GetTableResponse
getTableResponse pResponseStatus_ =
  GetTableResponse'
    {_ggrsTable = Nothing, _ggrsResponseStatus = pResponseStatus_}


-- | The @Table@ object that defines the specified table.
ggrsTable :: Lens' GetTableResponse (Maybe Table)
ggrsTable = lens _ggrsTable (\ s a -> s{_ggrsTable = a})

-- | -- | The response status code.
ggrsResponseStatus :: Lens' GetTableResponse Int
ggrsResponseStatus = lens _ggrsResponseStatus (\ s a -> s{_ggrsResponseStatus = a})

instance NFData GetTableResponse where
