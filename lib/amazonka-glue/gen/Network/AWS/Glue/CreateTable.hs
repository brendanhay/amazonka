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
-- Module      : Network.AWS.Glue.CreateTable
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new table definition in the Data Catalog.
--
--
module Network.AWS.Glue.CreateTable
    (
    -- * Creating a Request
      createTable
    , CreateTable
    -- * Request Lenses
    , ctCatalogId
    , ctDatabaseName
    , ctTableInput

    -- * Destructuring the Response
    , createTableResponse
    , CreateTableResponse
    -- * Response Lenses
    , cttrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createTable' smart constructor.
data CreateTable = CreateTable'
  { _ctCatalogId    :: !(Maybe Text)
  , _ctDatabaseName :: !Text
  , _ctTableInput   :: !TableInput
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTable' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctCatalogId' - The ID of the Data Catalog in which to create the @Table@ . If none is supplied, the AWS account ID is used by default.
--
-- * 'ctDatabaseName' - The catalog database in which to create the new table. For Hive compatibility, this name is entirely lowercase.
--
-- * 'ctTableInput' - The @TableInput@ object that defines the metadata table to create in the catalog.
createTable
    :: Text -- ^ 'ctDatabaseName'
    -> TableInput -- ^ 'ctTableInput'
    -> CreateTable
createTable pDatabaseName_ pTableInput_ =
  CreateTable'
    { _ctCatalogId = Nothing
    , _ctDatabaseName = pDatabaseName_
    , _ctTableInput = pTableInput_
    }


-- | The ID of the Data Catalog in which to create the @Table@ . If none is supplied, the AWS account ID is used by default.
ctCatalogId :: Lens' CreateTable (Maybe Text)
ctCatalogId = lens _ctCatalogId (\ s a -> s{_ctCatalogId = a})

-- | The catalog database in which to create the new table. For Hive compatibility, this name is entirely lowercase.
ctDatabaseName :: Lens' CreateTable Text
ctDatabaseName = lens _ctDatabaseName (\ s a -> s{_ctDatabaseName = a})

-- | The @TableInput@ object that defines the metadata table to create in the catalog.
ctTableInput :: Lens' CreateTable TableInput
ctTableInput = lens _ctTableInput (\ s a -> s{_ctTableInput = a})

instance AWSRequest CreateTable where
        type Rs CreateTable = CreateTableResponse
        request = postJSON glue
        response
          = receiveEmpty
              (\ s h x ->
                 CreateTableResponse' <$> (pure (fromEnum s)))

instance Hashable CreateTable where

instance NFData CreateTable where

instance ToHeaders CreateTable where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.CreateTable" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateTable where
        toJSON CreateTable'{..}
          = object
              (catMaybes
                 [("CatalogId" .=) <$> _ctCatalogId,
                  Just ("DatabaseName" .= _ctDatabaseName),
                  Just ("TableInput" .= _ctTableInput)])

instance ToPath CreateTable where
        toPath = const "/"

instance ToQuery CreateTable where
        toQuery = const mempty

-- | /See:/ 'createTableResponse' smart constructor.
newtype CreateTableResponse = CreateTableResponse'
  { _cttrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTableResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cttrsResponseStatus' - -- | The response status code.
createTableResponse
    :: Int -- ^ 'cttrsResponseStatus'
    -> CreateTableResponse
createTableResponse pResponseStatus_ =
  CreateTableResponse' {_cttrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
cttrsResponseStatus :: Lens' CreateTableResponse Int
cttrsResponseStatus = lens _cttrsResponseStatus (\ s a -> s{_cttrsResponseStatus = a})

instance NFData CreateTableResponse where
