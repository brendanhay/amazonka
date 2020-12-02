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
-- Module      : Network.AWS.Glue.UpdateTable
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a metadata table in the Data Catalog.
--
--
module Network.AWS.Glue.UpdateTable
    (
    -- * Creating a Request
      updateTable
    , UpdateTable
    -- * Request Lenses
    , utSkipArchive
    , utCatalogId
    , utDatabaseName
    , utTableInput

    -- * Destructuring the Response
    , updateTableResponse
    , UpdateTableResponse
    -- * Response Lenses
    , utrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateTable' smart constructor.
data UpdateTable = UpdateTable'
  { _utSkipArchive  :: !(Maybe Bool)
  , _utCatalogId    :: !(Maybe Text)
  , _utDatabaseName :: !Text
  , _utTableInput   :: !TableInput
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateTable' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utSkipArchive' - By default, @UpdateTable@ always creates an archived version of the table before updating it. If @skipArchive@ is set to true, however, @UpdateTable@ does not create the archived version.
--
-- * 'utCatalogId' - The ID of the Data Catalog where the table resides. If none is supplied, the AWS account ID is used by default.
--
-- * 'utDatabaseName' - The name of the catalog database in which the table resides. For Hive compatibility, this name is entirely lowercase.
--
-- * 'utTableInput' - An updated @TableInput@ object to define the metadata table in the catalog.
updateTable
    :: Text -- ^ 'utDatabaseName'
    -> TableInput -- ^ 'utTableInput'
    -> UpdateTable
updateTable pDatabaseName_ pTableInput_ =
  UpdateTable'
    { _utSkipArchive = Nothing
    , _utCatalogId = Nothing
    , _utDatabaseName = pDatabaseName_
    , _utTableInput = pTableInput_
    }


-- | By default, @UpdateTable@ always creates an archived version of the table before updating it. If @skipArchive@ is set to true, however, @UpdateTable@ does not create the archived version.
utSkipArchive :: Lens' UpdateTable (Maybe Bool)
utSkipArchive = lens _utSkipArchive (\ s a -> s{_utSkipArchive = a})

-- | The ID of the Data Catalog where the table resides. If none is supplied, the AWS account ID is used by default.
utCatalogId :: Lens' UpdateTable (Maybe Text)
utCatalogId = lens _utCatalogId (\ s a -> s{_utCatalogId = a})

-- | The name of the catalog database in which the table resides. For Hive compatibility, this name is entirely lowercase.
utDatabaseName :: Lens' UpdateTable Text
utDatabaseName = lens _utDatabaseName (\ s a -> s{_utDatabaseName = a})

-- | An updated @TableInput@ object to define the metadata table in the catalog.
utTableInput :: Lens' UpdateTable TableInput
utTableInput = lens _utTableInput (\ s a -> s{_utTableInput = a})

instance AWSRequest UpdateTable where
        type Rs UpdateTable = UpdateTableResponse
        request = postJSON glue
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateTableResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateTable where

instance NFData UpdateTable where

instance ToHeaders UpdateTable where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.UpdateTable" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateTable where
        toJSON UpdateTable'{..}
          = object
              (catMaybes
                 [("SkipArchive" .=) <$> _utSkipArchive,
                  ("CatalogId" .=) <$> _utCatalogId,
                  Just ("DatabaseName" .= _utDatabaseName),
                  Just ("TableInput" .= _utTableInput)])

instance ToPath UpdateTable where
        toPath = const "/"

instance ToQuery UpdateTable where
        toQuery = const mempty

-- | /See:/ 'updateTableResponse' smart constructor.
newtype UpdateTableResponse = UpdateTableResponse'
  { _utrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateTableResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utrsResponseStatus' - -- | The response status code.
updateTableResponse
    :: Int -- ^ 'utrsResponseStatus'
    -> UpdateTableResponse
updateTableResponse pResponseStatus_ =
  UpdateTableResponse' {_utrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
utrsResponseStatus :: Lens' UpdateTableResponse Int
utrsResponseStatus = lens _utrsResponseStatus (\ s a -> s{_utrsResponseStatus = a})

instance NFData UpdateTableResponse where
