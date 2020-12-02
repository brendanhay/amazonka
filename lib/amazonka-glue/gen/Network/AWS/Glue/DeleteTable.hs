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
-- Module      : Network.AWS.Glue.DeleteTable
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a table definition from the Data Catalog.
--
--
module Network.AWS.Glue.DeleteTable
    (
    -- * Creating a Request
      deleteTable
    , DeleteTable
    -- * Request Lenses
    , dtCatalogId
    , dtDatabaseName
    , dtName

    -- * Destructuring the Response
    , deleteTableResponse
    , DeleteTableResponse
    -- * Response Lenses
    , dtrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteTable' smart constructor.
data DeleteTable = DeleteTable'
  { _dtCatalogId    :: !(Maybe Text)
  , _dtDatabaseName :: !Text
  , _dtName         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteTable' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtCatalogId' - The ID of the Data Catalog where the table resides. If none is supplied, the AWS account ID is used by default.
--
-- * 'dtDatabaseName' - The name of the catalog database in which the table resides. For Hive compatibility, this name is entirely lowercase.
--
-- * 'dtName' - The name of the table to be deleted. For Hive compatibility, this name is entirely lowercase.
deleteTable
    :: Text -- ^ 'dtDatabaseName'
    -> Text -- ^ 'dtName'
    -> DeleteTable
deleteTable pDatabaseName_ pName_ =
  DeleteTable'
    {_dtCatalogId = Nothing, _dtDatabaseName = pDatabaseName_, _dtName = pName_}


-- | The ID of the Data Catalog where the table resides. If none is supplied, the AWS account ID is used by default.
dtCatalogId :: Lens' DeleteTable (Maybe Text)
dtCatalogId = lens _dtCatalogId (\ s a -> s{_dtCatalogId = a})

-- | The name of the catalog database in which the table resides. For Hive compatibility, this name is entirely lowercase.
dtDatabaseName :: Lens' DeleteTable Text
dtDatabaseName = lens _dtDatabaseName (\ s a -> s{_dtDatabaseName = a})

-- | The name of the table to be deleted. For Hive compatibility, this name is entirely lowercase.
dtName :: Lens' DeleteTable Text
dtName = lens _dtName (\ s a -> s{_dtName = a})

instance AWSRequest DeleteTable where
        type Rs DeleteTable = DeleteTableResponse
        request = postJSON glue
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteTableResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteTable where

instance NFData DeleteTable where

instance ToHeaders DeleteTable where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.DeleteTable" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteTable where
        toJSON DeleteTable'{..}
          = object
              (catMaybes
                 [("CatalogId" .=) <$> _dtCatalogId,
                  Just ("DatabaseName" .= _dtDatabaseName),
                  Just ("Name" .= _dtName)])

instance ToPath DeleteTable where
        toPath = const "/"

instance ToQuery DeleteTable where
        toQuery = const mempty

-- | /See:/ 'deleteTableResponse' smart constructor.
newtype DeleteTableResponse = DeleteTableResponse'
  { _dtrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteTableResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtrsResponseStatus' - -- | The response status code.
deleteTableResponse
    :: Int -- ^ 'dtrsResponseStatus'
    -> DeleteTableResponse
deleteTableResponse pResponseStatus_ =
  DeleteTableResponse' {_dtrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dtrsResponseStatus :: Lens' DeleteTableResponse Int
dtrsResponseStatus = lens _dtrsResponseStatus (\ s a -> s{_dtrsResponseStatus = a})

instance NFData DeleteTableResponse where
