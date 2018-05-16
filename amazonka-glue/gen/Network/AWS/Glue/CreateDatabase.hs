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
-- Module      : Network.AWS.Glue.CreateDatabase
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new database in a Data Catalog.
--
--
module Network.AWS.Glue.CreateDatabase
    (
    -- * Creating a Request
      createDatabase
    , CreateDatabase
    -- * Request Lenses
    , cdCatalogId
    , cdDatabaseInput

    -- * Destructuring the Response
    , createDatabaseResponse
    , CreateDatabaseResponse
    -- * Response Lenses
    , cdrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createDatabase' smart constructor.
data CreateDatabase = CreateDatabase'
  { _cdCatalogId     :: !(Maybe Text)
  , _cdDatabaseInput :: !DatabaseInput
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDatabase' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdCatalogId' - The ID of the Data Catalog in which to create the database. If none is supplied, the AWS account ID is used by default.
--
-- * 'cdDatabaseInput' - A @DatabaseInput@ object defining the metadata database to create in the catalog.
createDatabase
    :: DatabaseInput -- ^ 'cdDatabaseInput'
    -> CreateDatabase
createDatabase pDatabaseInput_ =
  CreateDatabase' {_cdCatalogId = Nothing, _cdDatabaseInput = pDatabaseInput_}


-- | The ID of the Data Catalog in which to create the database. If none is supplied, the AWS account ID is used by default.
cdCatalogId :: Lens' CreateDatabase (Maybe Text)
cdCatalogId = lens _cdCatalogId (\ s a -> s{_cdCatalogId = a})

-- | A @DatabaseInput@ object defining the metadata database to create in the catalog.
cdDatabaseInput :: Lens' CreateDatabase DatabaseInput
cdDatabaseInput = lens _cdDatabaseInput (\ s a -> s{_cdDatabaseInput = a})

instance AWSRequest CreateDatabase where
        type Rs CreateDatabase = CreateDatabaseResponse
        request = postJSON glue
        response
          = receiveEmpty
              (\ s h x ->
                 CreateDatabaseResponse' <$> (pure (fromEnum s)))

instance Hashable CreateDatabase where

instance NFData CreateDatabase where

instance ToHeaders CreateDatabase where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.CreateDatabase" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateDatabase where
        toJSON CreateDatabase'{..}
          = object
              (catMaybes
                 [("CatalogId" .=) <$> _cdCatalogId,
                  Just ("DatabaseInput" .= _cdDatabaseInput)])

instance ToPath CreateDatabase where
        toPath = const "/"

instance ToQuery CreateDatabase where
        toQuery = const mempty

-- | /See:/ 'createDatabaseResponse' smart constructor.
newtype CreateDatabaseResponse = CreateDatabaseResponse'
  { _cdrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDatabaseResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdrsResponseStatus' - -- | The response status code.
createDatabaseResponse
    :: Int -- ^ 'cdrsResponseStatus'
    -> CreateDatabaseResponse
createDatabaseResponse pResponseStatus_ =
  CreateDatabaseResponse' {_cdrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
cdrsResponseStatus :: Lens' CreateDatabaseResponse Int
cdrsResponseStatus = lens _cdrsResponseStatus (\ s a -> s{_cdrsResponseStatus = a})

instance NFData CreateDatabaseResponse where
