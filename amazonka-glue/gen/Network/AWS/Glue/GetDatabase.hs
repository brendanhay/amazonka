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
-- Module      : Network.AWS.Glue.GetDatabase
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the definition of a specified database.
--
--
module Network.AWS.Glue.GetDatabase
    (
    -- * Creating a Request
      getDatabase
    , GetDatabase
    -- * Request Lenses
    , gddCatalogId
    , gddName

    -- * Destructuring the Response
    , getDatabaseResponse
    , GetDatabaseResponse
    -- * Response Lenses
    , gdrsDatabase
    , gdrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getDatabase' smart constructor.
data GetDatabase = GetDatabase'
  { _gddCatalogId :: !(Maybe Text)
  , _gddName      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDatabase' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gddCatalogId' - The ID of the Data Catalog in which the database resides. If none is supplied, the AWS account ID is used by default.
--
-- * 'gddName' - The name of the database to retrieve. For Hive compatibility, this should be all lowercase.
getDatabase
    :: Text -- ^ 'gddName'
    -> GetDatabase
getDatabase pName_ = GetDatabase' {_gddCatalogId = Nothing, _gddName = pName_}


-- | The ID of the Data Catalog in which the database resides. If none is supplied, the AWS account ID is used by default.
gddCatalogId :: Lens' GetDatabase (Maybe Text)
gddCatalogId = lens _gddCatalogId (\ s a -> s{_gddCatalogId = a})

-- | The name of the database to retrieve. For Hive compatibility, this should be all lowercase.
gddName :: Lens' GetDatabase Text
gddName = lens _gddName (\ s a -> s{_gddName = a})

instance AWSRequest GetDatabase where
        type Rs GetDatabase = GetDatabaseResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 GetDatabaseResponse' <$>
                   (x .?> "Database") <*> (pure (fromEnum s)))

instance Hashable GetDatabase where

instance NFData GetDatabase where

instance ToHeaders GetDatabase where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.GetDatabase" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetDatabase where
        toJSON GetDatabase'{..}
          = object
              (catMaybes
                 [("CatalogId" .=) <$> _gddCatalogId,
                  Just ("Name" .= _gddName)])

instance ToPath GetDatabase where
        toPath = const "/"

instance ToQuery GetDatabase where
        toQuery = const mempty

-- | /See:/ 'getDatabaseResponse' smart constructor.
data GetDatabaseResponse = GetDatabaseResponse'
  { _gdrsDatabase       :: !(Maybe Database)
  , _gdrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDatabaseResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdrsDatabase' - The definition of the specified database in the catalog.
--
-- * 'gdrsResponseStatus' - -- | The response status code.
getDatabaseResponse
    :: Int -- ^ 'gdrsResponseStatus'
    -> GetDatabaseResponse
getDatabaseResponse pResponseStatus_ =
  GetDatabaseResponse'
    {_gdrsDatabase = Nothing, _gdrsResponseStatus = pResponseStatus_}


-- | The definition of the specified database in the catalog.
gdrsDatabase :: Lens' GetDatabaseResponse (Maybe Database)
gdrsDatabase = lens _gdrsDatabase (\ s a -> s{_gdrsDatabase = a})

-- | -- | The response status code.
gdrsResponseStatus :: Lens' GetDatabaseResponse Int
gdrsResponseStatus = lens _gdrsResponseStatus (\ s a -> s{_gdrsResponseStatus = a})

instance NFData GetDatabaseResponse where
