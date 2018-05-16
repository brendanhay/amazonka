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
-- Module      : Network.AWS.AppSync.DeleteDataSource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a @DataSource@ object.
--
--
module Network.AWS.AppSync.DeleteDataSource
    (
    -- * Creating a Request
      deleteDataSource
    , DeleteDataSource
    -- * Request Lenses
    , ddsApiId
    , ddsName

    -- * Destructuring the Response
    , deleteDataSourceResponse
    , DeleteDataSourceResponse
    -- * Response Lenses
    , ddsrsResponseStatus
    ) where

import Network.AWS.AppSync.Types
import Network.AWS.AppSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteDataSource' smart constructor.
data DeleteDataSource = DeleteDataSource'
  { _ddsApiId :: !Text
  , _ddsName  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDataSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddsApiId' - The API ID.
--
-- * 'ddsName' - The name of the data source.
deleteDataSource
    :: Text -- ^ 'ddsApiId'
    -> Text -- ^ 'ddsName'
    -> DeleteDataSource
deleteDataSource pApiId_ pName_ =
  DeleteDataSource' {_ddsApiId = pApiId_, _ddsName = pName_}


-- | The API ID.
ddsApiId :: Lens' DeleteDataSource Text
ddsApiId = lens _ddsApiId (\ s a -> s{_ddsApiId = a})

-- | The name of the data source.
ddsName :: Lens' DeleteDataSource Text
ddsName = lens _ddsName (\ s a -> s{_ddsName = a})

instance AWSRequest DeleteDataSource where
        type Rs DeleteDataSource = DeleteDataSourceResponse
        request = delete appSync
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteDataSourceResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteDataSource where

instance NFData DeleteDataSource where

instance ToHeaders DeleteDataSource where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteDataSource where
        toPath DeleteDataSource'{..}
          = mconcat
              ["/v1/apis/", toBS _ddsApiId, "/datasources/",
               toBS _ddsName]

instance ToQuery DeleteDataSource where
        toQuery = const mempty

-- | /See:/ 'deleteDataSourceResponse' smart constructor.
newtype DeleteDataSourceResponse = DeleteDataSourceResponse'
  { _ddsrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDataSourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddsrsResponseStatus' - -- | The response status code.
deleteDataSourceResponse
    :: Int -- ^ 'ddsrsResponseStatus'
    -> DeleteDataSourceResponse
deleteDataSourceResponse pResponseStatus_ =
  DeleteDataSourceResponse' {_ddsrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ddsrsResponseStatus :: Lens' DeleteDataSourceResponse Int
ddsrsResponseStatus = lens _ddsrsResponseStatus (\ s a -> s{_ddsrsResponseStatus = a})

instance NFData DeleteDataSourceResponse where
