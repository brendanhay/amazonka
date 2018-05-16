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
-- Module      : Network.AWS.AppSync.GetDataSource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a @DataSource@ object.
--
--
module Network.AWS.AppSync.GetDataSource
    (
    -- * Creating a Request
      getDataSource
    , GetDataSource
    -- * Request Lenses
    , gdsApiId
    , gdsName

    -- * Destructuring the Response
    , getDataSourceResponse
    , GetDataSourceResponse
    -- * Response Lenses
    , gdsrsDataSource
    , gdsrsResponseStatus
    ) where

import Network.AWS.AppSync.Types
import Network.AWS.AppSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getDataSource' smart constructor.
data GetDataSource = GetDataSource'
  { _gdsApiId :: !Text
  , _gdsName  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDataSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdsApiId' - The API ID.
--
-- * 'gdsName' - The name of the data source.
getDataSource
    :: Text -- ^ 'gdsApiId'
    -> Text -- ^ 'gdsName'
    -> GetDataSource
getDataSource pApiId_ pName_ =
  GetDataSource' {_gdsApiId = pApiId_, _gdsName = pName_}


-- | The API ID.
gdsApiId :: Lens' GetDataSource Text
gdsApiId = lens _gdsApiId (\ s a -> s{_gdsApiId = a})

-- | The name of the data source.
gdsName :: Lens' GetDataSource Text
gdsName = lens _gdsName (\ s a -> s{_gdsName = a})

instance AWSRequest GetDataSource where
        type Rs GetDataSource = GetDataSourceResponse
        request = get appSync
        response
          = receiveJSON
              (\ s h x ->
                 GetDataSourceResponse' <$>
                   (x .?> "dataSource") <*> (pure (fromEnum s)))

instance Hashable GetDataSource where

instance NFData GetDataSource where

instance ToHeaders GetDataSource where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetDataSource where
        toPath GetDataSource'{..}
          = mconcat
              ["/v1/apis/", toBS _gdsApiId, "/datasources/",
               toBS _gdsName]

instance ToQuery GetDataSource where
        toQuery = const mempty

-- | /See:/ 'getDataSourceResponse' smart constructor.
data GetDataSourceResponse = GetDataSourceResponse'
  { _gdsrsDataSource     :: !(Maybe DataSource)
  , _gdsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDataSourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdsrsDataSource' - The @DataSource@ object.
--
-- * 'gdsrsResponseStatus' - -- | The response status code.
getDataSourceResponse
    :: Int -- ^ 'gdsrsResponseStatus'
    -> GetDataSourceResponse
getDataSourceResponse pResponseStatus_ =
  GetDataSourceResponse'
    {_gdsrsDataSource = Nothing, _gdsrsResponseStatus = pResponseStatus_}


-- | The @DataSource@ object.
gdsrsDataSource :: Lens' GetDataSourceResponse (Maybe DataSource)
gdsrsDataSource = lens _gdsrsDataSource (\ s a -> s{_gdsrsDataSource = a})

-- | -- | The response status code.
gdsrsResponseStatus :: Lens' GetDataSourceResponse Int
gdsrsResponseStatus = lens _gdsrsResponseStatus (\ s a -> s{_gdsrsResponseStatus = a})

instance NFData GetDataSourceResponse where
