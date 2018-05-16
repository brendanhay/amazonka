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
-- Module      : Network.AWS.AppSync.GetGraphqlAPI
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a @GraphqlApi@ object.
--
--
module Network.AWS.AppSync.GetGraphqlAPI
    (
    -- * Creating a Request
      getGraphqlAPI
    , GetGraphqlAPI
    -- * Request Lenses
    , ggaApiId

    -- * Destructuring the Response
    , getGraphqlAPIResponse
    , GetGraphqlAPIResponse
    -- * Response Lenses
    , ggarsGraphqlAPI
    , ggarsResponseStatus
    ) where

import Network.AWS.AppSync.Types
import Network.AWS.AppSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getGraphqlAPI' smart constructor.
newtype GetGraphqlAPI = GetGraphqlAPI'
  { _ggaApiId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetGraphqlAPI' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ggaApiId' - The API ID for the GraphQL API.
getGraphqlAPI
    :: Text -- ^ 'ggaApiId'
    -> GetGraphqlAPI
getGraphqlAPI pApiId_ = GetGraphqlAPI' {_ggaApiId = pApiId_}


-- | The API ID for the GraphQL API.
ggaApiId :: Lens' GetGraphqlAPI Text
ggaApiId = lens _ggaApiId (\ s a -> s{_ggaApiId = a})

instance AWSRequest GetGraphqlAPI where
        type Rs GetGraphqlAPI = GetGraphqlAPIResponse
        request = get appSync
        response
          = receiveJSON
              (\ s h x ->
                 GetGraphqlAPIResponse' <$>
                   (x .?> "graphqlApi") <*> (pure (fromEnum s)))

instance Hashable GetGraphqlAPI where

instance NFData GetGraphqlAPI where

instance ToHeaders GetGraphqlAPI where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetGraphqlAPI where
        toPath GetGraphqlAPI'{..}
          = mconcat ["/v1/apis/", toBS _ggaApiId]

instance ToQuery GetGraphqlAPI where
        toQuery = const mempty

-- | /See:/ 'getGraphqlAPIResponse' smart constructor.
data GetGraphqlAPIResponse = GetGraphqlAPIResponse'
  { _ggarsGraphqlAPI     :: !(Maybe GraphqlAPI)
  , _ggarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetGraphqlAPIResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ggarsGraphqlAPI' - The @GraphqlApi@ object.
--
-- * 'ggarsResponseStatus' - -- | The response status code.
getGraphqlAPIResponse
    :: Int -- ^ 'ggarsResponseStatus'
    -> GetGraphqlAPIResponse
getGraphqlAPIResponse pResponseStatus_ =
  GetGraphqlAPIResponse'
    {_ggarsGraphqlAPI = Nothing, _ggarsResponseStatus = pResponseStatus_}


-- | The @GraphqlApi@ object.
ggarsGraphqlAPI :: Lens' GetGraphqlAPIResponse (Maybe GraphqlAPI)
ggarsGraphqlAPI = lens _ggarsGraphqlAPI (\ s a -> s{_ggarsGraphqlAPI = a})

-- | -- | The response status code.
ggarsResponseStatus :: Lens' GetGraphqlAPIResponse Int
ggarsResponseStatus = lens _ggarsResponseStatus (\ s a -> s{_ggarsResponseStatus = a})

instance NFData GetGraphqlAPIResponse where
