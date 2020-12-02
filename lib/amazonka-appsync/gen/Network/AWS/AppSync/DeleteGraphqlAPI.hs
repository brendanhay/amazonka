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
-- Module      : Network.AWS.AppSync.DeleteGraphqlAPI
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a @GraphqlApi@ object.
--
--
module Network.AWS.AppSync.DeleteGraphqlAPI
    (
    -- * Creating a Request
      deleteGraphqlAPI
    , DeleteGraphqlAPI
    -- * Request Lenses
    , dgaApiId

    -- * Destructuring the Response
    , deleteGraphqlAPIResponse
    , DeleteGraphqlAPIResponse
    -- * Response Lenses
    , dgarsResponseStatus
    ) where

import Network.AWS.AppSync.Types
import Network.AWS.AppSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteGraphqlAPI' smart constructor.
newtype DeleteGraphqlAPI = DeleteGraphqlAPI'
  { _dgaApiId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteGraphqlAPI' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgaApiId' - The API ID.
deleteGraphqlAPI
    :: Text -- ^ 'dgaApiId'
    -> DeleteGraphqlAPI
deleteGraphqlAPI pApiId_ = DeleteGraphqlAPI' {_dgaApiId = pApiId_}


-- | The API ID.
dgaApiId :: Lens' DeleteGraphqlAPI Text
dgaApiId = lens _dgaApiId (\ s a -> s{_dgaApiId = a})

instance AWSRequest DeleteGraphqlAPI where
        type Rs DeleteGraphqlAPI = DeleteGraphqlAPIResponse
        request = delete appSync
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteGraphqlAPIResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteGraphqlAPI where

instance NFData DeleteGraphqlAPI where

instance ToHeaders DeleteGraphqlAPI where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteGraphqlAPI where
        toPath DeleteGraphqlAPI'{..}
          = mconcat ["/v1/apis/", toBS _dgaApiId]

instance ToQuery DeleteGraphqlAPI where
        toQuery = const mempty

-- | /See:/ 'deleteGraphqlAPIResponse' smart constructor.
newtype DeleteGraphqlAPIResponse = DeleteGraphqlAPIResponse'
  { _dgarsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteGraphqlAPIResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgarsResponseStatus' - -- | The response status code.
deleteGraphqlAPIResponse
    :: Int -- ^ 'dgarsResponseStatus'
    -> DeleteGraphqlAPIResponse
deleteGraphqlAPIResponse pResponseStatus_ =
  DeleteGraphqlAPIResponse' {_dgarsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dgarsResponseStatus :: Lens' DeleteGraphqlAPIResponse Int
dgarsResponseStatus = lens _dgarsResponseStatus (\ s a -> s{_dgarsResponseStatus = a})

instance NFData DeleteGraphqlAPIResponse where
