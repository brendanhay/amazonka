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
-- Module      : Network.AWS.AppSync.DeleteAPIKey
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an API key.
--
--
module Network.AWS.AppSync.DeleteAPIKey
    (
    -- * Creating a Request
      deleteAPIKey
    , DeleteAPIKey
    -- * Request Lenses
    , dakApiId
    , dakId

    -- * Destructuring the Response
    , deleteAPIKeyResponse
    , DeleteAPIKeyResponse
    -- * Response Lenses
    , dakrsResponseStatus
    ) where

import Network.AWS.AppSync.Types
import Network.AWS.AppSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteAPIKey' smart constructor.
data DeleteAPIKey = DeleteAPIKey'
  { _dakApiId :: !Text
  , _dakId    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAPIKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dakApiId' - The API ID.
--
-- * 'dakId' - The ID for the API key.
deleteAPIKey
    :: Text -- ^ 'dakApiId'
    -> Text -- ^ 'dakId'
    -> DeleteAPIKey
deleteAPIKey pApiId_ pId_ = DeleteAPIKey' {_dakApiId = pApiId_, _dakId = pId_}


-- | The API ID.
dakApiId :: Lens' DeleteAPIKey Text
dakApiId = lens _dakApiId (\ s a -> s{_dakApiId = a})

-- | The ID for the API key.
dakId :: Lens' DeleteAPIKey Text
dakId = lens _dakId (\ s a -> s{_dakId = a})

instance AWSRequest DeleteAPIKey where
        type Rs DeleteAPIKey = DeleteAPIKeyResponse
        request = delete appSync
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteAPIKeyResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteAPIKey where

instance NFData DeleteAPIKey where

instance ToHeaders DeleteAPIKey where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteAPIKey where
        toPath DeleteAPIKey'{..}
          = mconcat
              ["/v1/apis/", toBS _dakApiId, "/apikeys/",
               toBS _dakId]

instance ToQuery DeleteAPIKey where
        toQuery = const mempty

-- | /See:/ 'deleteAPIKeyResponse' smart constructor.
newtype DeleteAPIKeyResponse = DeleteAPIKeyResponse'
  { _dakrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAPIKeyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dakrsResponseStatus' - -- | The response status code.
deleteAPIKeyResponse
    :: Int -- ^ 'dakrsResponseStatus'
    -> DeleteAPIKeyResponse
deleteAPIKeyResponse pResponseStatus_ =
  DeleteAPIKeyResponse' {_dakrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dakrsResponseStatus :: Lens' DeleteAPIKeyResponse Int
dakrsResponseStatus = lens _dakrsResponseStatus (\ s a -> s{_dakrsResponseStatus = a})

instance NFData DeleteAPIKeyResponse where
