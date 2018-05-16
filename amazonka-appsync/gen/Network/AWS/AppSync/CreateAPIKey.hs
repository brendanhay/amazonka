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
-- Module      : Network.AWS.AppSync.CreateAPIKey
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a unique key that you can distribute to clients who are executing your API.
--
--
module Network.AWS.AppSync.CreateAPIKey
    (
    -- * Creating a Request
      createAPIKey
    , CreateAPIKey
    -- * Request Lenses
    , cakExpires
    , cakDescription
    , cakApiId

    -- * Destructuring the Response
    , createAPIKeyResponse
    , CreateAPIKeyResponse
    -- * Response Lenses
    , cakrsApiKey
    , cakrsResponseStatus
    ) where

import Network.AWS.AppSync.Types
import Network.AWS.AppSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createAPIKey' smart constructor.
data CreateAPIKey = CreateAPIKey'
  { _cakExpires     :: !(Maybe Integer)
  , _cakDescription :: !(Maybe Text)
  , _cakApiId       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateAPIKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cakExpires' - The time from creation time after which the API key expires. The date is represented as seconds since the epoch, rounded down to the nearest hour. The default value for this parameter is 7 days from creation time. For more information, see .
--
-- * 'cakDescription' - A description of the purpose of the API key.
--
-- * 'cakApiId' - The ID for your GraphQL API.
createAPIKey
    :: Text -- ^ 'cakApiId'
    -> CreateAPIKey
createAPIKey pApiId_ =
  CreateAPIKey'
    {_cakExpires = Nothing, _cakDescription = Nothing, _cakApiId = pApiId_}


-- | The time from creation time after which the API key expires. The date is represented as seconds since the epoch, rounded down to the nearest hour. The default value for this parameter is 7 days from creation time. For more information, see .
cakExpires :: Lens' CreateAPIKey (Maybe Integer)
cakExpires = lens _cakExpires (\ s a -> s{_cakExpires = a})

-- | A description of the purpose of the API key.
cakDescription :: Lens' CreateAPIKey (Maybe Text)
cakDescription = lens _cakDescription (\ s a -> s{_cakDescription = a})

-- | The ID for your GraphQL API.
cakApiId :: Lens' CreateAPIKey Text
cakApiId = lens _cakApiId (\ s a -> s{_cakApiId = a})

instance AWSRequest CreateAPIKey where
        type Rs CreateAPIKey = CreateAPIKeyResponse
        request = postJSON appSync
        response
          = receiveJSON
              (\ s h x ->
                 CreateAPIKeyResponse' <$>
                   (x .?> "apiKey") <*> (pure (fromEnum s)))

instance Hashable CreateAPIKey where

instance NFData CreateAPIKey where

instance ToHeaders CreateAPIKey where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateAPIKey where
        toJSON CreateAPIKey'{..}
          = object
              (catMaybes
                 [("expires" .=) <$> _cakExpires,
                  ("description" .=) <$> _cakDescription])

instance ToPath CreateAPIKey where
        toPath CreateAPIKey'{..}
          = mconcat ["/v1/apis/", toBS _cakApiId, "/apikeys"]

instance ToQuery CreateAPIKey where
        toQuery = const mempty

-- | /See:/ 'createAPIKeyResponse' smart constructor.
data CreateAPIKeyResponse = CreateAPIKeyResponse'
  { _cakrsApiKey         :: !(Maybe APIKey)
  , _cakrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateAPIKeyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cakrsApiKey' - The API key.
--
-- * 'cakrsResponseStatus' - -- | The response status code.
createAPIKeyResponse
    :: Int -- ^ 'cakrsResponseStatus'
    -> CreateAPIKeyResponse
createAPIKeyResponse pResponseStatus_ =
  CreateAPIKeyResponse'
    {_cakrsApiKey = Nothing, _cakrsResponseStatus = pResponseStatus_}


-- | The API key.
cakrsApiKey :: Lens' CreateAPIKeyResponse (Maybe APIKey)
cakrsApiKey = lens _cakrsApiKey (\ s a -> s{_cakrsApiKey = a})

-- | -- | The response status code.
cakrsResponseStatus :: Lens' CreateAPIKeyResponse Int
cakrsResponseStatus = lens _cakrsResponseStatus (\ s a -> s{_cakrsResponseStatus = a})

instance NFData CreateAPIKeyResponse where
