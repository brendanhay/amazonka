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
-- Module      : Network.AWS.WorkDocs.GetCurrentUser
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves details of the current user for whom the authentication token was generated. This is not a valid action for SigV4 (administrative API) clients.
--
--
module Network.AWS.WorkDocs.GetCurrentUser
    (
    -- * Creating a Request
      getCurrentUser
    , GetCurrentUser
    -- * Request Lenses
    , gcuAuthenticationToken

    -- * Destructuring the Response
    , getCurrentUserResponse
    , GetCurrentUserResponse
    -- * Response Lenses
    , gcursUser
    , gcursResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkDocs.Types
import Network.AWS.WorkDocs.Types.Product

-- | /See:/ 'getCurrentUser' smart constructor.
newtype GetCurrentUser = GetCurrentUser'
  { _gcuAuthenticationToken :: Sensitive Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCurrentUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcuAuthenticationToken' - Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
getCurrentUser
    :: Text -- ^ 'gcuAuthenticationToken'
    -> GetCurrentUser
getCurrentUser pAuthenticationToken_ =
  GetCurrentUser' {_gcuAuthenticationToken = _Sensitive # pAuthenticationToken_}


-- | Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
gcuAuthenticationToken :: Lens' GetCurrentUser Text
gcuAuthenticationToken = lens _gcuAuthenticationToken (\ s a -> s{_gcuAuthenticationToken = a}) . _Sensitive

instance AWSRequest GetCurrentUser where
        type Rs GetCurrentUser = GetCurrentUserResponse
        request = get workDocs
        response
          = receiveJSON
              (\ s h x ->
                 GetCurrentUserResponse' <$>
                   (x .?> "User") <*> (pure (fromEnum s)))

instance Hashable GetCurrentUser where

instance NFData GetCurrentUser where

instance ToHeaders GetCurrentUser where
        toHeaders GetCurrentUser'{..}
          = mconcat
              ["Authentication" =# _gcuAuthenticationToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToPath GetCurrentUser where
        toPath = const "/api/v1/me"

instance ToQuery GetCurrentUser where
        toQuery = const mempty

-- | /See:/ 'getCurrentUserResponse' smart constructor.
data GetCurrentUserResponse = GetCurrentUserResponse'
  { _gcursUser           :: !(Maybe User)
  , _gcursResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCurrentUserResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcursUser' - Metadata of the user.
--
-- * 'gcursResponseStatus' - -- | The response status code.
getCurrentUserResponse
    :: Int -- ^ 'gcursResponseStatus'
    -> GetCurrentUserResponse
getCurrentUserResponse pResponseStatus_ =
  GetCurrentUserResponse'
    {_gcursUser = Nothing, _gcursResponseStatus = pResponseStatus_}


-- | Metadata of the user.
gcursUser :: Lens' GetCurrentUserResponse (Maybe User)
gcursUser = lens _gcursUser (\ s a -> s{_gcursUser = a})

-- | -- | The response status code.
gcursResponseStatus :: Lens' GetCurrentUserResponse Int
gcursResponseStatus = lens _gcursResponseStatus (\ s a -> s{_gcursResponseStatus = a})

instance NFData GetCurrentUserResponse where
