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
-- Module      : Network.AWS.WorkDocs.ActivateUser
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Activates the specified user. Only active users can access Amazon WorkDocs.
--
--
module Network.AWS.WorkDocs.ActivateUser
    (
    -- * Creating a Request
      activateUser
    , ActivateUser
    -- * Request Lenses
    , auAuthenticationToken
    , auUserId

    -- * Destructuring the Response
    , activateUserResponse
    , ActivateUserResponse
    -- * Response Lenses
    , aursUser
    , aursResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkDocs.Types
import Network.AWS.WorkDocs.Types.Product

-- | /See:/ 'activateUser' smart constructor.
data ActivateUser = ActivateUser'
  { _auAuthenticationToken :: !(Maybe (Sensitive Text))
  , _auUserId              :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'ActivateUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'auAuthenticationToken' - Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
--
-- * 'auUserId' - The ID of the user.
activateUser
    :: Text -- ^ 'auUserId'
    -> ActivateUser
activateUser pUserId_ =
  ActivateUser' {_auAuthenticationToken = Nothing, _auUserId = pUserId_}


-- | Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
auAuthenticationToken :: Lens' ActivateUser (Maybe Text)
auAuthenticationToken = lens _auAuthenticationToken (\ s a -> s{_auAuthenticationToken = a}) . mapping _Sensitive

-- | The ID of the user.
auUserId :: Lens' ActivateUser Text
auUserId = lens _auUserId (\ s a -> s{_auUserId = a})

instance AWSRequest ActivateUser where
        type Rs ActivateUser = ActivateUserResponse
        request = postJSON workDocs
        response
          = receiveJSON
              (\ s h x ->
                 ActivateUserResponse' <$>
                   (x .?> "User") <*> (pure (fromEnum s)))

instance Hashable ActivateUser where

instance NFData ActivateUser where

instance ToHeaders ActivateUser where
        toHeaders ActivateUser'{..}
          = mconcat
              ["Authentication" =# _auAuthenticationToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToJSON ActivateUser where
        toJSON = const (Object mempty)

instance ToPath ActivateUser where
        toPath ActivateUser'{..}
          = mconcat
              ["/api/v1/users/", toBS _auUserId, "/activation"]

instance ToQuery ActivateUser where
        toQuery = const mempty

-- | /See:/ 'activateUserResponse' smart constructor.
data ActivateUserResponse = ActivateUserResponse'
  { _aursUser           :: !(Maybe User)
  , _aursResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ActivateUserResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aursUser' - The user information.
--
-- * 'aursResponseStatus' - -- | The response status code.
activateUserResponse
    :: Int -- ^ 'aursResponseStatus'
    -> ActivateUserResponse
activateUserResponse pResponseStatus_ =
  ActivateUserResponse'
    {_aursUser = Nothing, _aursResponseStatus = pResponseStatus_}


-- | The user information.
aursUser :: Lens' ActivateUserResponse (Maybe User)
aursUser = lens _aursUser (\ s a -> s{_aursUser = a})

-- | -- | The response status code.
aursResponseStatus :: Lens' ActivateUserResponse Int
aursResponseStatus = lens _aursResponseStatus (\ s a -> s{_aursResponseStatus = a})

instance NFData ActivateUserResponse where
