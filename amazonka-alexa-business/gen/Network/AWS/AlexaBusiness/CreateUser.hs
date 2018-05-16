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
-- Module      : Network.AWS.AlexaBusiness.CreateUser
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a user.
--
--
module Network.AWS.AlexaBusiness.CreateUser
    (
    -- * Creating a Request
      createUser
    , CreateUser
    -- * Request Lenses
    , cuEmail
    , cuLastName
    , cuFirstName
    , cuClientRequestToken
    , cuTags
    , cuUserId

    -- * Destructuring the Response
    , createUserResponse
    , CreateUserResponse
    -- * Response Lenses
    , cursUserARN
    , cursResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createUser' smart constructor.
data CreateUser = CreateUser'
  { _cuEmail              :: !(Maybe Text)
  , _cuLastName           :: !(Maybe Text)
  , _cuFirstName          :: !(Maybe Text)
  , _cuClientRequestToken :: !(Maybe Text)
  , _cuTags               :: !(Maybe [Tag])
  , _cuUserId             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cuEmail' - The email address for the user.
--
-- * 'cuLastName' - The last name for the user.
--
-- * 'cuFirstName' - The first name for the user.
--
-- * 'cuClientRequestToken' - A unique, user-specified identifier for this request that ensures idempotency.
--
-- * 'cuTags' - The tags for the user.
--
-- * 'cuUserId' - The ARN for the user.
createUser
    :: Text -- ^ 'cuUserId'
    -> CreateUser
createUser pUserId_ =
  CreateUser'
    { _cuEmail = Nothing
    , _cuLastName = Nothing
    , _cuFirstName = Nothing
    , _cuClientRequestToken = Nothing
    , _cuTags = Nothing
    , _cuUserId = pUserId_
    }


-- | The email address for the user.
cuEmail :: Lens' CreateUser (Maybe Text)
cuEmail = lens _cuEmail (\ s a -> s{_cuEmail = a})

-- | The last name for the user.
cuLastName :: Lens' CreateUser (Maybe Text)
cuLastName = lens _cuLastName (\ s a -> s{_cuLastName = a})

-- | The first name for the user.
cuFirstName :: Lens' CreateUser (Maybe Text)
cuFirstName = lens _cuFirstName (\ s a -> s{_cuFirstName = a})

-- | A unique, user-specified identifier for this request that ensures idempotency.
cuClientRequestToken :: Lens' CreateUser (Maybe Text)
cuClientRequestToken = lens _cuClientRequestToken (\ s a -> s{_cuClientRequestToken = a})

-- | The tags for the user.
cuTags :: Lens' CreateUser [Tag]
cuTags = lens _cuTags (\ s a -> s{_cuTags = a}) . _Default . _Coerce

-- | The ARN for the user.
cuUserId :: Lens' CreateUser Text
cuUserId = lens _cuUserId (\ s a -> s{_cuUserId = a})

instance AWSRequest CreateUser where
        type Rs CreateUser = CreateUserResponse
        request = postJSON alexaBusiness
        response
          = receiveJSON
              (\ s h x ->
                 CreateUserResponse' <$>
                   (x .?> "UserArn") <*> (pure (fromEnum s)))

instance Hashable CreateUser where

instance NFData CreateUser where

instance ToHeaders CreateUser where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.CreateUser" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateUser where
        toJSON CreateUser'{..}
          = object
              (catMaybes
                 [("Email" .=) <$> _cuEmail,
                  ("LastName" .=) <$> _cuLastName,
                  ("FirstName" .=) <$> _cuFirstName,
                  ("ClientRequestToken" .=) <$> _cuClientRequestToken,
                  ("Tags" .=) <$> _cuTags,
                  Just ("UserId" .= _cuUserId)])

instance ToPath CreateUser where
        toPath = const "/"

instance ToQuery CreateUser where
        toQuery = const mempty

-- | /See:/ 'createUserResponse' smart constructor.
data CreateUserResponse = CreateUserResponse'
  { _cursUserARN        :: !(Maybe Text)
  , _cursResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateUserResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cursUserARN' - The ARN of the newly created user in the response.
--
-- * 'cursResponseStatus' - -- | The response status code.
createUserResponse
    :: Int -- ^ 'cursResponseStatus'
    -> CreateUserResponse
createUserResponse pResponseStatus_ =
  CreateUserResponse'
    {_cursUserARN = Nothing, _cursResponseStatus = pResponseStatus_}


-- | The ARN of the newly created user in the response.
cursUserARN :: Lens' CreateUserResponse (Maybe Text)
cursUserARN = lens _cursUserARN (\ s a -> s{_cursUserARN = a})

-- | -- | The response status code.
cursResponseStatus :: Lens' CreateUserResponse Int
cursResponseStatus = lens _cursResponseStatus (\ s a -> s{_cursResponseStatus = a})

instance NFData CreateUserResponse where
