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
-- Module      : Network.AWS.MQ.UpdateUser
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the information for an ActiveMQ user.
module Network.AWS.MQ.UpdateUser
    (
    -- * Creating a Request
      updateUser
    , UpdateUser
    -- * Request Lenses
    , uuGroups
    , uuConsoleAccess
    , uuPassword
    , uuUsername
    , uuBrokerId

    -- * Destructuring the Response
    , updateUserResponse
    , UpdateUserResponse
    -- * Response Lenses
    , uursResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MQ.Types
import Network.AWS.MQ.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Updates the information for an ActiveMQ user.
--
-- /See:/ 'updateUser' smart constructor.
data UpdateUser = UpdateUser'
  { _uuGroups        :: !(Maybe [Text])
  , _uuConsoleAccess :: !(Maybe Bool)
  , _uuPassword      :: !(Maybe Text)
  , _uuUsername      :: !Text
  , _uuBrokerId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uuGroups' - The list of groups (20 maximum) to which the ActiveMQ user belongs. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
--
-- * 'uuConsoleAccess' - Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
--
-- * 'uuPassword' - The password of the user. This value must be at least 12 characters long, must contain at least 4 unique characters, and must not contain commas.
--
-- * 'uuUsername' - Required. The username of the ActiveMQ user. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
--
-- * 'uuBrokerId' - The unique ID that Amazon MQ generates for the broker.
updateUser
    :: Text -- ^ 'uuUsername'
    -> Text -- ^ 'uuBrokerId'
    -> UpdateUser
updateUser pUsername_ pBrokerId_ =
  UpdateUser'
    { _uuGroups = Nothing
    , _uuConsoleAccess = Nothing
    , _uuPassword = Nothing
    , _uuUsername = pUsername_
    , _uuBrokerId = pBrokerId_
    }


-- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
uuGroups :: Lens' UpdateUser [Text]
uuGroups = lens _uuGroups (\ s a -> s{_uuGroups = a}) . _Default . _Coerce

-- | Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
uuConsoleAccess :: Lens' UpdateUser (Maybe Bool)
uuConsoleAccess = lens _uuConsoleAccess (\ s a -> s{_uuConsoleAccess = a})

-- | The password of the user. This value must be at least 12 characters long, must contain at least 4 unique characters, and must not contain commas.
uuPassword :: Lens' UpdateUser (Maybe Text)
uuPassword = lens _uuPassword (\ s a -> s{_uuPassword = a})

-- | Required. The username of the ActiveMQ user. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
uuUsername :: Lens' UpdateUser Text
uuUsername = lens _uuUsername (\ s a -> s{_uuUsername = a})

-- | The unique ID that Amazon MQ generates for the broker.
uuBrokerId :: Lens' UpdateUser Text
uuBrokerId = lens _uuBrokerId (\ s a -> s{_uuBrokerId = a})

instance AWSRequest UpdateUser where
        type Rs UpdateUser = UpdateUserResponse
        request = putJSON mq
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateUserResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateUser where

instance NFData UpdateUser where

instance ToHeaders UpdateUser where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateUser where
        toJSON UpdateUser'{..}
          = object
              (catMaybes
                 [("groups" .=) <$> _uuGroups,
                  ("consoleAccess" .=) <$> _uuConsoleAccess,
                  ("password" .=) <$> _uuPassword])

instance ToPath UpdateUser where
        toPath UpdateUser'{..}
          = mconcat
              ["/v1/brokers/", toBS _uuBrokerId, "/users/",
               toBS _uuUsername]

instance ToQuery UpdateUser where
        toQuery = const mempty

-- | /See:/ 'updateUserResponse' smart constructor.
newtype UpdateUserResponse = UpdateUserResponse'
  { _uursResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateUserResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uursResponseStatus' - -- | The response status code.
updateUserResponse
    :: Int -- ^ 'uursResponseStatus'
    -> UpdateUserResponse
updateUserResponse pResponseStatus_ =
  UpdateUserResponse' {_uursResponseStatus = pResponseStatus_}


-- | -- | The response status code.
uursResponseStatus :: Lens' UpdateUserResponse Int
uursResponseStatus = lens _uursResponseStatus (\ s a -> s{_uursResponseStatus = a})

instance NFData UpdateUserResponse where
