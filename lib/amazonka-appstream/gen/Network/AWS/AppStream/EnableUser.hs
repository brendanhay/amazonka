{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.EnableUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables a user in the user pool. After being enabled, users can sign in to AppStream 2.0 and open applications from the stacks to which they are assigned.
module Network.AWS.AppStream.EnableUser
  ( -- * Creating a Request
    enableUser,
    EnableUser,

    -- * Request Lenses
    euUserName,
    euAuthenticationType,

    -- * Destructuring the Response
    enableUserResponse,
    EnableUserResponse,

    -- * Response Lenses
    eursResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'enableUser' smart constructor.
data EnableUser = EnableUser'
  { _euUserName :: !(Sensitive Text),
    _euAuthenticationType :: !AuthenticationType
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'EnableUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'euUserName' - The email address of the user.
--
-- * 'euAuthenticationType' - The authentication type for the user. You must specify USERPOOL.
enableUser ::
  -- | 'euUserName'
  Text ->
  -- | 'euAuthenticationType'
  AuthenticationType ->
  EnableUser
enableUser pUserName_ pAuthenticationType_ =
  EnableUser'
    { _euUserName = _Sensitive # pUserName_,
      _euAuthenticationType = pAuthenticationType_
    }

-- | The email address of the user.
euUserName :: Lens' EnableUser Text
euUserName = lens _euUserName (\s a -> s {_euUserName = a}) . _Sensitive

-- | The authentication type for the user. You must specify USERPOOL.
euAuthenticationType :: Lens' EnableUser AuthenticationType
euAuthenticationType = lens _euAuthenticationType (\s a -> s {_euAuthenticationType = a})

instance AWSRequest EnableUser where
  type Rs EnableUser = EnableUserResponse
  request = postJSON appStream
  response =
    receiveEmpty
      (\s h x -> EnableUserResponse' <$> (pure (fromEnum s)))

instance Hashable EnableUser

instance NFData EnableUser

instance ToHeaders EnableUser where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("PhotonAdminProxyService.EnableUser" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON EnableUser where
  toJSON EnableUser' {..} =
    object
      ( catMaybes
          [ Just ("UserName" .= _euUserName),
            Just ("AuthenticationType" .= _euAuthenticationType)
          ]
      )

instance ToPath EnableUser where
  toPath = const "/"

instance ToQuery EnableUser where
  toQuery = const mempty

-- | /See:/ 'enableUserResponse' smart constructor.
newtype EnableUserResponse = EnableUserResponse'
  { _eursResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EnableUserResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eursResponseStatus' - -- | The response status code.
enableUserResponse ::
  -- | 'eursResponseStatus'
  Int ->
  EnableUserResponse
enableUserResponse pResponseStatus_ =
  EnableUserResponse' {_eursResponseStatus = pResponseStatus_}

-- | -- | The response status code.
eursResponseStatus :: Lens' EnableUserResponse Int
eursResponseStatus = lens _eursResponseStatus (\s a -> s {_eursResponseStatus = a})

instance NFData EnableUserResponse
