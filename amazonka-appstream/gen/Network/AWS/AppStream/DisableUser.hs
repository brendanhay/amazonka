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
-- Module      : Network.AWS.AppStream.DisableUser
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the specified user in the user pool. Users can't sign in to AppStream 2.0 until they are re-enabled. This action does not delete the user.
--
--
module Network.AWS.AppStream.DisableUser
    (
    -- * Creating a Request
      disableUser
    , DisableUser
    -- * Request Lenses
    , dUserName
    , dAuthenticationType

    -- * Destructuring the Response
    , disableUserResponse
    , DisableUserResponse
    -- * Response Lenses
    , disrsResponseStatus
    ) where

import Network.AWS.AppStream.Types
import Network.AWS.AppStream.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disableUser' smart constructor.
data DisableUser = DisableUser'
  { _dUserName           :: !(Sensitive Text)
  , _dAuthenticationType :: !AuthenticationType
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisableUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dUserName' - The email address of the user.
--
-- * 'dAuthenticationType' - The authentication type for the user. You must specify USERPOOL.
disableUser
    :: Text -- ^ 'dUserName'
    -> AuthenticationType -- ^ 'dAuthenticationType'
    -> DisableUser
disableUser pUserName_ pAuthenticationType_ =
  DisableUser'
    { _dUserName = _Sensitive # pUserName_
    , _dAuthenticationType = pAuthenticationType_
    }


-- | The email address of the user.
dUserName :: Lens' DisableUser Text
dUserName = lens _dUserName (\ s a -> s{_dUserName = a}) . _Sensitive

-- | The authentication type for the user. You must specify USERPOOL.
dAuthenticationType :: Lens' DisableUser AuthenticationType
dAuthenticationType = lens _dAuthenticationType (\ s a -> s{_dAuthenticationType = a})

instance AWSRequest DisableUser where
        type Rs DisableUser = DisableUserResponse
        request = postJSON appStream
        response
          = receiveEmpty
              (\ s h x ->
                 DisableUserResponse' <$> (pure (fromEnum s)))

instance Hashable DisableUser where

instance NFData DisableUser where

instance ToHeaders DisableUser where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("PhotonAdminProxyService.DisableUser" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DisableUser where
        toJSON DisableUser'{..}
          = object
              (catMaybes
                 [Just ("UserName" .= _dUserName),
                  Just ("AuthenticationType" .= _dAuthenticationType)])

instance ToPath DisableUser where
        toPath = const "/"

instance ToQuery DisableUser where
        toQuery = const mempty

-- | /See:/ 'disableUserResponse' smart constructor.
newtype DisableUserResponse = DisableUserResponse'
  { _disrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisableUserResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'disrsResponseStatus' - -- | The response status code.
disableUserResponse
    :: Int -- ^ 'disrsResponseStatus'
    -> DisableUserResponse
disableUserResponse pResponseStatus_ =
  DisableUserResponse' {_disrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
disrsResponseStatus :: Lens' DisableUserResponse Int
disrsResponseStatus = lens _disrsResponseStatus (\ s a -> s{_disrsResponseStatus = a})

instance NFData DisableUserResponse where
