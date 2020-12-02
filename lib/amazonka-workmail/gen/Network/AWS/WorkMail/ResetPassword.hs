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
-- Module      : Network.AWS.WorkMail.ResetPassword
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows the administrator to reset the password for a user.
--
--
module Network.AWS.WorkMail.ResetPassword
    (
    -- * Creating a Request
      resetPassword
    , ResetPassword
    -- * Request Lenses
    , rpOrganizationId
    , rpUserId
    , rpPassword

    -- * Destructuring the Response
    , resetPasswordResponse
    , ResetPasswordResponse
    -- * Response Lenses
    , rprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkMail.Types
import Network.AWS.WorkMail.Types.Product

-- | /See:/ 'resetPassword' smart constructor.
data ResetPassword = ResetPassword'
  { _rpOrganizationId :: !Text
  , _rpUserId         :: !Text
  , _rpPassword       :: !(Sensitive Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResetPassword' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpOrganizationId' - The identifier of the organization that contains the user for which the password is reset.
--
-- * 'rpUserId' - The identifier of the user for whom the password is reset.
--
-- * 'rpPassword' - The new password for the user.
resetPassword
    :: Text -- ^ 'rpOrganizationId'
    -> Text -- ^ 'rpUserId'
    -> Text -- ^ 'rpPassword'
    -> ResetPassword
resetPassword pOrganizationId_ pUserId_ pPassword_ =
  ResetPassword'
    { _rpOrganizationId = pOrganizationId_
    , _rpUserId = pUserId_
    , _rpPassword = _Sensitive # pPassword_
    }


-- | The identifier of the organization that contains the user for which the password is reset.
rpOrganizationId :: Lens' ResetPassword Text
rpOrganizationId = lens _rpOrganizationId (\ s a -> s{_rpOrganizationId = a})

-- | The identifier of the user for whom the password is reset.
rpUserId :: Lens' ResetPassword Text
rpUserId = lens _rpUserId (\ s a -> s{_rpUserId = a})

-- | The new password for the user.
rpPassword :: Lens' ResetPassword Text
rpPassword = lens _rpPassword (\ s a -> s{_rpPassword = a}) . _Sensitive

instance AWSRequest ResetPassword where
        type Rs ResetPassword = ResetPasswordResponse
        request = postJSON workMail
        response
          = receiveEmpty
              (\ s h x ->
                 ResetPasswordResponse' <$> (pure (fromEnum s)))

instance Hashable ResetPassword where

instance NFData ResetPassword where

instance ToHeaders ResetPassword where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("WorkMailService.ResetPassword" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ResetPassword where
        toJSON ResetPassword'{..}
          = object
              (catMaybes
                 [Just ("OrganizationId" .= _rpOrganizationId),
                  Just ("UserId" .= _rpUserId),
                  Just ("Password" .= _rpPassword)])

instance ToPath ResetPassword where
        toPath = const "/"

instance ToQuery ResetPassword where
        toQuery = const mempty

-- | /See:/ 'resetPasswordResponse' smart constructor.
newtype ResetPasswordResponse = ResetPasswordResponse'
  { _rprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResetPasswordResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rprsResponseStatus' - -- | The response status code.
resetPasswordResponse
    :: Int -- ^ 'rprsResponseStatus'
    -> ResetPasswordResponse
resetPasswordResponse pResponseStatus_ =
  ResetPasswordResponse' {_rprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
rprsResponseStatus :: Lens' ResetPasswordResponse Int
rprsResponseStatus = lens _rprsResponseStatus (\ s a -> s{_rprsResponseStatus = a})

instance NFData ResetPasswordResponse where
