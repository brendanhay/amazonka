{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ChangePassword
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Changes the password of the IAM user who is calling this action. The
-- root account password is not affected by this action.
--
-- To change the password for a different user, see UpdateLoginProfile. For
-- more information about modifying passwords, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_ManagingLogins.html Managing Passwords>
-- in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ChangePassword.html>
module Network.AWS.IAM.ChangePassword
    (
    -- * Request
      ChangePassword
    -- ** Request constructor
    , changePassword
    -- ** Request lenses
    , cprqOldPassword
    , cprqNewPassword

    -- * Response
    , ChangePasswordResponse
    -- ** Response constructor
    , changePasswordResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'changePassword' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cprqOldPassword'
--
-- * 'cprqNewPassword'
data ChangePassword = ChangePassword'
    { _cprqOldPassword :: !(Sensitive Text)
    , _cprqNewPassword :: !(Sensitive Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ChangePassword' smart constructor.
changePassword :: Text -> Text -> ChangePassword
changePassword pOldPassword pNewPassword =
    ChangePassword'
    { _cprqOldPassword = _Sensitive # pOldPassword
    , _cprqNewPassword = _Sensitive # pNewPassword
    }

-- | The IAM user\'s current password.
cprqOldPassword :: Lens' ChangePassword Text
cprqOldPassword = lens _cprqOldPassword (\ s a -> s{_cprqOldPassword = a}) . _Sensitive;

-- | The new password. The new password must conform to the AWS account\'s
-- password policy, if one exists.
cprqNewPassword :: Lens' ChangePassword Text
cprqNewPassword = lens _cprqNewPassword (\ s a -> s{_cprqNewPassword = a}) . _Sensitive;

instance AWSRequest ChangePassword where
        type Sv ChangePassword = IAM
        type Rs ChangePassword = ChangePasswordResponse
        request = post
        response = receiveNull ChangePasswordResponse'

instance ToHeaders ChangePassword where
        toHeaders = const mempty

instance ToPath ChangePassword where
        toPath = const "/"

instance ToQuery ChangePassword where
        toQuery ChangePassword'{..}
          = mconcat
              ["Action" =: ("ChangePassword" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "OldPassword" =: _cprqOldPassword,
               "NewPassword" =: _cprqNewPassword]

-- | /See:/ 'changePasswordResponse' smart constructor.
data ChangePasswordResponse =
    ChangePasswordResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ChangePasswordResponse' smart constructor.
changePasswordResponse :: ChangePasswordResponse
changePasswordResponse = ChangePasswordResponse'
