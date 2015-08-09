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
-- Module      : Network.AWS.IAM.ChangePassword
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
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
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ChangePassword.html AWS API Reference> for ChangePassword.
module Network.AWS.IAM.ChangePassword
    (
    -- * Creating a Request
      ChangePassword
    , changePassword
    -- * Request Lenses
    , cpOldPassword
    , cpNewPassword

    -- * Destructuring the Response
    , ChangePasswordResponse
    , changePasswordResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'changePassword' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpOldPassword'
--
-- * 'cpNewPassword'
data ChangePassword = ChangePassword'
    { _cpOldPassword :: !(Sensitive Text)
    , _cpNewPassword :: !(Sensitive Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ChangePassword' smart constructor.
changePassword :: Text -> Text -> ChangePassword
changePassword pOldPassword_ pNewPassword_ =
    ChangePassword'
    { _cpOldPassword = _Sensitive # pOldPassword_
    , _cpNewPassword = _Sensitive # pNewPassword_
    }

-- | The IAM user\'s current password.
cpOldPassword :: Lens' ChangePassword Text
cpOldPassword = lens _cpOldPassword (\ s a -> s{_cpOldPassword = a}) . _Sensitive;

-- | The new password. The new password must conform to the AWS account\'s
-- password policy, if one exists.
cpNewPassword :: Lens' ChangePassword Text
cpNewPassword = lens _cpNewPassword (\ s a -> s{_cpNewPassword = a}) . _Sensitive;

instance AWSRequest ChangePassword where
        type Sv ChangePassword = IAM
        type Rs ChangePassword = ChangePasswordResponse
        request = postQuery
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
               "OldPassword" =: _cpOldPassword,
               "NewPassword" =: _cpNewPassword]

-- | /See:/ 'changePasswordResponse' smart constructor.
data ChangePasswordResponse =
    ChangePasswordResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ChangePasswordResponse' smart constructor.
changePasswordResponse :: ChangePasswordResponse
changePasswordResponse = ChangePasswordResponse'
