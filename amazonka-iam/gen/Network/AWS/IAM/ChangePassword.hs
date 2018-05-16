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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the password of the IAM user who is calling this operation. The AWS account root user password is not affected by this operation.
--
--
-- To change the password for a different user, see 'UpdateLoginProfile' . For more information about modifying passwords, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_ManagingLogins.html Managing Passwords> in the /IAM User Guide/ .
--
module Network.AWS.IAM.ChangePassword
    (
    -- * Creating a Request
      changePassword
    , ChangePassword
    -- * Request Lenses
    , cpOldPassword
    , cpNewPassword

    -- * Destructuring the Response
    , changePasswordResponse
    , ChangePasswordResponse
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'changePassword' smart constructor.
data ChangePassword = ChangePassword'
  { _cpOldPassword :: !(Sensitive Text)
  , _cpNewPassword :: !(Sensitive Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'ChangePassword' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpOldPassword' - The IAM user's current password.
--
-- * 'cpNewPassword' - The new password. The new password must conform to the AWS account's password policy, if one exists. The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of characters. That string can include almost any printable ASCII character from the space (\u0020) through the end of the ASCII character range (\u00FF). You can also include the tab (\u0009), line feed (\u000A), and carriage return (\u000D) characters. Any of these characters are valid in a password. However, many tools, such as the AWS Management Console, might restrict the ability to type certain characters because they have special meaning within that tool.
changePassword
    :: Text -- ^ 'cpOldPassword'
    -> Text -- ^ 'cpNewPassword'
    -> ChangePassword
changePassword pOldPassword_ pNewPassword_ =
  ChangePassword'
    { _cpOldPassword = _Sensitive # pOldPassword_
    , _cpNewPassword = _Sensitive # pNewPassword_
    }


-- | The IAM user's current password.
cpOldPassword :: Lens' ChangePassword Text
cpOldPassword = lens _cpOldPassword (\ s a -> s{_cpOldPassword = a}) . _Sensitive

-- | The new password. The new password must conform to the AWS account's password policy, if one exists. The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of characters. That string can include almost any printable ASCII character from the space (\u0020) through the end of the ASCII character range (\u00FF). You can also include the tab (\u0009), line feed (\u000A), and carriage return (\u000D) characters. Any of these characters are valid in a password. However, many tools, such as the AWS Management Console, might restrict the ability to type certain characters because they have special meaning within that tool.
cpNewPassword :: Lens' ChangePassword Text
cpNewPassword = lens _cpNewPassword (\ s a -> s{_cpNewPassword = a}) . _Sensitive

instance AWSRequest ChangePassword where
        type Rs ChangePassword = ChangePasswordResponse
        request = postQuery iam
        response = receiveNull ChangePasswordResponse'

instance Hashable ChangePassword where

instance NFData ChangePassword where

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
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ChangePasswordResponse' with the minimum fields required to make a request.
--
changePasswordResponse
    :: ChangePasswordResponse
changePasswordResponse = ChangePasswordResponse'


instance NFData ChangePasswordResponse where
