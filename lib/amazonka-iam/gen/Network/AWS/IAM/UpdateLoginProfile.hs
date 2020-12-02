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
-- Module      : Network.AWS.IAM.UpdateLoginProfile
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the password for the specified IAM user.
--
--
-- IAM users can change their own passwords by calling 'ChangePassword' . For more information about modifying passwords, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_ManagingLogins.html Managing Passwords> in the /IAM User Guide/ .
--
module Network.AWS.IAM.UpdateLoginProfile
    (
    -- * Creating a Request
      updateLoginProfile
    , UpdateLoginProfile
    -- * Request Lenses
    , ulpPassword
    , ulpPasswordResetRequired
    , ulpUserName

    -- * Destructuring the Response
    , updateLoginProfileResponse
    , UpdateLoginProfileResponse
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateLoginProfile' smart constructor.
data UpdateLoginProfile = UpdateLoginProfile'
  { _ulpPassword              :: !(Maybe (Sensitive Text))
  , _ulpPasswordResetRequired :: !(Maybe Bool)
  , _ulpUserName              :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateLoginProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ulpPassword' - The new password for the specified IAM user. The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:     * Any printable ASCII character ranging from the space character (\u0020) through the end of the ASCII character range     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through \u00FF)     * The special characters tab (\u0009), line feed (\u000A), and carriage return (\u000D) However, the format can be further restricted by the account administrator by setting a password policy on the AWS account. For more information, see 'UpdateAccountPasswordPolicy' .
--
-- * 'ulpPasswordResetRequired' - Allows this new password to be used only once by requiring the specified IAM user to set a new password on next sign-in.
--
-- * 'ulpUserName' - The name of the user whose password you want to update. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
updateLoginProfile
    :: Text -- ^ 'ulpUserName'
    -> UpdateLoginProfile
updateLoginProfile pUserName_ =
  UpdateLoginProfile'
    { _ulpPassword = Nothing
    , _ulpPasswordResetRequired = Nothing
    , _ulpUserName = pUserName_
    }


-- | The new password for the specified IAM user. The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:     * Any printable ASCII character ranging from the space character (\u0020) through the end of the ASCII character range     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through \u00FF)     * The special characters tab (\u0009), line feed (\u000A), and carriage return (\u000D) However, the format can be further restricted by the account administrator by setting a password policy on the AWS account. For more information, see 'UpdateAccountPasswordPolicy' .
ulpPassword :: Lens' UpdateLoginProfile (Maybe Text)
ulpPassword = lens _ulpPassword (\ s a -> s{_ulpPassword = a}) . mapping _Sensitive

-- | Allows this new password to be used only once by requiring the specified IAM user to set a new password on next sign-in.
ulpPasswordResetRequired :: Lens' UpdateLoginProfile (Maybe Bool)
ulpPasswordResetRequired = lens _ulpPasswordResetRequired (\ s a -> s{_ulpPasswordResetRequired = a})

-- | The name of the user whose password you want to update. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
ulpUserName :: Lens' UpdateLoginProfile Text
ulpUserName = lens _ulpUserName (\ s a -> s{_ulpUserName = a})

instance AWSRequest UpdateLoginProfile where
        type Rs UpdateLoginProfile =
             UpdateLoginProfileResponse
        request = postQuery iam
        response = receiveNull UpdateLoginProfileResponse'

instance Hashable UpdateLoginProfile where

instance NFData UpdateLoginProfile where

instance ToHeaders UpdateLoginProfile where
        toHeaders = const mempty

instance ToPath UpdateLoginProfile where
        toPath = const "/"

instance ToQuery UpdateLoginProfile where
        toQuery UpdateLoginProfile'{..}
          = mconcat
              ["Action" =: ("UpdateLoginProfile" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "Password" =: _ulpPassword,
               "PasswordResetRequired" =: _ulpPasswordResetRequired,
               "UserName" =: _ulpUserName]

-- | /See:/ 'updateLoginProfileResponse' smart constructor.
data UpdateLoginProfileResponse =
  UpdateLoginProfileResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateLoginProfileResponse' with the minimum fields required to make a request.
--
updateLoginProfileResponse
    :: UpdateLoginProfileResponse
updateLoginProfileResponse = UpdateLoginProfileResponse'


instance NFData UpdateLoginProfileResponse where
