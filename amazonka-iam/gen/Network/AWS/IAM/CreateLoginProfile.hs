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
-- Module      : Network.AWS.IAM.CreateLoginProfile
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a password for the specified user, giving the user the ability to access AWS services through the AWS Management Console. For more information about managing passwords, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_ManagingLogins.html Managing Passwords> in the /IAM User Guide/ .
--
--
module Network.AWS.IAM.CreateLoginProfile
    (
    -- * Creating a Request
      createLoginProfile
    , CreateLoginProfile
    -- * Request Lenses
    , clpPasswordResetRequired
    , clpUserName
    , clpPassword

    -- * Destructuring the Response
    , createLoginProfileResponse
    , CreateLoginProfileResponse
    -- * Response Lenses
    , clprsResponseStatus
    , clprsLoginProfile
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createLoginProfile' smart constructor.
data CreateLoginProfile = CreateLoginProfile'
  { _clpPasswordResetRequired :: !(Maybe Bool)
  , _clpUserName              :: !Text
  , _clpPassword              :: !(Sensitive Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateLoginProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clpPasswordResetRequired' - Specifies whether the user is required to set a new password on next sign-in.
--
-- * 'clpUserName' - The name of the IAM user to create a password for. The user must already exist. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- * 'clpPassword' - The new password for the user. The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of characters. That string can include almost any printable ASCII character from the space (\u0020) through the end of the ASCII character range (\u00FF). You can also include the tab (\u0009), line feed (\u000A), and carriage return (\u000D) characters. Any of these characters are valid in a password. However, many tools, such as the AWS Management Console, might restrict the ability to type certain characters because they have special meaning within that tool.
createLoginProfile
    :: Text -- ^ 'clpUserName'
    -> Text -- ^ 'clpPassword'
    -> CreateLoginProfile
createLoginProfile pUserName_ pPassword_ =
  CreateLoginProfile'
    { _clpPasswordResetRequired = Nothing
    , _clpUserName = pUserName_
    , _clpPassword = _Sensitive # pPassword_
    }


-- | Specifies whether the user is required to set a new password on next sign-in.
clpPasswordResetRequired :: Lens' CreateLoginProfile (Maybe Bool)
clpPasswordResetRequired = lens _clpPasswordResetRequired (\ s a -> s{_clpPasswordResetRequired = a})

-- | The name of the IAM user to create a password for. The user must already exist. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
clpUserName :: Lens' CreateLoginProfile Text
clpUserName = lens _clpUserName (\ s a -> s{_clpUserName = a})

-- | The new password for the user. The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of characters. That string can include almost any printable ASCII character from the space (\u0020) through the end of the ASCII character range (\u00FF). You can also include the tab (\u0009), line feed (\u000A), and carriage return (\u000D) characters. Any of these characters are valid in a password. However, many tools, such as the AWS Management Console, might restrict the ability to type certain characters because they have special meaning within that tool.
clpPassword :: Lens' CreateLoginProfile Text
clpPassword = lens _clpPassword (\ s a -> s{_clpPassword = a}) . _Sensitive

instance AWSRequest CreateLoginProfile where
        type Rs CreateLoginProfile =
             CreateLoginProfileResponse
        request = postQuery iam
        response
          = receiveXMLWrapper "CreateLoginProfileResult"
              (\ s h x ->
                 CreateLoginProfileResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "LoginProfile"))

instance Hashable CreateLoginProfile where

instance NFData CreateLoginProfile where

instance ToHeaders CreateLoginProfile where
        toHeaders = const mempty

instance ToPath CreateLoginProfile where
        toPath = const "/"

instance ToQuery CreateLoginProfile where
        toQuery CreateLoginProfile'{..}
          = mconcat
              ["Action" =: ("CreateLoginProfile" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "PasswordResetRequired" =: _clpPasswordResetRequired,
               "UserName" =: _clpUserName,
               "Password" =: _clpPassword]

-- | Contains the response to a successful 'CreateLoginProfile' request.
--
--
--
-- /See:/ 'createLoginProfileResponse' smart constructor.
data CreateLoginProfileResponse = CreateLoginProfileResponse'
  { _clprsResponseStatus :: !Int
  , _clprsLoginProfile   :: !LoginProfile
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateLoginProfileResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clprsResponseStatus' - -- | The response status code.
--
-- * 'clprsLoginProfile' - A structure containing the user name and password create date.
createLoginProfileResponse
    :: Int -- ^ 'clprsResponseStatus'
    -> LoginProfile -- ^ 'clprsLoginProfile'
    -> CreateLoginProfileResponse
createLoginProfileResponse pResponseStatus_ pLoginProfile_ =
  CreateLoginProfileResponse'
    { _clprsResponseStatus = pResponseStatus_
    , _clprsLoginProfile = pLoginProfile_
    }


-- | -- | The response status code.
clprsResponseStatus :: Lens' CreateLoginProfileResponse Int
clprsResponseStatus = lens _clprsResponseStatus (\ s a -> s{_clprsResponseStatus = a})

-- | A structure containing the user name and password create date.
clprsLoginProfile :: Lens' CreateLoginProfileResponse LoginProfile
clprsLoginProfile = lens _clprsLoginProfile (\ s a -> s{_clprsLoginProfile = a})

instance NFData CreateLoginProfileResponse where
