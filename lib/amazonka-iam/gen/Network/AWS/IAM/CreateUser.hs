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
-- Module      : Network.AWS.IAM.CreateUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new IAM user for your AWS account.
--
--
-- The number and size of IAM resources in an AWS account are limited. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-quotas.html IAM and STS Quotas> in the /IAM User Guide/ .
module Network.AWS.IAM.CreateUser
  ( -- * Creating a Request
    createUser,
    CreateUser,

    -- * Request Lenses
    cuPath,
    cuPermissionsBoundary,
    cuTags,
    cuUserName,

    -- * Destructuring the Response
    createUserResponse,
    CreateUserResponse,

    -- * Response Lenses
    cursUser,
    cursResponseStatus,
  )
where

import Network.AWS.IAM.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createUser' smart constructor.
data CreateUser = CreateUser'
  { _cuPath :: !(Maybe Text),
    _cuPermissionsBoundary :: !(Maybe Text),
    _cuTags :: !(Maybe [Tag]),
    _cuUserName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cuPath' - The path for the user name. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ . This parameter is optional. If it is not included, it defaults to a slash (/). This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
--
-- * 'cuPermissionsBoundary' - The ARN of the policy that is used to set the permissions boundary for the user.
--
-- * 'cuTags' - A list of tags that you want to attach to the newly created user. Each tag consists of a key name and an associated value. For more information about tagging, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
--
-- * 'cuUserName' - The name of the user to create. IAM user, group, role, and policy names must be unique within the account. Names are not distinguished by case. For example, you cannot create resources named both "MyResource" and "myresource".
createUser ::
  -- | 'cuUserName'
  Text ->
  CreateUser
createUser pUserName_ =
  CreateUser'
    { _cuPath = Nothing,
      _cuPermissionsBoundary = Nothing,
      _cuTags = Nothing,
      _cuUserName = pUserName_
    }

-- | The path for the user name. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ . This parameter is optional. If it is not included, it defaults to a slash (/). This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
cuPath :: Lens' CreateUser (Maybe Text)
cuPath = lens _cuPath (\s a -> s {_cuPath = a})

-- | The ARN of the policy that is used to set the permissions boundary for the user.
cuPermissionsBoundary :: Lens' CreateUser (Maybe Text)
cuPermissionsBoundary = lens _cuPermissionsBoundary (\s a -> s {_cuPermissionsBoundary = a})

-- | A list of tags that you want to attach to the newly created user. Each tag consists of a key name and an associated value. For more information about tagging, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
cuTags :: Lens' CreateUser [Tag]
cuTags = lens _cuTags (\s a -> s {_cuTags = a}) . _Default . _Coerce

-- | The name of the user to create. IAM user, group, role, and policy names must be unique within the account. Names are not distinguished by case. For example, you cannot create resources named both "MyResource" and "myresource".
cuUserName :: Lens' CreateUser Text
cuUserName = lens _cuUserName (\s a -> s {_cuUserName = a})

instance AWSRequest CreateUser where
  type Rs CreateUser = CreateUserResponse
  request = postQuery iam
  response =
    receiveXMLWrapper
      "CreateUserResult"
      ( \s h x ->
          CreateUserResponse' <$> (x .@? "User") <*> (pure (fromEnum s))
      )

instance Hashable CreateUser

instance NFData CreateUser

instance ToHeaders CreateUser where
  toHeaders = const mempty

instance ToPath CreateUser where
  toPath = const "/"

instance ToQuery CreateUser where
  toQuery CreateUser' {..} =
    mconcat
      [ "Action" =: ("CreateUser" :: ByteString),
        "Version" =: ("2010-05-08" :: ByteString),
        "Path" =: _cuPath,
        "PermissionsBoundary" =: _cuPermissionsBoundary,
        "Tags" =: toQuery (toQueryList "member" <$> _cuTags),
        "UserName" =: _cuUserName
      ]

-- | Contains the response to a successful 'CreateUser' request.
--
--
--
-- /See:/ 'createUserResponse' smart constructor.
data CreateUserResponse = CreateUserResponse'
  { _cursUser ::
      !(Maybe User),
    _cursResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateUserResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cursUser' - A structure with details about the new IAM user.
--
-- * 'cursResponseStatus' - -- | The response status code.
createUserResponse ::
  -- | 'cursResponseStatus'
  Int ->
  CreateUserResponse
createUserResponse pResponseStatus_ =
  CreateUserResponse'
    { _cursUser = Nothing,
      _cursResponseStatus = pResponseStatus_
    }

-- | A structure with details about the new IAM user.
cursUser :: Lens' CreateUserResponse (Maybe User)
cursUser = lens _cursUser (\s a -> s {_cursUser = a})

-- | -- | The response status code.
cursResponseStatus :: Lens' CreateUserResponse Int
cursResponseStatus = lens _cursResponseStatus (\s a -> s {_cursResponseStatus = a})

instance NFData CreateUserResponse
