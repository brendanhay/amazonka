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
-- Module      : Network.AWS.IAM.UpdateUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the name and/or the path of the specified IAM user.
--
--
-- /Important:/ You should understand the implications of changing an IAM user's path or name. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_users_manage.html#id_users_renaming Renaming an IAM User> and <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_groups_manage_rename.html Renaming an IAM Group> in the /IAM User Guide/ .
module Network.AWS.IAM.UpdateUser
  ( -- * Creating a Request
    updateUser,
    UpdateUser,

    -- * Request Lenses
    updNewUserName,
    updNewPath,
    updUserName,

    -- * Destructuring the Response
    updateUserResponse,
    UpdateUserResponse,
  )
where

import Network.AWS.IAM.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateUser' smart constructor.
data UpdateUser = UpdateUser'
  { _updNewUserName :: !(Maybe Text),
    _updNewPath :: !(Maybe Text),
    _updUserName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'updNewUserName' - New name for the user. Include this parameter only if you're changing the user's name. IAM user, group, role, and policy names must be unique within the account. Names are not distinguished by case. For example, you cannot create resources named both "MyResource" and "myresource".
--
-- * 'updNewPath' - New path for the IAM user. Include this parameter only if you're changing the user's path. This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
--
-- * 'updUserName' - Name of the user to update. If you're changing the name of the user, this is the original user name. This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
updateUser ::
  -- | 'updUserName'
  Text ->
  UpdateUser
updateUser pUserName_ =
  UpdateUser'
    { _updNewUserName = Nothing,
      _updNewPath = Nothing,
      _updUserName = pUserName_
    }

-- | New name for the user. Include this parameter only if you're changing the user's name. IAM user, group, role, and policy names must be unique within the account. Names are not distinguished by case. For example, you cannot create resources named both "MyResource" and "myresource".
updNewUserName :: Lens' UpdateUser (Maybe Text)
updNewUserName = lens _updNewUserName (\s a -> s {_updNewUserName = a})

-- | New path for the IAM user. Include this parameter only if you're changing the user's path. This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
updNewPath :: Lens' UpdateUser (Maybe Text)
updNewPath = lens _updNewPath (\s a -> s {_updNewPath = a})

-- | Name of the user to update. If you're changing the name of the user, this is the original user name. This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
updUserName :: Lens' UpdateUser Text
updUserName = lens _updUserName (\s a -> s {_updUserName = a})

instance AWSRequest UpdateUser where
  type Rs UpdateUser = UpdateUserResponse
  request = postQuery iam
  response = receiveNull UpdateUserResponse'

instance Hashable UpdateUser

instance NFData UpdateUser

instance ToHeaders UpdateUser where
  toHeaders = const mempty

instance ToPath UpdateUser where
  toPath = const "/"

instance ToQuery UpdateUser where
  toQuery UpdateUser' {..} =
    mconcat
      [ "Action" =: ("UpdateUser" :: ByteString),
        "Version" =: ("2010-05-08" :: ByteString),
        "NewUserName" =: _updNewUserName,
        "NewPath" =: _updNewPath,
        "UserName" =: _updUserName
      ]

-- | /See:/ 'updateUserResponse' smart constructor.
data UpdateUserResponse = UpdateUserResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateUserResponse' with the minimum fields required to make a request.
updateUserResponse ::
  UpdateUserResponse
updateUserResponse = UpdateUserResponse'

instance NFData UpdateUserResponse
