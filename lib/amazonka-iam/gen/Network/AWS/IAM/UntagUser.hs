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
-- Module      : Network.AWS.IAM.UntagUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified tags from the user. For more information about tagging, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
module Network.AWS.IAM.UntagUser
  ( -- * Creating a Request
    untagUser,
    UntagUser,

    -- * Request Lenses
    uuUserName,
    uuTagKeys,

    -- * Destructuring the Response
    untagUserResponse,
    UntagUserResponse,
  )
where

import Network.AWS.IAM.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'untagUser' smart constructor.
data UntagUser = UntagUser'
  { _uuUserName :: !Text,
    _uuTagKeys :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UntagUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uuUserName' - The name of the IAM user from which you want to remove tags. This parameter accepts (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consist of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: =,.@-
--
-- * 'uuTagKeys' - A list of key names as a simple array of strings. The tags with matching keys are removed from the specified user.
untagUser ::
  -- | 'uuUserName'
  Text ->
  UntagUser
untagUser pUserName_ =
  UntagUser' {_uuUserName = pUserName_, _uuTagKeys = mempty}

-- | The name of the IAM user from which you want to remove tags. This parameter accepts (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consist of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: =,.@-
uuUserName :: Lens' UntagUser Text
uuUserName = lens _uuUserName (\s a -> s {_uuUserName = a})

-- | A list of key names as a simple array of strings. The tags with matching keys are removed from the specified user.
uuTagKeys :: Lens' UntagUser [Text]
uuTagKeys = lens _uuTagKeys (\s a -> s {_uuTagKeys = a}) . _Coerce

instance AWSRequest UntagUser where
  type Rs UntagUser = UntagUserResponse
  request = postQuery iam
  response = receiveNull UntagUserResponse'

instance Hashable UntagUser

instance NFData UntagUser

instance ToHeaders UntagUser where
  toHeaders = const mempty

instance ToPath UntagUser where
  toPath = const "/"

instance ToQuery UntagUser where
  toQuery UntagUser' {..} =
    mconcat
      [ "Action" =: ("UntagUser" :: ByteString),
        "Version" =: ("2010-05-08" :: ByteString),
        "UserName" =: _uuUserName,
        "TagKeys" =: toQueryList "member" _uuTagKeys
      ]

-- | /See:/ 'untagUserResponse' smart constructor.
data UntagUserResponse = UntagUserResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UntagUserResponse' with the minimum fields required to make a request.
untagUserResponse ::
  UntagUserResponse
untagUserResponse = UntagUserResponse'

instance NFData UntagUserResponse
