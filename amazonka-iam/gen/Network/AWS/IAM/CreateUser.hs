{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.IAM.CreateUser
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates a new user for your AWS account.
--
-- For information about limitations on the number of users you can create,
-- see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html Limitations on IAM Entities>
-- in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateUser.html>
module Network.AWS.IAM.CreateUser
    (
    -- * Request
      CreateUser
    -- ** Request constructor
    , createUser
    -- ** Request lenses
    , cuPath
    , cuUserName

    -- * Response
    , CreateUserResponse
    -- ** Response constructor
    , createUserResponse
    -- ** Response lenses
    , curUser
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.IAM.Types

-- | /See:/ 'createUser' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cuPath'
--
-- * 'cuUserName'
data CreateUser = CreateUser'{_cuPath :: Maybe Text, _cuUserName :: Text} deriving (Eq, Read, Show)

-- | 'CreateUser' smart constructor.
createUser :: Text -> CreateUser
createUser pUserName = CreateUser'{_cuPath = Nothing, _cuUserName = pUserName};

-- | The path for the user name. For more information about paths, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
--
-- This parameter is optional. If it is not included, it defaults to a
-- slash (\/).
cuPath :: Lens' CreateUser (Maybe Text)
cuPath = lens _cuPath (\ s a -> s{_cuPath = a});

-- | The name of the user to create.
cuUserName :: Lens' CreateUser Text
cuUserName = lens _cuUserName (\ s a -> s{_cuUserName = a});

instance AWSRequest CreateUser where
        type Sv CreateUser = IAM
        type Rs CreateUser = CreateUserResponse
        request = post
        response
          = receiveXMLWrapper "CreateUserResult"
              (\ s h x -> CreateUserResponse' <$> x .@? "User")

instance ToHeaders CreateUser where
        toHeaders = const mempty

instance ToPath CreateUser where
        toPath = const "/"

instance ToQuery CreateUser where
        toQuery CreateUser'{..}
          = mconcat
              ["Action" =: ("CreateUser" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "Path" =: _cuPath, "UserName" =: _cuUserName]

-- | /See:/ 'createUserResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'curUser'
newtype CreateUserResponse = CreateUserResponse'{_curUser :: Maybe User} deriving (Eq, Read, Show)

-- | 'CreateUserResponse' smart constructor.
createUserResponse :: CreateUserResponse
createUserResponse = CreateUserResponse'{_curUser = Nothing};

-- | Information about the user.
curUser :: Lens' CreateUserResponse (Maybe User)
curUser = lens _curUser (\ s a -> s{_curUser = a});
