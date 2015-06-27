{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.IAM.DeleteUser
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

-- | Deletes the specified user. The user must not belong to any groups, have
-- any keys or signing certificates, or have any attached policies.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteUser.html>
module Network.AWS.IAM.DeleteUser
    (
    -- * Request
      DeleteUser
    -- ** Request constructor
    , deleteUser
    -- ** Request lenses
    , duUserName

    -- * Response
    , DeleteUserResponse
    -- ** Response constructor
    , deleteUserResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteUser' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'duUserName'
newtype DeleteUser = DeleteUser'
    { _duUserName :: Text
    } deriving (Eq,Read,Show)

-- | 'DeleteUser' smart constructor.
deleteUser :: Text -> DeleteUser
deleteUser pUserName =
    DeleteUser'
    { _duUserName = pUserName
    }

-- | The name of the user to delete.
duUserName :: Lens' DeleteUser Text
duUserName = lens _duUserName (\ s a -> s{_duUserName = a});

instance AWSRequest DeleteUser where
        type Sv DeleteUser = IAM
        type Rs DeleteUser = DeleteUserResponse
        request = post
        response = receiveNull DeleteUserResponse'

instance ToHeaders DeleteUser where
        toHeaders = const mempty

instance ToPath DeleteUser where
        toPath = const "/"

instance ToQuery DeleteUser where
        toQuery DeleteUser'{..}
          = mconcat
              ["Action" =: ("DeleteUser" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "UserName" =: _duUserName]

-- | /See:/ 'deleteUserResponse' smart constructor.
data DeleteUserResponse =
    DeleteUserResponse'
    deriving (Eq,Read,Show)

-- | 'DeleteUserResponse' smart constructor.
deleteUserResponse :: DeleteUserResponse
deleteUserResponse = DeleteUserResponse'
