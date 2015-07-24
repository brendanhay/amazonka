{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteUser
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified user. The user must not belong to any groups, have
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteUser' smart constructor.
deleteUser :: Text -> DeleteUser
deleteUser pUserName_ =
    DeleteUser'
    { _duUserName = pUserName_
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
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteUserResponse' smart constructor.
deleteUserResponse :: DeleteUserResponse
deleteUserResponse = DeleteUserResponse'
