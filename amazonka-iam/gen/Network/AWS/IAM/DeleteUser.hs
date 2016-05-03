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
-- Module      : Network.AWS.IAM.DeleteUser
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified user. The user must not belong to any groups, have
-- any keys or signing certificates, or have any attached policies.
module Network.AWS.IAM.DeleteUser
    (
    -- * Creating a Request
      deleteUser
    , DeleteUser
    -- * Request Lenses
    , duUserName

    -- * Destructuring the Response
    , deleteUserResponse
    , DeleteUserResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteUser' smart constructor.
newtype DeleteUser = DeleteUser'
    { _duUserName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'duUserName'
deleteUser
    :: Text -- ^ 'duUserName'
    -> DeleteUser
deleteUser pUserName_ =
    DeleteUser'
    { _duUserName = pUserName_
    }

-- | The name of the user to delete.
duUserName :: Lens' DeleteUser Text
duUserName = lens _duUserName (\ s a -> s{_duUserName = a});

instance AWSRequest DeleteUser where
        type Rs DeleteUser = DeleteUserResponse
        request = postQuery iam
        response = receiveNull DeleteUserResponse'

instance Hashable DeleteUser

instance NFData DeleteUser

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

-- | Creates a value of 'DeleteUserResponse' with the minimum fields required to make a request.
--
deleteUserResponse
    :: DeleteUserResponse
deleteUserResponse = DeleteUserResponse'

instance NFData DeleteUserResponse
