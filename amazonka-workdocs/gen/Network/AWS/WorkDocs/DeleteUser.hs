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
-- Module      : Network.AWS.WorkDocs.DeleteUser
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified user from a Simple AD or Microsoft AD directory.
--
--
module Network.AWS.WorkDocs.DeleteUser
    (
    -- * Creating a Request
      deleteUser
    , DeleteUser
    -- * Request Lenses
    , dUserId

    -- * Destructuring the Response
    , deleteUserResponse
    , DeleteUserResponse
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.WorkDocs.Types
import           Network.AWS.WorkDocs.Types.Product

-- | /See:/ 'deleteUser' smart constructor.
newtype DeleteUser = DeleteUser'
    { _dUserId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dUserId' - The ID of the user.
deleteUser
    :: Text -- ^ 'dUserId'
    -> DeleteUser
deleteUser pUserId_ =
    DeleteUser'
    { _dUserId = pUserId_
    }

-- | The ID of the user.
dUserId :: Lens' DeleteUser Text
dUserId = lens _dUserId (\ s a -> s{_dUserId = a});

instance AWSRequest DeleteUser where
        type Rs DeleteUser = DeleteUserResponse
        request = delete workDocs
        response = receiveNull DeleteUserResponse'

instance Hashable DeleteUser

instance NFData DeleteUser

instance ToHeaders DeleteUser where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteUser where
        toPath DeleteUser'{..}
          = mconcat ["/api/v1/users/", toBS _dUserId]

instance ToQuery DeleteUser where
        toQuery = const mempty

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
