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
-- Module      : Network.AWS.WorkMail.DeleteUser
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a user from Amazon WorkMail and all subsequent systems. The action can't be undone. The mailbox is kept as-is for a minimum of 30 days, without any means to restore it.
--
--
module Network.AWS.WorkMail.DeleteUser
    (
    -- * Creating a Request
      deleteUser
    , DeleteUser
    -- * Request Lenses
    , delOrganizationId
    , delUserId

    -- * Destructuring the Response
    , deleteUserResponse
    , DeleteUserResponse
    -- * Response Lenses
    , delrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkMail.Types
import Network.AWS.WorkMail.Types.Product

-- | /See:/ 'deleteUser' smart constructor.
data DeleteUser = DeleteUser'
  { _delOrganizationId :: !Text
  , _delUserId         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delOrganizationId' - The organization that contains the user.
--
-- * 'delUserId' - The identifier of the user to be deleted.
deleteUser
    :: Text -- ^ 'delOrganizationId'
    -> Text -- ^ 'delUserId'
    -> DeleteUser
deleteUser pOrganizationId_ pUserId_ =
  DeleteUser' {_delOrganizationId = pOrganizationId_, _delUserId = pUserId_}


-- | The organization that contains the user.
delOrganizationId :: Lens' DeleteUser Text
delOrganizationId = lens _delOrganizationId (\ s a -> s{_delOrganizationId = a})

-- | The identifier of the user to be deleted.
delUserId :: Lens' DeleteUser Text
delUserId = lens _delUserId (\ s a -> s{_delUserId = a})

instance AWSRequest DeleteUser where
        type Rs DeleteUser = DeleteUserResponse
        request = postJSON workMail
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteUserResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteUser where

instance NFData DeleteUser where

instance ToHeaders DeleteUser where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("WorkMailService.DeleteUser" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteUser where
        toJSON DeleteUser'{..}
          = object
              (catMaybes
                 [Just ("OrganizationId" .= _delOrganizationId),
                  Just ("UserId" .= _delUserId)])

instance ToPath DeleteUser where
        toPath = const "/"

instance ToQuery DeleteUser where
        toQuery = const mempty

-- | /See:/ 'deleteUserResponse' smart constructor.
newtype DeleteUserResponse = DeleteUserResponse'
  { _delrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteUserResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delrsResponseStatus' - -- | The response status code.
deleteUserResponse
    :: Int -- ^ 'delrsResponseStatus'
    -> DeleteUserResponse
deleteUserResponse pResponseStatus_ =
  DeleteUserResponse' {_delrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
delrsResponseStatus :: Lens' DeleteUserResponse Int
delrsResponseStatus = lens _delrsResponseStatus (\ s a -> s{_delrsResponseStatus = a})

instance NFData DeleteUserResponse where
