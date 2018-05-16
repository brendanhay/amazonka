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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
    , duuAuthenticationToken
    , duuUserId

    -- * Destructuring the Response
    , deleteUserResponse
    , DeleteUserResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkDocs.Types
import Network.AWS.WorkDocs.Types.Product

-- | /See:/ 'deleteUser' smart constructor.
data DeleteUser = DeleteUser'
  { _duuAuthenticationToken :: !(Maybe (Sensitive Text))
  , _duuUserId              :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'duuAuthenticationToken' - Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
--
-- * 'duuUserId' - The ID of the user.
deleteUser
    :: Text -- ^ 'duuUserId'
    -> DeleteUser
deleteUser pUserId_ =
  DeleteUser' {_duuAuthenticationToken = Nothing, _duuUserId = pUserId_}


-- | Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
duuAuthenticationToken :: Lens' DeleteUser (Maybe Text)
duuAuthenticationToken = lens _duuAuthenticationToken (\ s a -> s{_duuAuthenticationToken = a}) . mapping _Sensitive

-- | The ID of the user.
duuUserId :: Lens' DeleteUser Text
duuUserId = lens _duuUserId (\ s a -> s{_duuUserId = a})

instance AWSRequest DeleteUser where
        type Rs DeleteUser = DeleteUserResponse
        request = delete workDocs
        response = receiveNull DeleteUserResponse'

instance Hashable DeleteUser where

instance NFData DeleteUser where

instance ToHeaders DeleteUser where
        toHeaders DeleteUser'{..}
          = mconcat
              ["Authentication" =# _duuAuthenticationToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToPath DeleteUser where
        toPath DeleteUser'{..}
          = mconcat ["/api/v1/users/", toBS _duuUserId]

instance ToQuery DeleteUser where
        toQuery = const mempty

-- | /See:/ 'deleteUserResponse' smart constructor.
data DeleteUserResponse =
  DeleteUserResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteUserResponse' with the minimum fields required to make a request.
--
deleteUserResponse
    :: DeleteUserResponse
deleteUserResponse = DeleteUserResponse'


instance NFData DeleteUserResponse where
