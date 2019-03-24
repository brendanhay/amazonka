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
-- Module      : Network.AWS.Connect.DeleteUser
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a user account from Amazon Connect.
--
--
module Network.AWS.Connect.DeleteUser
    (
    -- * Creating a Request
      deleteUser
    , DeleteUser
    -- * Request Lenses
    , dInstanceId
    , dUserId

    -- * Destructuring the Response
    , deleteUserResponse
    , DeleteUserResponse
    ) where

import Network.AWS.Connect.Types
import Network.AWS.Connect.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteUser' smart constructor.
data DeleteUser = DeleteUser'
  { _dInstanceId :: !Text
  , _dUserId     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dInstanceId' - The identifier for your Amazon Connect instance. To find the ID of your instance, open the AWS console and select Amazon Connect. Select the alias of the instance in the Instance alias column. The instance ID is displayed in the Overview section of your instance settings. For example, the instance ID is the set of characters at the end of the instance ARN, after instance/, such as 10a4c4eb-f57e-4d4c-b602-bf39176ced07.
--
-- * 'dUserId' - The unique identifier of the user to delete.
deleteUser
    :: Text -- ^ 'dInstanceId'
    -> Text -- ^ 'dUserId'
    -> DeleteUser
deleteUser pInstanceId_ pUserId_ =
  DeleteUser' {_dInstanceId = pInstanceId_, _dUserId = pUserId_}


-- | The identifier for your Amazon Connect instance. To find the ID of your instance, open the AWS console and select Amazon Connect. Select the alias of the instance in the Instance alias column. The instance ID is displayed in the Overview section of your instance settings. For example, the instance ID is the set of characters at the end of the instance ARN, after instance/, such as 10a4c4eb-f57e-4d4c-b602-bf39176ced07.
dInstanceId :: Lens' DeleteUser Text
dInstanceId = lens _dInstanceId (\ s a -> s{_dInstanceId = a})

-- | The unique identifier of the user to delete.
dUserId :: Lens' DeleteUser Text
dUserId = lens _dUserId (\ s a -> s{_dUserId = a})

instance AWSRequest DeleteUser where
        type Rs DeleteUser = DeleteUserResponse
        request = delete connect
        response = receiveNull DeleteUserResponse'

instance Hashable DeleteUser where

instance NFData DeleteUser where

instance ToHeaders DeleteUser where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteUser where
        toPath DeleteUser'{..}
          = mconcat
              ["/users/", toBS _dInstanceId, "/", toBS _dUserId]

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
