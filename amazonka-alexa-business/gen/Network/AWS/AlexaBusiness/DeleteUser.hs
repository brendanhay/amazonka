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
-- Module      : Network.AWS.AlexaBusiness.DeleteUser
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified user by user ARN and enrollment ARN.
--
--
module Network.AWS.AlexaBusiness.DeleteUser
    (
    -- * Creating a Request
      deleteUser
    , DeleteUser
    -- * Request Lenses
    , duUserARN
    , duEnrollmentId

    -- * Destructuring the Response
    , deleteUserResponse
    , DeleteUserResponse
    -- * Response Lenses
    , dursResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteUser' smart constructor.
data DeleteUser = DeleteUser'
  { _duUserARN      :: !(Maybe Text)
  , _duEnrollmentId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'duUserARN' - The ARN of the user to delete in the organization. Required.
--
-- * 'duEnrollmentId' - The ARN of the user's enrollment in the organization. Required.
deleteUser
    :: Text -- ^ 'duEnrollmentId'
    -> DeleteUser
deleteUser pEnrollmentId_ =
  DeleteUser' {_duUserARN = Nothing, _duEnrollmentId = pEnrollmentId_}


-- | The ARN of the user to delete in the organization. Required.
duUserARN :: Lens' DeleteUser (Maybe Text)
duUserARN = lens _duUserARN (\ s a -> s{_duUserARN = a})

-- | The ARN of the user's enrollment in the organization. Required.
duEnrollmentId :: Lens' DeleteUser Text
duEnrollmentId = lens _duEnrollmentId (\ s a -> s{_duEnrollmentId = a})

instance AWSRequest DeleteUser where
        type Rs DeleteUser = DeleteUserResponse
        request = postJSON alexaBusiness
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
                    ("AlexaForBusiness.DeleteUser" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteUser where
        toJSON DeleteUser'{..}
          = object
              (catMaybes
                 [("UserArn" .=) <$> _duUserARN,
                  Just ("EnrollmentId" .= _duEnrollmentId)])

instance ToPath DeleteUser where
        toPath = const "/"

instance ToQuery DeleteUser where
        toQuery = const mempty

-- | /See:/ 'deleteUserResponse' smart constructor.
newtype DeleteUserResponse = DeleteUserResponse'
  { _dursResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteUserResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dursResponseStatus' - -- | The response status code.
deleteUserResponse
    :: Int -- ^ 'dursResponseStatus'
    -> DeleteUserResponse
deleteUserResponse pResponseStatus_ =
  DeleteUserResponse' {_dursResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dursResponseStatus :: Lens' DeleteUserResponse Int
dursResponseStatus = lens _dursResponseStatus (\ s a -> s{_dursResponseStatus = a})

instance NFData DeleteUserResponse where
