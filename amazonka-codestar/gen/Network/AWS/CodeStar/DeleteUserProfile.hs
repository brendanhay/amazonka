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
-- Module      : Network.AWS.CodeStar.DeleteUserProfile
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a user profile in AWS CodeStar, including all personal preference data associated with that profile, such as display name and email address. It does not delete the history of that user, for example the history of commits made by that user.
--
--
module Network.AWS.CodeStar.DeleteUserProfile
    (
    -- * Creating a Request
      deleteUserProfile
    , DeleteUserProfile
    -- * Request Lenses
    , dUserARN

    -- * Destructuring the Response
    , deleteUserProfileResponse
    , DeleteUserProfileResponse
    -- * Response Lenses
    , delrsResponseStatus
    , delrsUserARN
    ) where

import Network.AWS.CodeStar.Types
import Network.AWS.CodeStar.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteUserProfile' smart constructor.
newtype DeleteUserProfile = DeleteUserProfile'
  { _dUserARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteUserProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dUserARN' - The Amazon Resource Name (ARN) of the user to delete from AWS CodeStar.
deleteUserProfile
    :: Text -- ^ 'dUserARN'
    -> DeleteUserProfile
deleteUserProfile pUserARN_ = DeleteUserProfile' {_dUserARN = pUserARN_}


-- | The Amazon Resource Name (ARN) of the user to delete from AWS CodeStar.
dUserARN :: Lens' DeleteUserProfile Text
dUserARN = lens _dUserARN (\ s a -> s{_dUserARN = a})

instance AWSRequest DeleteUserProfile where
        type Rs DeleteUserProfile = DeleteUserProfileResponse
        request = postJSON codeStar
        response
          = receiveJSON
              (\ s h x ->
                 DeleteUserProfileResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "userArn"))

instance Hashable DeleteUserProfile where

instance NFData DeleteUserProfile where

instance ToHeaders DeleteUserProfile where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeStar_20170419.DeleteUserProfile" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteUserProfile where
        toJSON DeleteUserProfile'{..}
          = object (catMaybes [Just ("userArn" .= _dUserARN)])

instance ToPath DeleteUserProfile where
        toPath = const "/"

instance ToQuery DeleteUserProfile where
        toQuery = const mempty

-- | /See:/ 'deleteUserProfileResponse' smart constructor.
data DeleteUserProfileResponse = DeleteUserProfileResponse'
  { _delrsResponseStatus :: !Int
  , _delrsUserARN        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteUserProfileResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delrsResponseStatus' - -- | The response status code.
--
-- * 'delrsUserARN' - The Amazon Resource Name (ARN) of the user deleted from AWS CodeStar.
deleteUserProfileResponse
    :: Int -- ^ 'delrsResponseStatus'
    -> Text -- ^ 'delrsUserARN'
    -> DeleteUserProfileResponse
deleteUserProfileResponse pResponseStatus_ pUserARN_ =
  DeleteUserProfileResponse'
    {_delrsResponseStatus = pResponseStatus_, _delrsUserARN = pUserARN_}


-- | -- | The response status code.
delrsResponseStatus :: Lens' DeleteUserProfileResponse Int
delrsResponseStatus = lens _delrsResponseStatus (\ s a -> s{_delrsResponseStatus = a})

-- | The Amazon Resource Name (ARN) of the user deleted from AWS CodeStar.
delrsUserARN :: Lens' DeleteUserProfileResponse Text
delrsUserARN = lens _delrsUserARN (\ s a -> s{_delrsUserARN = a})

instance NFData DeleteUserProfileResponse where
