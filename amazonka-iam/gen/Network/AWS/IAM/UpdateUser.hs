{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UpdateUser
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Updates the name and\/or the path of the specified user.
--
-- You should understand the implications of changing a user\'s path or
-- name. For more information, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_WorkingWithGroupsAndUsers.html Renaming Users and Groups>
-- in the /Using IAM/ guide.
--
-- To change a user name the requester must have appropriate permissions on
-- both the source object and the target object. For example, to change Bob
-- to Robert, the entity making the request must have permission on Bob and
-- Robert, or must have permission on all (*). For more information about
-- permissions, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/PermissionsAndPolicies.html Permissions and Policies>.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_UpdateUser.html>
module Network.AWS.IAM.UpdateUser
    (
    -- * Request
      UpdateUser
    -- ** Request constructor
    , updateUser
    -- ** Request lenses
    , uuNewUserName
    , uuNewPath
    , uuUserName

    -- * Response
    , UpdateUserResponse
    -- ** Response constructor
    , updateUserResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'updateUser' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uuNewUserName'
--
-- * 'uuNewPath'
--
-- * 'uuUserName'
data UpdateUser = UpdateUser'
    { _uuNewUserName :: !(Maybe Text)
    , _uuNewPath     :: !(Maybe Text)
    , _uuUserName    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateUser' smart constructor.
updateUser :: Text -> UpdateUser
updateUser pUserName =
    UpdateUser'
    { _uuNewUserName = Nothing
    , _uuNewPath = Nothing
    , _uuUserName = pUserName
    }

-- | New name for the user. Include this parameter only if you\'re changing
-- the user\'s name.
uuNewUserName :: Lens' UpdateUser (Maybe Text)
uuNewUserName = lens _uuNewUserName (\ s a -> s{_uuNewUserName = a});

-- | New path for the user. Include this parameter only if you\'re changing
-- the user\'s path.
uuNewPath :: Lens' UpdateUser (Maybe Text)
uuNewPath = lens _uuNewPath (\ s a -> s{_uuNewPath = a});

-- | Name of the user to update. If you\'re changing the name of the user,
-- this is the original user name.
uuUserName :: Lens' UpdateUser Text
uuUserName = lens _uuUserName (\ s a -> s{_uuUserName = a});

instance AWSRequest UpdateUser where
        type Sv UpdateUser = IAM
        type Rs UpdateUser = UpdateUserResponse
        request = post
        response = receiveNull UpdateUserResponse'

instance ToHeaders UpdateUser where
        toHeaders = const mempty

instance ToPath UpdateUser where
        toPath = const "/"

instance ToQuery UpdateUser where
        toQuery UpdateUser'{..}
          = mconcat
              ["Action" =: ("UpdateUser" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "NewUserName" =: _uuNewUserName,
               "NewPath" =: _uuNewPath, "UserName" =: _uuUserName]

-- | /See:/ 'updateUserResponse' smart constructor.
data UpdateUserResponse =
    UpdateUserResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateUserResponse' smart constructor.
updateUserResponse :: UpdateUserResponse
updateUserResponse = UpdateUserResponse'
