{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UpdateUser
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Updates the name and\/or the path of the specified user.
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
    , uurqNewUserName
    , uurqNewPath
    , uurqUserName

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
-- * 'uurqNewUserName'
--
-- * 'uurqNewPath'
--
-- * 'uurqUserName'
data UpdateUser = UpdateUser'
    { _uurqNewUserName :: !(Maybe Text)
    , _uurqNewPath     :: !(Maybe Text)
    , _uurqUserName    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateUser' smart constructor.
updateUser :: Text -> UpdateUser
updateUser pUserName =
    UpdateUser'
    { _uurqNewUserName = Nothing
    , _uurqNewPath = Nothing
    , _uurqUserName = pUserName
    }

-- | New name for the user. Include this parameter only if you\'re changing
-- the user\'s name.
uurqNewUserName :: Lens' UpdateUser (Maybe Text)
uurqNewUserName = lens _uurqNewUserName (\ s a -> s{_uurqNewUserName = a});

-- | New path for the user. Include this parameter only if you\'re changing
-- the user\'s path.
uurqNewPath :: Lens' UpdateUser (Maybe Text)
uurqNewPath = lens _uurqNewPath (\ s a -> s{_uurqNewPath = a});

-- | Name of the user to update. If you\'re changing the name of the user,
-- this is the original user name.
uurqUserName :: Lens' UpdateUser Text
uurqUserName = lens _uurqUserName (\ s a -> s{_uurqUserName = a});

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
               "NewUserName" =: _uurqNewUserName,
               "NewPath" =: _uurqNewPath,
               "UserName" =: _uurqUserName]

-- | /See:/ 'updateUserResponse' smart constructor.
data UpdateUserResponse =
    UpdateUserResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateUserResponse' smart constructor.
updateUserResponse :: UpdateUserResponse
updateUserResponse = UpdateUserResponse'
