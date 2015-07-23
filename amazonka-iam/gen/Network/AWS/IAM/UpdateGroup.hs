{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UpdateGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Updates the name and\/or the path of the specified group.
--
-- You should understand the implications of changing a group\'s path or
-- name. For more information, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_WorkingWithGroupsAndUsers.html Renaming Users and Groups>
-- in the /Using IAM/ guide.
--
-- To change a group name the requester must have appropriate permissions
-- on both the source object and the target object. For example, to change
-- Managers to MGRs, the entity making the request must have permission on
-- Managers and MGRs, or must have permission on all (*). For more
-- information about permissions, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/PermissionsAndPolicies.html Permissions and Policies>.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_UpdateGroup.html>
module Network.AWS.IAM.UpdateGroup
    (
    -- * Request
      UpdateGroup
    -- ** Request constructor
    , updateGroup
    -- ** Request lenses
    , ugrqNewGroupName
    , ugrqNewPath
    , ugrqGroupName

    -- * Response
    , UpdateGroupResponse
    -- ** Response constructor
    , updateGroupResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'updateGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ugrqNewGroupName'
--
-- * 'ugrqNewPath'
--
-- * 'ugrqGroupName'
data UpdateGroup = UpdateGroup'
    { _ugrqNewGroupName :: !(Maybe Text)
    , _ugrqNewPath      :: !(Maybe Text)
    , _ugrqGroupName    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateGroup' smart constructor.
updateGroup :: Text -> UpdateGroup
updateGroup pGroupName_ =
    UpdateGroup'
    { _ugrqNewGroupName = Nothing
    , _ugrqNewPath = Nothing
    , _ugrqGroupName = pGroupName_
    }

-- | New name for the group. Only include this if changing the group\'s name.
ugrqNewGroupName :: Lens' UpdateGroup (Maybe Text)
ugrqNewGroupName = lens _ugrqNewGroupName (\ s a -> s{_ugrqNewGroupName = a});

-- | New path for the group. Only include this if changing the group\'s path.
ugrqNewPath :: Lens' UpdateGroup (Maybe Text)
ugrqNewPath = lens _ugrqNewPath (\ s a -> s{_ugrqNewPath = a});

-- | Name of the group to update. If you\'re changing the name of the group,
-- this is the original name.
ugrqGroupName :: Lens' UpdateGroup Text
ugrqGroupName = lens _ugrqGroupName (\ s a -> s{_ugrqGroupName = a});

instance AWSRequest UpdateGroup where
        type Sv UpdateGroup = IAM
        type Rs UpdateGroup = UpdateGroupResponse
        request = post
        response = receiveNull UpdateGroupResponse'

instance ToHeaders UpdateGroup where
        toHeaders = const mempty

instance ToPath UpdateGroup where
        toPath = const "/"

instance ToQuery UpdateGroup where
        toQuery UpdateGroup'{..}
          = mconcat
              ["Action" =: ("UpdateGroup" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "NewGroupName" =: _ugrqNewGroupName,
               "NewPath" =: _ugrqNewPath,
               "GroupName" =: _ugrqGroupName]

-- | /See:/ 'updateGroupResponse' smart constructor.
data UpdateGroupResponse =
    UpdateGroupResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateGroupResponse' smart constructor.
updateGroupResponse :: UpdateGroupResponse
updateGroupResponse = UpdateGroupResponse'
