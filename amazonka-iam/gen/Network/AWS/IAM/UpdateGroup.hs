{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.IAM.UpdateGroup
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Updates the name and\/or the path of the specified group.
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
    , ugNewGroupName
    , ugNewPath
    , ugGroupName

    -- * Response
    , UpdateGroupResponse
    -- ** Response constructor
    , updateGroupResponse
    ) where

import Network.AWS.IAM.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ugNewGroupName'
--
-- * 'ugNewPath'
--
-- * 'ugGroupName'
data UpdateGroup = UpdateGroup'{_ugNewGroupName :: Maybe Text, _ugNewPath :: Maybe Text, _ugGroupName :: Text} deriving (Eq, Read, Show)

-- | 'UpdateGroup' smart constructor.
updateGroup :: Text -> UpdateGroup
updateGroup pGroupName = UpdateGroup'{_ugNewGroupName = Nothing, _ugNewPath = Nothing, _ugGroupName = pGroupName};

-- | New name for the group. Only include this if changing the group\'s name.
ugNewGroupName :: Lens' UpdateGroup (Maybe Text)
ugNewGroupName = lens _ugNewGroupName (\ s a -> s{_ugNewGroupName = a});

-- | New path for the group. Only include this if changing the group\'s path.
ugNewPath :: Lens' UpdateGroup (Maybe Text)
ugNewPath = lens _ugNewPath (\ s a -> s{_ugNewPath = a});

-- | Name of the group to update. If you\'re changing the name of the group,
-- this is the original name.
ugGroupName :: Lens' UpdateGroup Text
ugGroupName = lens _ugGroupName (\ s a -> s{_ugGroupName = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

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
               "NewGroupName" =: _ugNewGroupName,
               "NewPath" =: _ugNewPath, "GroupName" =: _ugGroupName]

-- | /See:/ 'updateGroupResponse' smart constructor.
data UpdateGroupResponse = UpdateGroupResponse' deriving (Eq, Read, Show)

-- | 'UpdateGroupResponse' smart constructor.
updateGroupResponse :: UpdateGroupResponse
updateGroupResponse = UpdateGroupResponse';
