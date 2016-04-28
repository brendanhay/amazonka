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
-- Module      : Network.AWS.IAM.UpdateGroup
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the name and\/or the path of the specified group.
--
-- You should understand the implications of changing a group\'s path or
-- name. For more information, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_WorkingWithGroupsAndUsers.html Renaming Users and Groups>
-- in the /IAM User Guide/.
--
-- To change a group name the requester must have appropriate permissions
-- on both the source object and the target object. For example, to change
-- Managers to MGRs, the entity making the request must have permission on
-- Managers and MGRs, or must have permission on all (*). For more
-- information about permissions, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/PermissionsAndPolicies.html Permissions and Policies>.
module Network.AWS.IAM.UpdateGroup
    (
    -- * Creating a Request
      updateGroup
    , UpdateGroup
    -- * Request Lenses
    , ugNewGroupName
    , ugNewPath
    , ugGroupName

    -- * Destructuring the Response
    , updateGroupResponse
    , UpdateGroupResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'updateGroup' smart constructor.
data UpdateGroup = UpdateGroup'
    { _ugNewGroupName :: !(Maybe Text)
    , _ugNewPath      :: !(Maybe Text)
    , _ugGroupName    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugNewGroupName'
--
-- * 'ugNewPath'
--
-- * 'ugGroupName'
updateGroup
    :: Text -- ^ 'ugGroupName'
    -> UpdateGroup
updateGroup pGroupName_ =
    UpdateGroup'
    { _ugNewGroupName = Nothing
    , _ugNewPath = Nothing
    , _ugGroupName = pGroupName_
    }

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

instance AWSRequest UpdateGroup where
        type Rs UpdateGroup = UpdateGroupResponse
        request = postQuery iam
        response = receiveNull UpdateGroupResponse'

instance Hashable UpdateGroup

instance NFData UpdateGroup

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
data UpdateGroupResponse =
    UpdateGroupResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateGroupResponse' with the minimum fields required to make a request.
--
updateGroupResponse
    :: UpdateGroupResponse
updateGroupResponse = UpdateGroupResponse'
