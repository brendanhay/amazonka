{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.IAM.DeleteRole
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

-- | Deletes the specified role. The role must not have any policies
-- attached. For more information about roles, go to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/WorkingWithRoles.html Working with Roles>.
--
-- Make sure you do not have any Amazon EC2 instances running with the role
-- you are about to delete. Deleting a role or instance profile that is
-- associated with a running instance will break any applications running
-- on the instance.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteRole.html>
module Network.AWS.IAM.DeleteRole
    (
    -- * Request
      DeleteRole
    -- ** Request constructor
    , deleteRole
    -- ** Request lenses
    , drRoleName

    -- * Response
    , DeleteRoleResponse
    -- ** Response constructor
    , deleteRoleResponse
    ) where

import Network.AWS.IAM.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteRole' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drRoleName'
newtype DeleteRole = DeleteRole'{_drRoleName :: Text} deriving (Eq, Read, Show)

-- | 'DeleteRole' smart constructor.
deleteRole :: Text -> DeleteRole
deleteRole pRoleName = DeleteRole'{_drRoleName = pRoleName};

-- | The name of the role to delete.
drRoleName :: Lens' DeleteRole Text
drRoleName = lens _drRoleName (\ s a -> s{_drRoleName = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest DeleteRole where
        type Sv DeleteRole = IAM
        type Rs DeleteRole = DeleteRoleResponse
        request = post
        response = receiveNull DeleteRoleResponse'

instance ToHeaders DeleteRole where
        toHeaders = const mempty

instance ToPath DeleteRole where
        toPath = const "/"

instance ToQuery DeleteRole where
        toQuery DeleteRole'{..}
          = mconcat
              ["Action" =: ("DeleteRole" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "RoleName" =: _drRoleName]

-- | /See:/ 'deleteRoleResponse' smart constructor.
data DeleteRoleResponse = DeleteRoleResponse' deriving (Eq, Read, Show)

-- | 'DeleteRoleResponse' smart constructor.
deleteRoleResponse :: DeleteRoleResponse
deleteRoleResponse = DeleteRoleResponse';
