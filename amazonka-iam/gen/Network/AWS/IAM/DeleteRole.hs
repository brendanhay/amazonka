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
-- Module      : Network.AWS.IAM.DeleteRole
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified role. The role must not have any policies attached. For more information about roles, go to <http://docs.aws.amazon.com/IAM/latest/UserGuide/WorkingWithRoles.html Working with Roles> .
--
--
-- /Important:/ Make sure that you do not have any Amazon EC2 instances running with the role you are about to delete. Deleting a role or instance profile that is associated with a running instance will break any applications running on the instance.
--
module Network.AWS.IAM.DeleteRole
    (
    -- * Creating a Request
      deleteRole
    , DeleteRole
    -- * Request Lenses
    , drRoleName

    -- * Destructuring the Response
    , deleteRoleResponse
    , DeleteRoleResponse
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteRole' smart constructor.
newtype DeleteRole = DeleteRole'
  { _drRoleName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRole' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drRoleName' - The name of the role to delete. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
deleteRole
    :: Text -- ^ 'drRoleName'
    -> DeleteRole
deleteRole pRoleName_ = DeleteRole' {_drRoleName = pRoleName_}


-- | The name of the role to delete. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
drRoleName :: Lens' DeleteRole Text
drRoleName = lens _drRoleName (\ s a -> s{_drRoleName = a})

instance AWSRequest DeleteRole where
        type Rs DeleteRole = DeleteRoleResponse
        request = postQuery iam
        response = receiveNull DeleteRoleResponse'

instance Hashable DeleteRole where

instance NFData DeleteRole where

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
data DeleteRoleResponse =
  DeleteRoleResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRoleResponse' with the minimum fields required to make a request.
--
deleteRoleResponse
    :: DeleteRoleResponse
deleteRoleResponse = DeleteRoleResponse'


instance NFData DeleteRoleResponse where
