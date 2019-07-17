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
-- Module      : Network.AWS.IAM.DeleteRolePermissionsBoundary
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the permissions boundary for the specified IAM role.
--
--
-- /Important:/ Deleting the permissions boundary for a role might increase its permissions by allowing anyone who assumes the role to perform all the actions granted in its permissions policies.
--
module Network.AWS.IAM.DeleteRolePermissionsBoundary
    (
    -- * Creating a Request
      deleteRolePermissionsBoundary
    , DeleteRolePermissionsBoundary
    -- * Request Lenses
    , drpbRoleName

    -- * Destructuring the Response
    , deleteRolePermissionsBoundaryResponse
    , DeleteRolePermissionsBoundaryResponse
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteRolePermissionsBoundary' smart constructor.
newtype DeleteRolePermissionsBoundary = DeleteRolePermissionsBoundary'
  { _drpbRoleName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRolePermissionsBoundary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drpbRoleName' - The name (friendly name, not ARN) of the IAM role from which you want to remove the permissions boundary.
deleteRolePermissionsBoundary
    :: Text -- ^ 'drpbRoleName'
    -> DeleteRolePermissionsBoundary
deleteRolePermissionsBoundary pRoleName_ =
  DeleteRolePermissionsBoundary' {_drpbRoleName = pRoleName_}


-- | The name (friendly name, not ARN) of the IAM role from which you want to remove the permissions boundary.
drpbRoleName :: Lens' DeleteRolePermissionsBoundary Text
drpbRoleName = lens _drpbRoleName (\ s a -> s{_drpbRoleName = a})

instance AWSRequest DeleteRolePermissionsBoundary
         where
        type Rs DeleteRolePermissionsBoundary =
             DeleteRolePermissionsBoundaryResponse
        request = postQuery iam
        response
          = receiveNull DeleteRolePermissionsBoundaryResponse'

instance Hashable DeleteRolePermissionsBoundary where

instance NFData DeleteRolePermissionsBoundary where

instance ToHeaders DeleteRolePermissionsBoundary
         where
        toHeaders = const mempty

instance ToPath DeleteRolePermissionsBoundary where
        toPath = const "/"

instance ToQuery DeleteRolePermissionsBoundary where
        toQuery DeleteRolePermissionsBoundary'{..}
          = mconcat
              ["Action" =:
                 ("DeleteRolePermissionsBoundary" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "RoleName" =: _drpbRoleName]

-- | /See:/ 'deleteRolePermissionsBoundaryResponse' smart constructor.
data DeleteRolePermissionsBoundaryResponse =
  DeleteRolePermissionsBoundaryResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRolePermissionsBoundaryResponse' with the minimum fields required to make a request.
--
deleteRolePermissionsBoundaryResponse
    :: DeleteRolePermissionsBoundaryResponse
deleteRolePermissionsBoundaryResponse = DeleteRolePermissionsBoundaryResponse'


instance NFData DeleteRolePermissionsBoundaryResponse
         where
