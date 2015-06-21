{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.IAM.DeleteRolePolicy
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

-- | Deletes the specified inline policy that is embedded in the specified
-- role.
--
-- A role can also have managed policies attached to it. To detach a
-- managed policy from a role, use DetachRolePolicy. For more information
-- about policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteRolePolicy.html>
module Network.AWS.IAM.DeleteRolePolicy
    (
    -- * Request
      DeleteRolePolicy
    -- ** Request constructor
    , deleteRolePolicy
    -- ** Request lenses
    , dRoleName
    , dPolicyName

    -- * Response
    , DeleteRolePolicyResponse
    -- ** Response constructor
    , deleteRolePolicyResponse
    ) where

import Network.AWS.IAM.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteRolePolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dRoleName'
--
-- * 'dPolicyName'
data DeleteRolePolicy = DeleteRolePolicy'{_dRoleName :: Text, _dPolicyName :: Text} deriving (Eq, Read, Show)

-- | 'DeleteRolePolicy' smart constructor.
deleteRolePolicy :: Text -> Text -> DeleteRolePolicy
deleteRolePolicy pRoleName pPolicyName = DeleteRolePolicy'{_dRoleName = pRoleName, _dPolicyName = pPolicyName};

-- | The name (friendly name, not ARN) identifying the role that the policy
-- is embedded in.
dRoleName :: Lens' DeleteRolePolicy Text
dRoleName = lens _dRoleName (\ s a -> s{_dRoleName = a});

-- | The name identifying the policy document to delete.
dPolicyName :: Lens' DeleteRolePolicy Text
dPolicyName = lens _dPolicyName (\ s a -> s{_dPolicyName = a});

instance AWSRequest DeleteRolePolicy where
        type Sv DeleteRolePolicy = IAM
        type Rs DeleteRolePolicy = DeleteRolePolicyResponse
        request = post
        response = receiveNull DeleteRolePolicyResponse'

instance ToHeaders DeleteRolePolicy where
        toHeaders = const mempty

instance ToPath DeleteRolePolicy where
        toPath = const "/"

instance ToQuery DeleteRolePolicy where
        toQuery DeleteRolePolicy'{..}
          = mconcat
              ["Action" =: ("DeleteRolePolicy" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "RoleName" =: _dRoleName,
               "PolicyName" =: _dPolicyName]

-- | /See:/ 'deleteRolePolicyResponse' smart constructor.
data DeleteRolePolicyResponse = DeleteRolePolicyResponse' deriving (Eq, Read, Show)

-- | 'DeleteRolePolicyResponse' smart constructor.
deleteRolePolicyResponse :: DeleteRolePolicyResponse
deleteRolePolicyResponse = DeleteRolePolicyResponse';
