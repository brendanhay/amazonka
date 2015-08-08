{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteRolePolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified inline policy that is embedded in the specified
-- role.
--
-- A role can also have managed policies attached to it. To detach a
-- managed policy from a role, use DetachRolePolicy. For more information
-- about policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /Using IAM/ guide.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteRolePolicy.html AWS API Reference> for DeleteRolePolicy.
module Network.AWS.IAM.DeleteRolePolicy
    (
    -- * Creating a Request
      DeleteRolePolicy
    , deleteRolePolicy
    -- * Request Lenses
    , delRoleName
    , delPolicyName

    -- * Destructuring the Response
    , DeleteRolePolicyResponse
    , deleteRolePolicyResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteRolePolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delRoleName'
--
-- * 'delPolicyName'
data DeleteRolePolicy = DeleteRolePolicy'
    { _delRoleName   :: !Text
    , _delPolicyName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteRolePolicy' smart constructor.
deleteRolePolicy :: Text -> Text -> DeleteRolePolicy
deleteRolePolicy pRoleName_ pPolicyName_ =
    DeleteRolePolicy'
    { _delRoleName = pRoleName_
    , _delPolicyName = pPolicyName_
    }

-- | The name (friendly name, not ARN) identifying the role that the policy
-- is embedded in.
delRoleName :: Lens' DeleteRolePolicy Text
delRoleName = lens _delRoleName (\ s a -> s{_delRoleName = a});

-- | The name identifying the policy document to delete.
delPolicyName :: Lens' DeleteRolePolicy Text
delPolicyName = lens _delPolicyName (\ s a -> s{_delPolicyName = a});

instance AWSRequest DeleteRolePolicy where
        type Sv DeleteRolePolicy = IAM
        type Rs DeleteRolePolicy = DeleteRolePolicyResponse
        request = postQuery
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
               "RoleName" =: _delRoleName,
               "PolicyName" =: _delPolicyName]

-- | /See:/ 'deleteRolePolicyResponse' smart constructor.
data DeleteRolePolicyResponse =
    DeleteRolePolicyResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteRolePolicyResponse' smart constructor.
deleteRolePolicyResponse :: DeleteRolePolicyResponse
deleteRolePolicyResponse = DeleteRolePolicyResponse'
