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
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteRolePolicy.html>
module Network.AWS.IAM.DeleteRolePolicy
    (
    -- * Request
      DeleteRolePolicy
    -- ** Request constructor
    , deleteRolePolicy
    -- ** Request lenses
    , delrqRoleName
    , delrqPolicyName

    -- * Response
    , DeleteRolePolicyResponse
    -- ** Response constructor
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
-- * 'delrqRoleName'
--
-- * 'delrqPolicyName'
data DeleteRolePolicy = DeleteRolePolicy'
    { _delrqRoleName   :: !Text
    , _delrqPolicyName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteRolePolicy' smart constructor.
deleteRolePolicy :: Text -> Text -> DeleteRolePolicy
deleteRolePolicy pRoleName pPolicyName =
    DeleteRolePolicy'
    { _delrqRoleName = pRoleName
    , _delrqPolicyName = pPolicyName
    }

-- | The name (friendly name, not ARN) identifying the role that the policy
-- is embedded in.
delrqRoleName :: Lens' DeleteRolePolicy Text
delrqRoleName = lens _delrqRoleName (\ s a -> s{_delrqRoleName = a});

-- | The name identifying the policy document to delete.
delrqPolicyName :: Lens' DeleteRolePolicy Text
delrqPolicyName = lens _delrqPolicyName (\ s a -> s{_delrqPolicyName = a});

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
               "RoleName" =: _delrqRoleName,
               "PolicyName" =: _delrqPolicyName]

-- | /See:/ 'deleteRolePolicyResponse' smart constructor.
data DeleteRolePolicyResponse =
    DeleteRolePolicyResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteRolePolicyResponse' smart constructor.
deleteRolePolicyResponse :: DeleteRolePolicyResponse
deleteRolePolicyResponse = DeleteRolePolicyResponse'
