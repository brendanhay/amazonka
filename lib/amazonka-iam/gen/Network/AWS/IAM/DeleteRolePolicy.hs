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
-- Module      : Network.AWS.IAM.DeleteRolePolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified inline policy that is embedded in the specified IAM role.
--
--
-- A role can also have managed policies attached to it. To detach a managed policy from a role, use 'DetachRolePolicy' . For more information about policies, refer to <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
--
module Network.AWS.IAM.DeleteRolePolicy
    (
    -- * Creating a Request
      deleteRolePolicy
    , DeleteRolePolicy
    -- * Request Lenses
    , delRoleName
    , delPolicyName

    -- * Destructuring the Response
    , deleteRolePolicyResponse
    , DeleteRolePolicyResponse
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteRolePolicy' smart constructor.
data DeleteRolePolicy = DeleteRolePolicy'
  { _delRoleName   :: !Text
  , _delPolicyName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRolePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delRoleName' - The name (friendly name, not ARN) identifying the role that the policy is embedded in. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- * 'delPolicyName' - The name of the inline policy to delete from the specified IAM role. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
deleteRolePolicy
    :: Text -- ^ 'delRoleName'
    -> Text -- ^ 'delPolicyName'
    -> DeleteRolePolicy
deleteRolePolicy pRoleName_ pPolicyName_ =
  DeleteRolePolicy' {_delRoleName = pRoleName_, _delPolicyName = pPolicyName_}


-- | The name (friendly name, not ARN) identifying the role that the policy is embedded in. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
delRoleName :: Lens' DeleteRolePolicy Text
delRoleName = lens _delRoleName (\ s a -> s{_delRoleName = a})

-- | The name of the inline policy to delete from the specified IAM role. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
delPolicyName :: Lens' DeleteRolePolicy Text
delPolicyName = lens _delPolicyName (\ s a -> s{_delPolicyName = a})

instance AWSRequest DeleteRolePolicy where
        type Rs DeleteRolePolicy = DeleteRolePolicyResponse
        request = postQuery iam
        response = receiveNull DeleteRolePolicyResponse'

instance Hashable DeleteRolePolicy where

instance NFData DeleteRolePolicy where

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
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRolePolicyResponse' with the minimum fields required to make a request.
--
deleteRolePolicyResponse
    :: DeleteRolePolicyResponse
deleteRolePolicyResponse = DeleteRolePolicyResponse'


instance NFData DeleteRolePolicyResponse where
