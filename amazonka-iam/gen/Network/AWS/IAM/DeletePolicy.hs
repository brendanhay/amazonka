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
-- Module      : Network.AWS.IAM.DeletePolicy
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified managed policy.
--
-- Before you can delete a managed policy, you must detach the policy from
-- all users, groups, and roles that it is attached to, and you must delete
-- all of the policy\'s versions. The following steps describe the process
-- for deleting a managed policy:
--
-- 1.  Detach the policy from all users, groups, and roles that the policy
--     is attached to, using the < DetachUserPolicy>, < DetachGroupPolicy>,
--     or < DetachRolePolicy> APIs. To list all the users, groups, and
--     roles that a policy is attached to, use < ListEntitiesForPolicy>.
-- 2.  Delete all versions of the policy using < DeletePolicyVersion>. To
--     list the policy\'s versions, use < ListPolicyVersions>. You cannot
--     use < DeletePolicyVersion> to delete the version that is marked as
--     the default version. You delete the policy\'s default version in the
--     next step of the process.
-- 3.  Delete the policy (this automatically deletes the policy\'s default
--     version) using this API.
--
-- For information about managed policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /IAM User Guide/.
module Network.AWS.IAM.DeletePolicy
    (
    -- * Creating a Request
      deletePolicy
    , DeletePolicy
    -- * Request Lenses
    , dpPolicyARN

    -- * Destructuring the Response
    , deletePolicyResponse
    , DeletePolicyResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deletePolicy' smart constructor.
newtype DeletePolicy = DeletePolicy'
    { _dpPolicyARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeletePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpPolicyARN'
deletePolicy
    :: Text -- ^ 'dpPolicyARN'
    -> DeletePolicy
deletePolicy pPolicyARN_ =
    DeletePolicy'
    { _dpPolicyARN = pPolicyARN_
    }

-- | Undocumented member.
dpPolicyARN :: Lens' DeletePolicy Text
dpPolicyARN = lens _dpPolicyARN (\ s a -> s{_dpPolicyARN = a});

instance AWSRequest DeletePolicy where
        type Rs DeletePolicy = DeletePolicyResponse
        request = postQuery iam
        response = receiveNull DeletePolicyResponse'

instance Hashable DeletePolicy

instance NFData DeletePolicy

instance ToHeaders DeletePolicy where
        toHeaders = const mempty

instance ToPath DeletePolicy where
        toPath = const "/"

instance ToQuery DeletePolicy where
        toQuery DeletePolicy'{..}
          = mconcat
              ["Action" =: ("DeletePolicy" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "PolicyArn" =: _dpPolicyARN]

-- | /See:/ 'deletePolicyResponse' smart constructor.
data DeletePolicyResponse =
    DeletePolicyResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeletePolicyResponse' with the minimum fields required to make a request.
--
deletePolicyResponse
    :: DeletePolicyResponse
deletePolicyResponse = DeletePolicyResponse'
