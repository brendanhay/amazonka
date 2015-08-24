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
-- Module      : Network.AWS.IAM.PutRolePolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds (or updates) an inline policy document that is embedded in the
-- specified role.
--
-- When you embed an inline policy in a role, the inline policy is used as
-- the role\'s access (permissions) policy. The role\'s trust policy is
-- created at the same time as the role, using CreateRole. You can update a
-- role\'s trust policy using UpdateAssumeRolePolicy. For more information
-- about roles, go to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/roles-toplevel.html Using Roles to Delegate Permissions and Federate Identities>.
--
-- A role can also have a managed policy attached to it. To attach a
-- managed policy to a role, use AttachRolePolicy. To create a new managed
-- policy, use CreatePolicy. For information about policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /Using IAM/ guide.
--
-- For information about limits on the number of inline policies that you
-- can embed with a role, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html Limitations on IAM Entities>
-- in the /Using IAM/ guide.
--
-- Because policy documents can be large, you should use POST rather than
-- GET when calling 'PutRolePolicy'. For general information about using
-- the Query API with IAM, go to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/IAM_UsingQueryAPI.html Making Query Requests>
-- in the /Using IAM/ guide.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_PutRolePolicy.html AWS API Reference> for PutRolePolicy.
module Network.AWS.IAM.PutRolePolicy
    (
    -- * Creating a Request
      putRolePolicy
    , PutRolePolicy
    -- * Request Lenses
    , prpRoleName
    , prpPolicyName
    , prpPolicyDocument

    -- * Destructuring the Response
    , putRolePolicyResponse
    , PutRolePolicyResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'putRolePolicy' smart constructor.
data PutRolePolicy = PutRolePolicy'
    { _prpRoleName       :: !Text
    , _prpPolicyName     :: !Text
    , _prpPolicyDocument :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutRolePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prpRoleName'
--
-- * 'prpPolicyName'
--
-- * 'prpPolicyDocument'
putRolePolicy
    :: Text -- ^ 'prpRoleName'
    -> Text -- ^ 'prpPolicyName'
    -> Text -- ^ 'prpPolicyDocument'
    -> PutRolePolicy
putRolePolicy pRoleName_ pPolicyName_ pPolicyDocument_ =
    PutRolePolicy'
    { _prpRoleName = pRoleName_
    , _prpPolicyName = pPolicyName_
    , _prpPolicyDocument = pPolicyDocument_
    }

-- | The name of the role to associate the policy with.
prpRoleName :: Lens' PutRolePolicy Text
prpRoleName = lens _prpRoleName (\ s a -> s{_prpRoleName = a});

-- | The name of the policy document.
prpPolicyName :: Lens' PutRolePolicy Text
prpPolicyName = lens _prpPolicyName (\ s a -> s{_prpPolicyName = a});

-- | The policy document.
prpPolicyDocument :: Lens' PutRolePolicy Text
prpPolicyDocument = lens _prpPolicyDocument (\ s a -> s{_prpPolicyDocument = a});

instance AWSRequest PutRolePolicy where
        type Rs PutRolePolicy = PutRolePolicyResponse
        request = postQuery iAM
        response = receiveNull PutRolePolicyResponse'

instance ToHeaders PutRolePolicy where
        toHeaders = const mempty

instance ToPath PutRolePolicy where
        toPath = const "/"

instance ToQuery PutRolePolicy where
        toQuery PutRolePolicy'{..}
          = mconcat
              ["Action" =: ("PutRolePolicy" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "RoleName" =: _prpRoleName,
               "PolicyName" =: _prpPolicyName,
               "PolicyDocument" =: _prpPolicyDocument]

-- | /See:/ 'putRolePolicyResponse' smart constructor.
data PutRolePolicyResponse =
    PutRolePolicyResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutRolePolicyResponse' with the minimum fields required to make a request.
--
putRolePolicyResponse
    :: PutRolePolicyResponse
putRolePolicyResponse = PutRolePolicyResponse'
