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
-- Module      : Network.AWS.IAM.GetRolePolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the specified inline policy document that is embedded with the
-- specified role.
--
-- A role can also have managed policies attached to it. To retrieve a
-- managed policy document that is attached to a role, use GetPolicy to
-- determine the policy\'s default version, then use GetPolicyVersion to
-- retrieve the policy document.
--
-- For more information about policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /Using IAM/ guide.
--
-- For more information about roles, go to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/roles-toplevel.html Using Roles to Delegate Permissions and Federate Identities>.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetRolePolicy.html AWS API Reference> for GetRolePolicy.
module Network.AWS.IAM.GetRolePolicy
    (
    -- * Creating a Request
      getRolePolicy
    , GetRolePolicy
    -- * Request Lenses
    , grpRoleName
    , grpPolicyName

    -- * Destructuring the Response
    , getRolePolicyResponse
    , GetRolePolicyResponse
    -- * Response Lenses
    , grprsResponseStatus
    , grprsRoleName
    , grprsPolicyName
    , grprsPolicyDocument
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getRolePolicy' smart constructor.
data GetRolePolicy = GetRolePolicy'
    { _grpRoleName   :: !Text
    , _grpPolicyName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetRolePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grpRoleName'
--
-- * 'grpPolicyName'
getRolePolicy
    :: Text -- ^ 'grpRoleName'
    -> Text -- ^ 'grpPolicyName'
    -> GetRolePolicy
getRolePolicy pRoleName_ pPolicyName_ =
    GetRolePolicy'
    { _grpRoleName = pRoleName_
    , _grpPolicyName = pPolicyName_
    }

-- | The name of the role associated with the policy.
grpRoleName :: Lens' GetRolePolicy Text
grpRoleName = lens _grpRoleName (\ s a -> s{_grpRoleName = a});

-- | The name of the policy document to get.
grpPolicyName :: Lens' GetRolePolicy Text
grpPolicyName = lens _grpPolicyName (\ s a -> s{_grpPolicyName = a});

instance AWSRequest GetRolePolicy where
        type Rs GetRolePolicy = GetRolePolicyResponse
        request = postQuery iAM
        response
          = receiveXMLWrapper "GetRolePolicyResult"
              (\ s h x ->
                 GetRolePolicyResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "RoleName") <*>
                     (x .@ "PolicyName")
                     <*> (x .@ "PolicyDocument"))

instance ToHeaders GetRolePolicy where
        toHeaders = const mempty

instance ToPath GetRolePolicy where
        toPath = const "/"

instance ToQuery GetRolePolicy where
        toQuery GetRolePolicy'{..}
          = mconcat
              ["Action" =: ("GetRolePolicy" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "RoleName" =: _grpRoleName,
               "PolicyName" =: _grpPolicyName]

-- | Contains the response to a successful GetRolePolicy request.
--
-- /See:/ 'getRolePolicyResponse' smart constructor.
data GetRolePolicyResponse = GetRolePolicyResponse'
    { _grprsResponseStatus :: !Int
    , _grprsRoleName       :: !Text
    , _grprsPolicyName     :: !Text
    , _grprsPolicyDocument :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetRolePolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grprsResponseStatus'
--
-- * 'grprsRoleName'
--
-- * 'grprsPolicyName'
--
-- * 'grprsPolicyDocument'
getRolePolicyResponse
    :: Int -- ^ 'grprsResponseStatus'
    -> Text -- ^ 'grprsRoleName'
    -> Text -- ^ 'grprsPolicyName'
    -> Text -- ^ 'grprsPolicyDocument'
    -> GetRolePolicyResponse
getRolePolicyResponse pResponseStatus_ pRoleName_ pPolicyName_ pPolicyDocument_ =
    GetRolePolicyResponse'
    { _grprsResponseStatus = pResponseStatus_
    , _grprsRoleName = pRoleName_
    , _grprsPolicyName = pPolicyName_
    , _grprsPolicyDocument = pPolicyDocument_
    }

-- | The response status code.
grprsResponseStatus :: Lens' GetRolePolicyResponse Int
grprsResponseStatus = lens _grprsResponseStatus (\ s a -> s{_grprsResponseStatus = a});

-- | The role the policy is associated with.
grprsRoleName :: Lens' GetRolePolicyResponse Text
grprsRoleName = lens _grprsRoleName (\ s a -> s{_grprsRoleName = a});

-- | The name of the policy.
grprsPolicyName :: Lens' GetRolePolicyResponse Text
grprsPolicyName = lens _grprsPolicyName (\ s a -> s{_grprsPolicyName = a});

-- | The policy document.
grprsPolicyDocument :: Lens' GetRolePolicyResponse Text
grprsPolicyDocument = lens _grprsPolicyDocument (\ s a -> s{_grprsPolicyDocument = a});
