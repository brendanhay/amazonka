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
      GetRolePolicy
    , getRolePolicy
    -- * Request Lenses
    , grpRoleName
    , grpPolicyName

    -- * Destructuring the Response
    , GetRolePolicyResponse
    , getRolePolicyResponse
    -- * Response Lenses
    , grprsStatus
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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'grpRoleName'
--
-- * 'grpPolicyName'
data GetRolePolicy = GetRolePolicy'
    { _grpRoleName   :: !Text
    , _grpPolicyName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetRolePolicy' smart constructor.
getRolePolicy :: Text -> Text -> GetRolePolicy
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
        type Sv GetRolePolicy = IAM
        type Rs GetRolePolicy = GetRolePolicyResponse
        request = postQuery
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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'grprsStatus'
--
-- * 'grprsRoleName'
--
-- * 'grprsPolicyName'
--
-- * 'grprsPolicyDocument'
data GetRolePolicyResponse = GetRolePolicyResponse'
    { _grprsStatus         :: !Int
    , _grprsRoleName       :: !Text
    , _grprsPolicyName     :: !Text
    , _grprsPolicyDocument :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetRolePolicyResponse' smart constructor.
getRolePolicyResponse :: Int -> Text -> Text -> Text -> GetRolePolicyResponse
getRolePolicyResponse pStatus_ pRoleName_ pPolicyName_ pPolicyDocument_ =
    GetRolePolicyResponse'
    { _grprsStatus = pStatus_
    , _grprsRoleName = pRoleName_
    , _grprsPolicyName = pPolicyName_
    , _grprsPolicyDocument = pPolicyDocument_
    }

-- | Undocumented member.
grprsStatus :: Lens' GetRolePolicyResponse Int
grprsStatus = lens _grprsStatus (\ s a -> s{_grprsStatus = a});

-- | The role the policy is associated with.
grprsRoleName :: Lens' GetRolePolicyResponse Text
grprsRoleName = lens _grprsRoleName (\ s a -> s{_grprsRoleName = a});

-- | The name of the policy.
grprsPolicyName :: Lens' GetRolePolicyResponse Text
grprsPolicyName = lens _grprsPolicyName (\ s a -> s{_grprsPolicyName = a});

-- | The policy document.
grprsPolicyDocument :: Lens' GetRolePolicyResponse Text
grprsPolicyDocument = lens _grprsPolicyDocument (\ s a -> s{_grprsPolicyDocument = a});
