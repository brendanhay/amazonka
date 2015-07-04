{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.IAM.GetRolePolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Retrieves the specified inline policy document that is embedded with the
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
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetRolePolicy.html>
module Network.AWS.IAM.GetRolePolicy
    (
    -- * Request
      GetRolePolicy
    -- ** Request constructor
    , getRolePolicy
    -- ** Request lenses
    , grpRoleName
    , grpPolicyName

    -- * Response
    , GetRolePolicyResponse
    -- ** Response constructor
    , getRolePolicyResponse
    -- ** Response lenses
    , grprStatus
    , grprRoleName
    , grprPolicyName
    , grprPolicyDocument
    ) where

import           Network.AWS.IAM.Types
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
getRolePolicy pRoleName pPolicyName =
    GetRolePolicy'
    { _grpRoleName = pRoleName
    , _grpPolicyName = pPolicyName
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
        request = post
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
-- * 'grprStatus'
--
-- * 'grprRoleName'
--
-- * 'grprPolicyName'
--
-- * 'grprPolicyDocument'
data GetRolePolicyResponse = GetRolePolicyResponse'
    { _grprStatus         :: !Int
    , _grprRoleName       :: !Text
    , _grprPolicyName     :: !Text
    , _grprPolicyDocument :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetRolePolicyResponse' smart constructor.
getRolePolicyResponse :: Int -> Text -> Text -> Text -> GetRolePolicyResponse
getRolePolicyResponse pStatus pRoleName pPolicyName pPolicyDocument =
    GetRolePolicyResponse'
    { _grprStatus = pStatus
    , _grprRoleName = pRoleName
    , _grprPolicyName = pPolicyName
    , _grprPolicyDocument = pPolicyDocument
    }

-- | FIXME: Undocumented member.
grprStatus :: Lens' GetRolePolicyResponse Int
grprStatus = lens _grprStatus (\ s a -> s{_grprStatus = a});

-- | The role the policy is associated with.
grprRoleName :: Lens' GetRolePolicyResponse Text
grprRoleName = lens _grprRoleName (\ s a -> s{_grprRoleName = a});

-- | The name of the policy.
grprPolicyName :: Lens' GetRolePolicyResponse Text
grprPolicyName = lens _grprPolicyName (\ s a -> s{_grprPolicyName = a});

-- | The policy document.
grprPolicyDocument :: Lens' GetRolePolicyResponse Text
grprPolicyDocument = lens _grprPolicyDocument (\ s a -> s{_grprPolicyDocument = a});
