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
-- Module      : Network.AWS.Organizations.AttachPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a policy to a root, an organizational unit, or an individual account. How the policy affects accounts depends on the type of policy:
--
--
--     * __Service control policy (SCP)__ - An SCP specifies what permissions can be delegated to users in affected member accounts. The scope of influence for a policy depends on what you attach the policy to:
--
--     * If you attach an SCP to a root, it affects all accounts in the organization.
--
--     * If you attach an SCP to an OU, it affects all accounts in that OU and in any child OUs.
--
--     * If you attach the policy directly to an account, then it affects only that account.
--
--
--
-- SCPs essentially are permission "filters". When you attach one SCP to a higher level root or OU, and you also attach a different SCP to a child OU or to an account, the child policy can further restrict only the permissions that pass through the parent filter and are available to the child. An SCP that is attached to a child cannot grant a permission that is not already granted by the parent. For example, imagine that the parent SCP allows permissions A, B, C, D, and E. The child SCP allows C, D, E, F, and G. The result is that the accounts affected by the child SCP are allowed to use only C, D, and E. They cannot use A or B because they were filtered out by the child OU. They also cannot use F and G because they were filtered out by the parent OU. They cannot be granted back by the child SCP; child SCPs can only filter the permissions they receive from the parent SCP.
--
-- AWS Organizations attaches a default SCP named @"FullAWSAccess@ to every root, OU, and account. This default SCP allows all services and actions, enabling any new child OU or account to inherit the permissions of the parent root or OU. If you detach the default policy, you must replace it with a policy that specifies the permissions that you want to allow in that OU or account.
--
-- For more information about how Organizations policies permissions work, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_scp.html Using Service Control Policies> in the /AWS Organizations User Guide/ .
--
--
--
-- This operation can be called only from the organization's master account.
--
module Network.AWS.Organizations.AttachPolicy
    (
    -- * Creating a Request
      attachPolicy
    , AttachPolicy
    -- * Request Lenses
    , apPolicyId
    , apTargetId

    -- * Destructuring the Response
    , attachPolicyResponse
    , AttachPolicyResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Organizations.Types
import Network.AWS.Organizations.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'attachPolicy' smart constructor.
data AttachPolicy = AttachPolicy'
  { _apPolicyId :: !Text
  , _apTargetId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttachPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apPolicyId' - The unique identifier (ID) of the policy that you want to attach to the target. You can get the ID for the policy by calling the 'ListPolicies' operation. The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID string requires "p-" followed by from 8 to 128 lower-case letters or digits.
--
-- * 'apTargetId' - The unique identifier (ID) of the root, OU, or account that you want to attach the policy to. You can get the ID by calling the 'ListRoots' , 'ListOrganizationalUnitsForParent' , or 'ListAccounts' operations. The <http://wikipedia.org/wiki/regex regex pattern> for a target ID string requires one of the following:     * Root: a string that begins with "r-" followed by from 4 to 32 lower-case letters or digits.     * Account: a string that consists of exactly 12 digits.     * Organizational unit (OU): a string that begins with "ou-" followed by from 4 to 32 lower-case letters or digits (the ID of the root that the OU is in) followed by a second "-" dash and from 8 to 32 additional lower-case letters or digits.
attachPolicy
    :: Text -- ^ 'apPolicyId'
    -> Text -- ^ 'apTargetId'
    -> AttachPolicy
attachPolicy pPolicyId_ pTargetId_ =
  AttachPolicy' {_apPolicyId = pPolicyId_, _apTargetId = pTargetId_}


-- | The unique identifier (ID) of the policy that you want to attach to the target. You can get the ID for the policy by calling the 'ListPolicies' operation. The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID string requires "p-" followed by from 8 to 128 lower-case letters or digits.
apPolicyId :: Lens' AttachPolicy Text
apPolicyId = lens _apPolicyId (\ s a -> s{_apPolicyId = a})

-- | The unique identifier (ID) of the root, OU, or account that you want to attach the policy to. You can get the ID by calling the 'ListRoots' , 'ListOrganizationalUnitsForParent' , or 'ListAccounts' operations. The <http://wikipedia.org/wiki/regex regex pattern> for a target ID string requires one of the following:     * Root: a string that begins with "r-" followed by from 4 to 32 lower-case letters or digits.     * Account: a string that consists of exactly 12 digits.     * Organizational unit (OU): a string that begins with "ou-" followed by from 4 to 32 lower-case letters or digits (the ID of the root that the OU is in) followed by a second "-" dash and from 8 to 32 additional lower-case letters or digits.
apTargetId :: Lens' AttachPolicy Text
apTargetId = lens _apTargetId (\ s a -> s{_apTargetId = a})

instance AWSRequest AttachPolicy where
        type Rs AttachPolicy = AttachPolicyResponse
        request = postJSON organizations
        response = receiveNull AttachPolicyResponse'

instance Hashable AttachPolicy where

instance NFData AttachPolicy where

instance ToHeaders AttachPolicy where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSOrganizationsV20161128.AttachPolicy" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AttachPolicy where
        toJSON AttachPolicy'{..}
          = object
              (catMaybes
                 [Just ("PolicyId" .= _apPolicyId),
                  Just ("TargetId" .= _apTargetId)])

instance ToPath AttachPolicy where
        toPath = const "/"

instance ToQuery AttachPolicy where
        toQuery = const mempty

-- | /See:/ 'attachPolicyResponse' smart constructor.
data AttachPolicyResponse =
  AttachPolicyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttachPolicyResponse' with the minimum fields required to make a request.
--
attachPolicyResponse
    :: AttachPolicyResponse
attachPolicyResponse = AttachPolicyResponse'


instance NFData AttachPolicyResponse where
