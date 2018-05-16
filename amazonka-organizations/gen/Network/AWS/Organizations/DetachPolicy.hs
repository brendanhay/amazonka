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
-- Module      : Network.AWS.Organizations.DetachPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches a policy from a target root, organizational unit, or account. If the policy being detached is a service control policy (SCP), the changes to permissions for IAM users and roles in affected accounts are immediate.
--
--
-- __Note:__ Every root, OU, and account must have at least one SCP attached. If you want to replace the default @FullAWSAccess@ policy with one that limits the permissions that can be delegated, then you must attach the replacement policy before you can remove the default one. This is the authorization strategy of <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_about-scps.html#orgs_policies_whitelist whitelisting> . If you instead attach a second SCP and leave the @FullAWSAccess@ SCP still attached, and specify @"Effect": "Deny"@ in the second SCP to override the @"Effect": "Allow"@ in the @FullAWSAccess@ policy (or any other attached SCP), then you are using the authorization strategy of <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_about-scps.html#orgs_policies_blacklist blacklisting> .
--
-- This operation can be called only from the organization's master account.
--
module Network.AWS.Organizations.DetachPolicy
    (
    -- * Creating a Request
      detachPolicy
    , DetachPolicy
    -- * Request Lenses
    , detPolicyId
    , detTargetId

    -- * Destructuring the Response
    , detachPolicyResponse
    , DetachPolicyResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Organizations.Types
import Network.AWS.Organizations.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'detachPolicy' smart constructor.
data DetachPolicy = DetachPolicy'
  { _detPolicyId :: !Text
  , _detTargetId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetachPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'detPolicyId' - The unique identifier (ID) of the policy you want to detach. You can get the ID from the 'ListPolicies' or 'ListPoliciesForTarget' operations. The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID string requires "p-" followed by from 8 to 128 lower-case letters or digits.
--
-- * 'detTargetId' - The unique identifier (ID) of the root, OU, or account from which you want to detach the policy. You can get the ID from the 'ListRoots' , 'ListOrganizationalUnitsForParent' , or 'ListAccounts' operations. The <http://wikipedia.org/wiki/regex regex pattern> for a target ID string requires one of the following:     * Root: a string that begins with "r-" followed by from 4 to 32 lower-case letters or digits.     * Account: a string that consists of exactly 12 digits.     * Organizational unit (OU): a string that begins with "ou-" followed by from 4 to 32 lower-case letters or digits (the ID of the root that the OU is in) followed by a second "-" dash and from 8 to 32 additional lower-case letters or digits.
detachPolicy
    :: Text -- ^ 'detPolicyId'
    -> Text -- ^ 'detTargetId'
    -> DetachPolicy
detachPolicy pPolicyId_ pTargetId_ =
  DetachPolicy' {_detPolicyId = pPolicyId_, _detTargetId = pTargetId_}


-- | The unique identifier (ID) of the policy you want to detach. You can get the ID from the 'ListPolicies' or 'ListPoliciesForTarget' operations. The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID string requires "p-" followed by from 8 to 128 lower-case letters or digits.
detPolicyId :: Lens' DetachPolicy Text
detPolicyId = lens _detPolicyId (\ s a -> s{_detPolicyId = a})

-- | The unique identifier (ID) of the root, OU, or account from which you want to detach the policy. You can get the ID from the 'ListRoots' , 'ListOrganizationalUnitsForParent' , or 'ListAccounts' operations. The <http://wikipedia.org/wiki/regex regex pattern> for a target ID string requires one of the following:     * Root: a string that begins with "r-" followed by from 4 to 32 lower-case letters or digits.     * Account: a string that consists of exactly 12 digits.     * Organizational unit (OU): a string that begins with "ou-" followed by from 4 to 32 lower-case letters or digits (the ID of the root that the OU is in) followed by a second "-" dash and from 8 to 32 additional lower-case letters or digits.
detTargetId :: Lens' DetachPolicy Text
detTargetId = lens _detTargetId (\ s a -> s{_detTargetId = a})

instance AWSRequest DetachPolicy where
        type Rs DetachPolicy = DetachPolicyResponse
        request = postJSON organizations
        response = receiveNull DetachPolicyResponse'

instance Hashable DetachPolicy where

instance NFData DetachPolicy where

instance ToHeaders DetachPolicy where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSOrganizationsV20161128.DetachPolicy" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DetachPolicy where
        toJSON DetachPolicy'{..}
          = object
              (catMaybes
                 [Just ("PolicyId" .= _detPolicyId),
                  Just ("TargetId" .= _detTargetId)])

instance ToPath DetachPolicy where
        toPath = const "/"

instance ToQuery DetachPolicy where
        toQuery = const mempty

-- | /See:/ 'detachPolicyResponse' smart constructor.
data DetachPolicyResponse =
  DetachPolicyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetachPolicyResponse' with the minimum fields required to make a request.
--
detachPolicyResponse
    :: DetachPolicyResponse
detachPolicyResponse = DetachPolicyResponse'


instance NFData DetachPolicyResponse where
