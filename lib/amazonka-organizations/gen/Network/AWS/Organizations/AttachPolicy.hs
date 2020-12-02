{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.AttachPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a policy to a root, an organizational unit (OU), or an individual account. How the policy affects accounts depends on the type of policy. Refer to the /AWS Organizations User Guide/ for information about each policy type:
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_ai-opt-out.html AISERVICES_OPT_OUT_POLICY>
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_backup.html BACKUP_POLICY>
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_scp.html SERVICE_CONTROL_POLICY>
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html TAG_POLICY>
--
--
--
-- This operation can be called only from the organization's management account.
module Network.AWS.Organizations.AttachPolicy
  ( -- * Creating a Request
    attachPolicy,
    AttachPolicy,

    -- * Request Lenses
    apPolicyId,
    apTargetId,

    -- * Destructuring the Response
    attachPolicyResponse,
    AttachPolicyResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Organizations.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'attachPolicy' smart constructor.
data AttachPolicy = AttachPolicy'
  { _apPolicyId :: !Text,
    _apTargetId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AttachPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apPolicyId' - The unique identifier (ID) of the policy that you want to attach to the target. You can get the ID for the policy by calling the 'ListPolicies' operation. The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID string requires "p-" followed by from 8 to 128 lowercase or uppercase letters, digits, or the underscore character (_).
--
-- * 'apTargetId' - The unique identifier (ID) of the root, OU, or account that you want to attach the policy to. You can get the ID by calling the 'ListRoots' , 'ListOrganizationalUnitsForParent' , or 'ListAccounts' operations. The <http://wikipedia.org/wiki/regex regex pattern> for a target ID string requires one of the following:     * __Root__ - A string that begins with "r-" followed by from 4 to 32 lowercase letters or digits.     * __Account__ - A string that consists of exactly 12 digits.     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that the OU is in). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
attachPolicy ::
  -- | 'apPolicyId'
  Text ->
  -- | 'apTargetId'
  Text ->
  AttachPolicy
attachPolicy pPolicyId_ pTargetId_ =
  AttachPolicy' {_apPolicyId = pPolicyId_, _apTargetId = pTargetId_}

-- | The unique identifier (ID) of the policy that you want to attach to the target. You can get the ID for the policy by calling the 'ListPolicies' operation. The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID string requires "p-" followed by from 8 to 128 lowercase or uppercase letters, digits, or the underscore character (_).
apPolicyId :: Lens' AttachPolicy Text
apPolicyId = lens _apPolicyId (\s a -> s {_apPolicyId = a})

-- | The unique identifier (ID) of the root, OU, or account that you want to attach the policy to. You can get the ID by calling the 'ListRoots' , 'ListOrganizationalUnitsForParent' , or 'ListAccounts' operations. The <http://wikipedia.org/wiki/regex regex pattern> for a target ID string requires one of the following:     * __Root__ - A string that begins with "r-" followed by from 4 to 32 lowercase letters or digits.     * __Account__ - A string that consists of exactly 12 digits.     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that the OU is in). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
apTargetId :: Lens' AttachPolicy Text
apTargetId = lens _apTargetId (\s a -> s {_apTargetId = a})

instance AWSRequest AttachPolicy where
  type Rs AttachPolicy = AttachPolicyResponse
  request = postJSON organizations
  response = receiveNull AttachPolicyResponse'

instance Hashable AttachPolicy

instance NFData AttachPolicy

instance ToHeaders AttachPolicy where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSOrganizationsV20161128.AttachPolicy" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON AttachPolicy where
  toJSON AttachPolicy' {..} =
    object
      ( catMaybes
          [ Just ("PolicyId" .= _apPolicyId),
            Just ("TargetId" .= _apTargetId)
          ]
      )

instance ToPath AttachPolicy where
  toPath = const "/"

instance ToQuery AttachPolicy where
  toQuery = const mempty

-- | /See:/ 'attachPolicyResponse' smart constructor.
data AttachPolicyResponse = AttachPolicyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AttachPolicyResponse' with the minimum fields required to make a request.
attachPolicyResponse ::
  AttachPolicyResponse
attachPolicyResponse = AttachPolicyResponse'

instance NFData AttachPolicyResponse
