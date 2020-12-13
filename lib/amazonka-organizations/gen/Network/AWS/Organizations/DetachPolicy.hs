{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.DetachPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches a policy from a target root, organizational unit (OU), or account.
--
-- /Important:/ If the policy being detached is a service control policy (SCP), the changes to permissions for AWS Identity and Access Management (IAM) users and roles in affected accounts are immediate.
-- Every root, OU, and account must have at least one SCP attached. If you want to replace the default @FullAWSAccess@ policy with an SCP that limits the permissions that can be delegated, you must attach the replacement SCP before you can remove the default SCP. This is the authorization strategy of an "<https://docs.aws.amazon.com/organizations/latest/userguide/SCP_strategies.html#orgs_policies_allowlist allow list> ". If you instead attach a second SCP and leave the @FullAWSAccess@ SCP still attached, and specify @"Effect": "Deny"@ in the second SCP to override the @"Effect": "Allow"@ in the @FullAWSAccess@ policy (or any other attached SCP), you're using the authorization strategy of a "<https://docs.aws.amazon.com/organizations/latest/userguide/SCP_strategies.html#orgs_policies_denylist deny list> ".
-- This operation can be called only from the organization's management account.
module Network.AWS.Organizations.DetachPolicy
  ( -- * Creating a request
    DetachPolicy (..),
    mkDetachPolicy,

    -- ** Request lenses
    dpfTargetId,
    dpfPolicyId,

    -- * Destructuring the response
    DetachPolicyResponse (..),
    mkDetachPolicyResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDetachPolicy' smart constructor.
data DetachPolicy = DetachPolicy'
  { -- | The unique identifier (ID) of the root, OU, or account that you want to detach the policy from. You can get the ID from the 'ListRoots' , 'ListOrganizationalUnitsForParent' , or 'ListAccounts' operations.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for a target ID string requires one of the following:
    --
    --     * __Root__ - A string that begins with "r-" followed by from 4 to 32 lowercase letters or digits.
    --
    --
    --     * __Account__ - A string that consists of exactly 12 digits.
    --
    --
    --     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that the OU is in). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
    targetId :: Lude.Text,
    -- | The unique identifier (ID) of the policy you want to detach. You can get the ID from the 'ListPolicies' or 'ListPoliciesForTarget' operations.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID string requires "p-" followed by from 8 to 128 lowercase or uppercase letters, digits, or the underscore character (_).
    policyId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetachPolicy' with the minimum fields required to make a request.
--
-- * 'targetId' - The unique identifier (ID) of the root, OU, or account that you want to detach the policy from. You can get the ID from the 'ListRoots' , 'ListOrganizationalUnitsForParent' , or 'ListAccounts' operations.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a target ID string requires one of the following:
--
--     * __Root__ - A string that begins with "r-" followed by from 4 to 32 lowercase letters or digits.
--
--
--     * __Account__ - A string that consists of exactly 12 digits.
--
--
--     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that the OU is in). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
--
--
-- * 'policyId' - The unique identifier (ID) of the policy you want to detach. You can get the ID from the 'ListPolicies' or 'ListPoliciesForTarget' operations.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID string requires "p-" followed by from 8 to 128 lowercase or uppercase letters, digits, or the underscore character (_).
mkDetachPolicy ::
  -- | 'targetId'
  Lude.Text ->
  -- | 'policyId'
  Lude.Text ->
  DetachPolicy
mkDetachPolicy pTargetId_ pPolicyId_ =
  DetachPolicy' {targetId = pTargetId_, policyId = pPolicyId_}

-- | The unique identifier (ID) of the root, OU, or account that you want to detach the policy from. You can get the ID from the 'ListRoots' , 'ListOrganizationalUnitsForParent' , or 'ListAccounts' operations.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a target ID string requires one of the following:
--
--     * __Root__ - A string that begins with "r-" followed by from 4 to 32 lowercase letters or digits.
--
--
--     * __Account__ - A string that consists of exactly 12 digits.
--
--
--     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that the OU is in). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
--
--
--
-- /Note:/ Consider using 'targetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpfTargetId :: Lens.Lens' DetachPolicy Lude.Text
dpfTargetId = Lens.lens (targetId :: DetachPolicy -> Lude.Text) (\s a -> s {targetId = a} :: DetachPolicy)
{-# DEPRECATED dpfTargetId "Use generic-lens or generic-optics with 'targetId' instead." #-}

-- | The unique identifier (ID) of the policy you want to detach. You can get the ID from the 'ListPolicies' or 'ListPoliciesForTarget' operations.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID string requires "p-" followed by from 8 to 128 lowercase or uppercase letters, digits, or the underscore character (_).
--
-- /Note:/ Consider using 'policyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpfPolicyId :: Lens.Lens' DetachPolicy Lude.Text
dpfPolicyId = Lens.lens (policyId :: DetachPolicy -> Lude.Text) (\s a -> s {policyId = a} :: DetachPolicy)
{-# DEPRECATED dpfPolicyId "Use generic-lens or generic-optics with 'policyId' instead." #-}

instance Lude.AWSRequest DetachPolicy where
  type Rs DetachPolicy = DetachPolicyResponse
  request = Req.postJSON organizationsService
  response = Res.receiveNull DetachPolicyResponse'

instance Lude.ToHeaders DetachPolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSOrganizationsV20161128.DetachPolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DetachPolicy where
  toJSON DetachPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("TargetId" Lude..= targetId),
            Lude.Just ("PolicyId" Lude..= policyId)
          ]
      )

instance Lude.ToPath DetachPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery DetachPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDetachPolicyResponse' smart constructor.
data DetachPolicyResponse = DetachPolicyResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetachPolicyResponse' with the minimum fields required to make a request.
mkDetachPolicyResponse ::
  DetachPolicyResponse
mkDetachPolicyResponse = DetachPolicyResponse'
