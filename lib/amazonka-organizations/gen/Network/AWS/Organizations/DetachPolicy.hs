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
    dpfPolicyId,
    dpfTargetId,

    -- * Destructuring the response
    DetachPolicyResponse (..),
    mkDetachPolicyResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDetachPolicy' smart constructor.
data DetachPolicy = DetachPolicy'
  { -- | The unique identifier (ID) of the policy you want to detach. You can get the ID from the 'ListPolicies' or 'ListPoliciesForTarget' operations.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID string requires "p-" followed by from 8 to 128 lowercase or uppercase letters, digits, or the underscore character (_).
    policyId :: Types.PolicyId,
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
    targetId :: Types.TargetId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetachPolicy' value with any optional fields omitted.
mkDetachPolicy ::
  -- | 'policyId'
  Types.PolicyId ->
  -- | 'targetId'
  Types.TargetId ->
  DetachPolicy
mkDetachPolicy policyId targetId =
  DetachPolicy' {policyId, targetId}

-- | The unique identifier (ID) of the policy you want to detach. You can get the ID from the 'ListPolicies' or 'ListPoliciesForTarget' operations.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID string requires "p-" followed by from 8 to 128 lowercase or uppercase letters, digits, or the underscore character (_).
--
-- /Note:/ Consider using 'policyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpfPolicyId :: Lens.Lens' DetachPolicy Types.PolicyId
dpfPolicyId = Lens.field @"policyId"
{-# DEPRECATED dpfPolicyId "Use generic-lens or generic-optics with 'policyId' instead." #-}

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
dpfTargetId :: Lens.Lens' DetachPolicy Types.TargetId
dpfTargetId = Lens.field @"targetId"
{-# DEPRECATED dpfTargetId "Use generic-lens or generic-optics with 'targetId' instead." #-}

instance Core.FromJSON DetachPolicy where
  toJSON DetachPolicy {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("PolicyId" Core..= policyId),
            Core.Just ("TargetId" Core..= targetId)
          ]
      )

instance Core.AWSRequest DetachPolicy where
  type Rs DetachPolicy = DetachPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSOrganizationsV20161128.DetachPolicy")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull DetachPolicyResponse'

-- | /See:/ 'mkDetachPolicyResponse' smart constructor.
data DetachPolicyResponse = DetachPolicyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetachPolicyResponse' value with any optional fields omitted.
mkDetachPolicyResponse ::
  DetachPolicyResponse
mkDetachPolicyResponse = DetachPolicyResponse'
