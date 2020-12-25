{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.AttachedPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.AttachedPolicy
  ( AttachedPolicy (..),

    -- * Smart constructor
    mkAttachedPolicy,

    -- * Lenses
    apPolicyArn,
    apPolicyName,
  )
where

import qualified Network.AWS.IAM.Types.PolicyArn as Types
import qualified Network.AWS.IAM.Types.PolicyName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about an attached policy.
--
-- An attached policy is a managed policy that has been attached to a user, group, or role. This data type is used as a response element in the 'ListAttachedGroupPolicies' , 'ListAttachedRolePolicies' , 'ListAttachedUserPolicies' , and 'GetAccountAuthorizationDetails' operations.
-- For more information about managed policies, refer to <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
--
-- /See:/ 'mkAttachedPolicy' smart constructor.
data AttachedPolicy = AttachedPolicy'
  { policyArn :: Core.Maybe Types.PolicyArn,
    -- | The friendly name of the attached policy.
    policyName :: Core.Maybe Types.PolicyName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachedPolicy' value with any optional fields omitted.
mkAttachedPolicy ::
  AttachedPolicy
mkAttachedPolicy =
  AttachedPolicy'
    { policyArn = Core.Nothing,
      policyName = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'policyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apPolicyArn :: Lens.Lens' AttachedPolicy (Core.Maybe Types.PolicyArn)
apPolicyArn = Lens.field @"policyArn"
{-# DEPRECATED apPolicyArn "Use generic-lens or generic-optics with 'policyArn' instead." #-}

-- | The friendly name of the attached policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apPolicyName :: Lens.Lens' AttachedPolicy (Core.Maybe Types.PolicyName)
apPolicyName = Lens.field @"policyName"
{-# DEPRECATED apPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

instance Core.FromXML AttachedPolicy where
  parseXML x =
    AttachedPolicy'
      Core.<$> (x Core..@? "PolicyArn") Core.<*> (x Core..@? "PolicyName")
