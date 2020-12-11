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
    apPolicyName,
    apPolicyARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about an attached policy.
--
-- An attached policy is a managed policy that has been attached to a user, group, or role. This data type is used as a response element in the 'ListAttachedGroupPolicies' , 'ListAttachedRolePolicies' , 'ListAttachedUserPolicies' , and 'GetAccountAuthorizationDetails' operations.
-- For more information about managed policies, refer to <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
--
-- /See:/ 'mkAttachedPolicy' smart constructor.
data AttachedPolicy = AttachedPolicy'
  { policyName ::
      Lude.Maybe Lude.Text,
    policyARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachedPolicy' with the minimum fields required to make a request.
--
-- * 'policyARN' - Undocumented field.
-- * 'policyName' - The friendly name of the attached policy.
mkAttachedPolicy ::
  AttachedPolicy
mkAttachedPolicy =
  AttachedPolicy'
    { policyName = Lude.Nothing,
      policyARN = Lude.Nothing
    }

-- | The friendly name of the attached policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apPolicyName :: Lens.Lens' AttachedPolicy (Lude.Maybe Lude.Text)
apPolicyName = Lens.lens (policyName :: AttachedPolicy -> Lude.Maybe Lude.Text) (\s a -> s {policyName = a} :: AttachedPolicy)
{-# DEPRECATED apPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'policyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apPolicyARN :: Lens.Lens' AttachedPolicy (Lude.Maybe Lude.Text)
apPolicyARN = Lens.lens (policyARN :: AttachedPolicy -> Lude.Maybe Lude.Text) (\s a -> s {policyARN = a} :: AttachedPolicy)
{-# DEPRECATED apPolicyARN "Use generic-lens or generic-optics with 'policyARN' instead." #-}

instance Lude.FromXML AttachedPolicy where
  parseXML x =
    AttachedPolicy'
      Lude.<$> (x Lude..@? "PolicyName") Lude.<*> (x Lude..@? "PolicyArn")
