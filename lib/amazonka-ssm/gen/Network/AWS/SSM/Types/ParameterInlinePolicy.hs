{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ParameterInlinePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ParameterInlinePolicy
  ( ParameterInlinePolicy (..),

    -- * Smart constructor
    mkParameterInlinePolicy,

    -- * Lenses
    pipPolicyStatus,
    pipPolicyText,
    pipPolicyType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.String as Types

-- | One or more policies assigned to a parameter.
--
-- /See:/ 'mkParameterInlinePolicy' smart constructor.
data ParameterInlinePolicy = ParameterInlinePolicy'
  { -- | The status of the policy. Policies report the following statuses: Pending (the policy has not been enforced or applied yet), Finished (the policy was applied), Failed (the policy was not applied), or InProgress (the policy is being applied now).
    policyStatus :: Core.Maybe Types.String,
    -- | The JSON text of the policy.
    policyText :: Core.Maybe Types.String,
    -- | The type of policy. Parameter Store supports the following policy types: Expiration, ExpirationNotification, and NoChangeNotification.
    policyType :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ParameterInlinePolicy' value with any optional fields omitted.
mkParameterInlinePolicy ::
  ParameterInlinePolicy
mkParameterInlinePolicy =
  ParameterInlinePolicy'
    { policyStatus = Core.Nothing,
      policyText = Core.Nothing,
      policyType = Core.Nothing
    }

-- | The status of the policy. Policies report the following statuses: Pending (the policy has not been enforced or applied yet), Finished (the policy was applied), Failed (the policy was not applied), or InProgress (the policy is being applied now).
--
-- /Note:/ Consider using 'policyStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pipPolicyStatus :: Lens.Lens' ParameterInlinePolicy (Core.Maybe Types.String)
pipPolicyStatus = Lens.field @"policyStatus"
{-# DEPRECATED pipPolicyStatus "Use generic-lens or generic-optics with 'policyStatus' instead." #-}

-- | The JSON text of the policy.
--
-- /Note:/ Consider using 'policyText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pipPolicyText :: Lens.Lens' ParameterInlinePolicy (Core.Maybe Types.String)
pipPolicyText = Lens.field @"policyText"
{-# DEPRECATED pipPolicyText "Use generic-lens or generic-optics with 'policyText' instead." #-}

-- | The type of policy. Parameter Store supports the following policy types: Expiration, ExpirationNotification, and NoChangeNotification.
--
-- /Note:/ Consider using 'policyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pipPolicyType :: Lens.Lens' ParameterInlinePolicy (Core.Maybe Types.String)
pipPolicyType = Lens.field @"policyType"
{-# DEPRECATED pipPolicyType "Use generic-lens or generic-optics with 'policyType' instead." #-}

instance Core.FromJSON ParameterInlinePolicy where
  parseJSON =
    Core.withObject "ParameterInlinePolicy" Core.$
      \x ->
        ParameterInlinePolicy'
          Core.<$> (x Core..:? "PolicyStatus")
          Core.<*> (x Core..:? "PolicyText")
          Core.<*> (x Core..:? "PolicyType")
