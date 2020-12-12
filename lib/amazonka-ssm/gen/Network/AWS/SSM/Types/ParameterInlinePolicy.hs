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
    pipPolicyType,
    pipPolicyStatus,
    pipPolicyText,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | One or more policies assigned to a parameter.
--
-- /See:/ 'mkParameterInlinePolicy' smart constructor.
data ParameterInlinePolicy = ParameterInlinePolicy'
  { policyType ::
      Lude.Maybe Lude.Text,
    policyStatus :: Lude.Maybe Lude.Text,
    policyText :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ParameterInlinePolicy' with the minimum fields required to make a request.
--
-- * 'policyStatus' - The status of the policy. Policies report the following statuses: Pending (the policy has not been enforced or applied yet), Finished (the policy was applied), Failed (the policy was not applied), or InProgress (the policy is being applied now).
-- * 'policyText' - The JSON text of the policy.
-- * 'policyType' - The type of policy. Parameter Store supports the following policy types: Expiration, ExpirationNotification, and NoChangeNotification.
mkParameterInlinePolicy ::
  ParameterInlinePolicy
mkParameterInlinePolicy =
  ParameterInlinePolicy'
    { policyType = Lude.Nothing,
      policyStatus = Lude.Nothing,
      policyText = Lude.Nothing
    }

-- | The type of policy. Parameter Store supports the following policy types: Expiration, ExpirationNotification, and NoChangeNotification.
--
-- /Note:/ Consider using 'policyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pipPolicyType :: Lens.Lens' ParameterInlinePolicy (Lude.Maybe Lude.Text)
pipPolicyType = Lens.lens (policyType :: ParameterInlinePolicy -> Lude.Maybe Lude.Text) (\s a -> s {policyType = a} :: ParameterInlinePolicy)
{-# DEPRECATED pipPolicyType "Use generic-lens or generic-optics with 'policyType' instead." #-}

-- | The status of the policy. Policies report the following statuses: Pending (the policy has not been enforced or applied yet), Finished (the policy was applied), Failed (the policy was not applied), or InProgress (the policy is being applied now).
--
-- /Note:/ Consider using 'policyStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pipPolicyStatus :: Lens.Lens' ParameterInlinePolicy (Lude.Maybe Lude.Text)
pipPolicyStatus = Lens.lens (policyStatus :: ParameterInlinePolicy -> Lude.Maybe Lude.Text) (\s a -> s {policyStatus = a} :: ParameterInlinePolicy)
{-# DEPRECATED pipPolicyStatus "Use generic-lens or generic-optics with 'policyStatus' instead." #-}

-- | The JSON text of the policy.
--
-- /Note:/ Consider using 'policyText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pipPolicyText :: Lens.Lens' ParameterInlinePolicy (Lude.Maybe Lude.Text)
pipPolicyText = Lens.lens (policyText :: ParameterInlinePolicy -> Lude.Maybe Lude.Text) (\s a -> s {policyText = a} :: ParameterInlinePolicy)
{-# DEPRECATED pipPolicyText "Use generic-lens or generic-optics with 'policyText' instead." #-}

instance Lude.FromJSON ParameterInlinePolicy where
  parseJSON =
    Lude.withObject
      "ParameterInlinePolicy"
      ( \x ->
          ParameterInlinePolicy'
            Lude.<$> (x Lude..:? "PolicyType")
            Lude.<*> (x Lude..:? "PolicyStatus")
            Lude.<*> (x Lude..:? "PolicyText")
      )
