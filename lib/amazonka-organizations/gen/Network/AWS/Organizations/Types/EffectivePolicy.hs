{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.EffectivePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.EffectivePolicy
  ( EffectivePolicy (..),

    -- * Smart constructor
    mkEffectivePolicy,

    -- * Lenses
    epTargetId,
    epPolicyType,
    epLastUpdatedTimestamp,
    epPolicyContent,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types.EffectivePolicyType
import qualified Network.AWS.Prelude as Lude

-- | Contains rules to be applied to the affected accounts. The effective policy is the aggregation of any policies the account inherits, plus any policy directly attached to the account.
--
-- /See:/ 'mkEffectivePolicy' smart constructor.
data EffectivePolicy = EffectivePolicy'
  { -- | The account ID of the policy target.
    targetId :: Lude.Maybe Lude.Text,
    -- | The policy type.
    policyType :: Lude.Maybe EffectivePolicyType,
    -- | The time of the last update to this policy.
    lastUpdatedTimestamp :: Lude.Maybe Lude.Timestamp,
    -- | The text content of the policy.
    policyContent :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EffectivePolicy' with the minimum fields required to make a request.
--
-- * 'targetId' - The account ID of the policy target.
-- * 'policyType' - The policy type.
-- * 'lastUpdatedTimestamp' - The time of the last update to this policy.
-- * 'policyContent' - The text content of the policy.
mkEffectivePolicy ::
  EffectivePolicy
mkEffectivePolicy =
  EffectivePolicy'
    { targetId = Lude.Nothing,
      policyType = Lude.Nothing,
      lastUpdatedTimestamp = Lude.Nothing,
      policyContent = Lude.Nothing
    }

-- | The account ID of the policy target.
--
-- /Note:/ Consider using 'targetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epTargetId :: Lens.Lens' EffectivePolicy (Lude.Maybe Lude.Text)
epTargetId = Lens.lens (targetId :: EffectivePolicy -> Lude.Maybe Lude.Text) (\s a -> s {targetId = a} :: EffectivePolicy)
{-# DEPRECATED epTargetId "Use generic-lens or generic-optics with 'targetId' instead." #-}

-- | The policy type.
--
-- /Note:/ Consider using 'policyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epPolicyType :: Lens.Lens' EffectivePolicy (Lude.Maybe EffectivePolicyType)
epPolicyType = Lens.lens (policyType :: EffectivePolicy -> Lude.Maybe EffectivePolicyType) (\s a -> s {policyType = a} :: EffectivePolicy)
{-# DEPRECATED epPolicyType "Use generic-lens or generic-optics with 'policyType' instead." #-}

-- | The time of the last update to this policy.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epLastUpdatedTimestamp :: Lens.Lens' EffectivePolicy (Lude.Maybe Lude.Timestamp)
epLastUpdatedTimestamp = Lens.lens (lastUpdatedTimestamp :: EffectivePolicy -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedTimestamp = a} :: EffectivePolicy)
{-# DEPRECATED epLastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead." #-}

-- | The text content of the policy.
--
-- /Note:/ Consider using 'policyContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epPolicyContent :: Lens.Lens' EffectivePolicy (Lude.Maybe Lude.Text)
epPolicyContent = Lens.lens (policyContent :: EffectivePolicy -> Lude.Maybe Lude.Text) (\s a -> s {policyContent = a} :: EffectivePolicy)
{-# DEPRECATED epPolicyContent "Use generic-lens or generic-optics with 'policyContent' instead." #-}

instance Lude.FromJSON EffectivePolicy where
  parseJSON =
    Lude.withObject
      "EffectivePolicy"
      ( \x ->
          EffectivePolicy'
            Lude.<$> (x Lude..:? "TargetId")
            Lude.<*> (x Lude..:? "PolicyType")
            Lude.<*> (x Lude..:? "LastUpdatedTimestamp")
            Lude.<*> (x Lude..:? "PolicyContent")
      )
