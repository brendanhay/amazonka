{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.EffectivePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Organizations.Types.EffectivePolicy
  ( EffectivePolicy (..)
  -- * Smart constructor
  , mkEffectivePolicy
  -- * Lenses
  , epLastUpdatedTimestamp
  , epPolicyContent
  , epPolicyType
  , epTargetId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types.EffectivePolicyType as Types
import qualified Network.AWS.Organizations.Types.PolicyContent as Types
import qualified Network.AWS.Organizations.Types.TargetId as Types
import qualified Network.AWS.Prelude as Core

-- | Contains rules to be applied to the affected accounts. The effective policy is the aggregation of any policies the account inherits, plus any policy directly attached to the account.
--
-- /See:/ 'mkEffectivePolicy' smart constructor.
data EffectivePolicy = EffectivePolicy'
  { lastUpdatedTimestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ The time of the last update to this policy.
  , policyContent :: Core.Maybe Types.PolicyContent
    -- ^ The text content of the policy.
  , policyType :: Core.Maybe Types.EffectivePolicyType
    -- ^ The policy type.
  , targetId :: Core.Maybe Types.TargetId
    -- ^ The account ID of the policy target. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'EffectivePolicy' value with any optional fields omitted.
mkEffectivePolicy
    :: EffectivePolicy
mkEffectivePolicy
  = EffectivePolicy'{lastUpdatedTimestamp = Core.Nothing,
                     policyContent = Core.Nothing, policyType = Core.Nothing,
                     targetId = Core.Nothing}

-- | The time of the last update to this policy.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epLastUpdatedTimestamp :: Lens.Lens' EffectivePolicy (Core.Maybe Core.NominalDiffTime)
epLastUpdatedTimestamp = Lens.field @"lastUpdatedTimestamp"
{-# INLINEABLE epLastUpdatedTimestamp #-}
{-# DEPRECATED lastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead"  #-}

-- | The text content of the policy.
--
-- /Note:/ Consider using 'policyContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epPolicyContent :: Lens.Lens' EffectivePolicy (Core.Maybe Types.PolicyContent)
epPolicyContent = Lens.field @"policyContent"
{-# INLINEABLE epPolicyContent #-}
{-# DEPRECATED policyContent "Use generic-lens or generic-optics with 'policyContent' instead"  #-}

-- | The policy type.
--
-- /Note:/ Consider using 'policyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epPolicyType :: Lens.Lens' EffectivePolicy (Core.Maybe Types.EffectivePolicyType)
epPolicyType = Lens.field @"policyType"
{-# INLINEABLE epPolicyType #-}
{-# DEPRECATED policyType "Use generic-lens or generic-optics with 'policyType' instead"  #-}

-- | The account ID of the policy target. 
--
-- /Note:/ Consider using 'targetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epTargetId :: Lens.Lens' EffectivePolicy (Core.Maybe Types.TargetId)
epTargetId = Lens.field @"targetId"
{-# INLINEABLE epTargetId #-}
{-# DEPRECATED targetId "Use generic-lens or generic-optics with 'targetId' instead"  #-}

instance Core.FromJSON EffectivePolicy where
        parseJSON
          = Core.withObject "EffectivePolicy" Core.$
              \ x ->
                EffectivePolicy' Core.<$>
                  (x Core..:? "LastUpdatedTimestamp") Core.<*>
                    x Core..:? "PolicyContent"
                    Core.<*> x Core..:? "PolicyType"
                    Core.<*> x Core..:? "TargetId"
