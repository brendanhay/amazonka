{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.ResourcePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.ResourcePolicy
  ( ResourcePolicy (..),

    -- * Smart constructor
    mkResourcePolicy,

    -- * Lenses
    rpLastUpdatedTime,
    rpPolicyDocument,
    rpPolicyName,
  )
where

import qualified Network.AWS.CloudWatchLogs.Types.PolicyDocument as Types
import qualified Network.AWS.CloudWatchLogs.Types.PolicyName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A policy enabling one or more entities to put logs to a log group in this account.
--
-- /See:/ 'mkResourcePolicy' smart constructor.
data ResourcePolicy = ResourcePolicy'
  { -- | Timestamp showing when this policy was last updated, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
    lastUpdatedTime :: Core.Maybe Core.Natural,
    -- | The details of the policy.
    policyDocument :: Core.Maybe Types.PolicyDocument,
    -- | The name of the resource policy.
    policyName :: Core.Maybe Types.PolicyName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourcePolicy' value with any optional fields omitted.
mkResourcePolicy ::
  ResourcePolicy
mkResourcePolicy =
  ResourcePolicy'
    { lastUpdatedTime = Core.Nothing,
      policyDocument = Core.Nothing,
      policyName = Core.Nothing
    }

-- | Timestamp showing when this policy was last updated, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- /Note:/ Consider using 'lastUpdatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpLastUpdatedTime :: Lens.Lens' ResourcePolicy (Core.Maybe Core.Natural)
rpLastUpdatedTime = Lens.field @"lastUpdatedTime"
{-# DEPRECATED rpLastUpdatedTime "Use generic-lens or generic-optics with 'lastUpdatedTime' instead." #-}

-- | The details of the policy.
--
-- /Note:/ Consider using 'policyDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpPolicyDocument :: Lens.Lens' ResourcePolicy (Core.Maybe Types.PolicyDocument)
rpPolicyDocument = Lens.field @"policyDocument"
{-# DEPRECATED rpPolicyDocument "Use generic-lens or generic-optics with 'policyDocument' instead." #-}

-- | The name of the resource policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpPolicyName :: Lens.Lens' ResourcePolicy (Core.Maybe Types.PolicyName)
rpPolicyName = Lens.field @"policyName"
{-# DEPRECATED rpPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

instance Core.FromJSON ResourcePolicy where
  parseJSON =
    Core.withObject "ResourcePolicy" Core.$
      \x ->
        ResourcePolicy'
          Core.<$> (x Core..:? "lastUpdatedTime")
          Core.<*> (x Core..:? "policyDocument")
          Core.<*> (x Core..:? "policyName")
