{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.Policy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Organizations.Types.Policy
  ( Policy (..)
  -- * Smart constructor
  , mkPolicy
  -- * Lenses
  , pContent
  , pPolicySummary
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types.Content as Types
import qualified Network.AWS.Organizations.Types.PolicySummary as Types
import qualified Network.AWS.Prelude as Core

-- | Contains rules to be applied to the affected accounts. Policies can be attached directly to accounts, or to roots and OUs to affect all accounts in those hierarchies.
--
-- /See:/ 'mkPolicy' smart constructor.
data Policy = Policy'
  { content :: Core.Maybe Types.Content
    -- ^ The text content of the policy.
  , policySummary :: Core.Maybe Types.PolicySummary
    -- ^ A structure that contains additional details about the policy.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Policy' value with any optional fields omitted.
mkPolicy
    :: Policy
mkPolicy
  = Policy'{content = Core.Nothing, policySummary = Core.Nothing}

-- | The text content of the policy.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pContent :: Lens.Lens' Policy (Core.Maybe Types.Content)
pContent = Lens.field @"content"
{-# INLINEABLE pContent #-}
{-# DEPRECATED content "Use generic-lens or generic-optics with 'content' instead"  #-}

-- | A structure that contains additional details about the policy.
--
-- /Note:/ Consider using 'policySummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPolicySummary :: Lens.Lens' Policy (Core.Maybe Types.PolicySummary)
pPolicySummary = Lens.field @"policySummary"
{-# INLINEABLE pPolicySummary #-}
{-# DEPRECATED policySummary "Use generic-lens or generic-optics with 'policySummary' instead"  #-}

instance Core.FromJSON Policy where
        parseJSON
          = Core.withObject "Policy" Core.$
              \ x ->
                Policy' Core.<$>
                  (x Core..:? "Content") Core.<*> x Core..:? "PolicySummary"
