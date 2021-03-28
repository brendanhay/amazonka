{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.IpRuleItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types.IpRuleItem
  ( IpRuleItem (..)
  -- * Smart constructor
  , mkIpRuleItem
  -- * Lenses
  , iriIpRule
  , iriRuleDesc
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkSpaces.Types.IpRule as Types
import qualified Network.AWS.WorkSpaces.Types.RuleDesc as Types

-- | Describes a rule for an IP access control group.
--
-- /See:/ 'mkIpRuleItem' smart constructor.
data IpRuleItem = IpRuleItem'
  { ipRule :: Core.Maybe Types.IpRule
    -- ^ The IP address range, in CIDR notation.
  , ruleDesc :: Core.Maybe Types.RuleDesc
    -- ^ The description.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IpRuleItem' value with any optional fields omitted.
mkIpRuleItem
    :: IpRuleItem
mkIpRuleItem
  = IpRuleItem'{ipRule = Core.Nothing, ruleDesc = Core.Nothing}

-- | The IP address range, in CIDR notation.
--
-- /Note:/ Consider using 'ipRule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iriIpRule :: Lens.Lens' IpRuleItem (Core.Maybe Types.IpRule)
iriIpRule = Lens.field @"ipRule"
{-# INLINEABLE iriIpRule #-}
{-# DEPRECATED ipRule "Use generic-lens or generic-optics with 'ipRule' instead"  #-}

-- | The description.
--
-- /Note:/ Consider using 'ruleDesc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iriRuleDesc :: Lens.Lens' IpRuleItem (Core.Maybe Types.RuleDesc)
iriRuleDesc = Lens.field @"ruleDesc"
{-# INLINEABLE iriRuleDesc #-}
{-# DEPRECATED ruleDesc "Use generic-lens or generic-optics with 'ruleDesc' instead"  #-}

instance Core.FromJSON IpRuleItem where
        toJSON IpRuleItem{..}
          = Core.object
              (Core.catMaybes
                 [("ipRule" Core..=) Core.<$> ipRule,
                  ("ruleDesc" Core..=) Core.<$> ruleDesc])

instance Core.FromJSON IpRuleItem where
        parseJSON
          = Core.withObject "IpRuleItem" Core.$
              \ x ->
                IpRuleItem' Core.<$>
                  (x Core..:? "ipRule") Core.<*> x Core..:? "ruleDesc"
