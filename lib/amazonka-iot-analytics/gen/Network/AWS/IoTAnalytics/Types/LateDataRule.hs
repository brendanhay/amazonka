{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.LateDataRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.LateDataRule
  ( LateDataRule (..),

    -- * Smart constructor
    mkLateDataRule,

    -- * Lenses
    ldrRuleConfiguration,
    ldrRuleName,
  )
where

import qualified Network.AWS.IoTAnalytics.Types.LateDataRuleConfiguration as Types
import qualified Network.AWS.IoTAnalytics.Types.LateDataRuleName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A structure that contains the name and configuration information of a late data rule.
--
-- /See:/ 'mkLateDataRule' smart constructor.
data LateDataRule = LateDataRule'
  { -- | The information needed to configure the late data rule.
    ruleConfiguration :: Types.LateDataRuleConfiguration,
    -- | The name of the late data rule.
    ruleName :: Core.Maybe Types.LateDataRuleName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LateDataRule' value with any optional fields omitted.
mkLateDataRule ::
  -- | 'ruleConfiguration'
  Types.LateDataRuleConfiguration ->
  LateDataRule
mkLateDataRule ruleConfiguration =
  LateDataRule' {ruleConfiguration, ruleName = Core.Nothing}

-- | The information needed to configure the late data rule.
--
-- /Note:/ Consider using 'ruleConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrRuleConfiguration :: Lens.Lens' LateDataRule Types.LateDataRuleConfiguration
ldrRuleConfiguration = Lens.field @"ruleConfiguration"
{-# DEPRECATED ldrRuleConfiguration "Use generic-lens or generic-optics with 'ruleConfiguration' instead." #-}

-- | The name of the late data rule.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrRuleName :: Lens.Lens' LateDataRule (Core.Maybe Types.LateDataRuleName)
ldrRuleName = Lens.field @"ruleName"
{-# DEPRECATED ldrRuleName "Use generic-lens or generic-optics with 'ruleName' instead." #-}

instance Core.FromJSON LateDataRule where
  toJSON LateDataRule {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ruleConfiguration" Core..= ruleConfiguration),
            ("ruleName" Core..=) Core.<$> ruleName
          ]
      )

instance Core.FromJSON LateDataRule where
  parseJSON =
    Core.withObject "LateDataRule" Core.$
      \x ->
        LateDataRule'
          Core.<$> (x Core..: "ruleConfiguration") Core.<*> (x Core..:? "ruleName")
