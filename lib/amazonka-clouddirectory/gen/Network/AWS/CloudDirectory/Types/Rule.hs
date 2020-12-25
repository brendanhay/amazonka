{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.Rule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.Rule
  ( Rule (..),

    -- * Smart constructor
    mkRule,

    -- * Lenses
    rParameters,
    rType,
  )
where

import qualified Network.AWS.CloudDirectory.Types.RuleParameterKey as Types
import qualified Network.AWS.CloudDirectory.Types.RuleParameterValue as Types
import qualified Network.AWS.CloudDirectory.Types.RuleType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains an Amazon Resource Name (ARN) and parameters that are associated with the rule.
--
-- /See:/ 'mkRule' smart constructor.
data Rule = Rule'
  { -- | The minimum and maximum parameters that are associated with the rule.
    parameters :: Core.Maybe (Core.HashMap Types.RuleParameterKey Types.RuleParameterValue),
    -- | The type of attribute validation rule.
    type' :: Core.Maybe Types.RuleType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Rule' value with any optional fields omitted.
mkRule ::
  Rule
mkRule = Rule' {parameters = Core.Nothing, type' = Core.Nothing}

-- | The minimum and maximum parameters that are associated with the rule.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rParameters :: Lens.Lens' Rule (Core.Maybe (Core.HashMap Types.RuleParameterKey Types.RuleParameterValue))
rParameters = Lens.field @"parameters"
{-# DEPRECATED rParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The type of attribute validation rule.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rType :: Lens.Lens' Rule (Core.Maybe Types.RuleType)
rType = Lens.field @"type'"
{-# DEPRECATED rType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON Rule where
  toJSON Rule {..} =
    Core.object
      ( Core.catMaybes
          [ ("Parameters" Core..=) Core.<$> parameters,
            ("Type" Core..=) Core.<$> type'
          ]
      )

instance Core.FromJSON Rule where
  parseJSON =
    Core.withObject "Rule" Core.$
      \x ->
        Rule'
          Core.<$> (x Core..:? "Parameters") Core.<*> (x Core..:? "Type")
