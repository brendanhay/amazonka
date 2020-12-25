{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.WafOverrideAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.WafOverrideAction
  ( WafOverrideAction (..),

    -- * Smart constructor
    mkWafOverrideAction,

    -- * Lenses
    woaType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WAFRegional.Types.WafOverrideActionType as Types

-- | The action to take if any rule within the @RuleGroup@ matches a request.
--
-- /See:/ 'mkWafOverrideAction' smart constructor.
newtype WafOverrideAction = WafOverrideAction'
  { -- | @COUNT@ overrides the action specified by the individual rule within a @RuleGroup@ . If set to @NONE@ , the rule's action will take place.
    type' :: Types.WafOverrideActionType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'WafOverrideAction' value with any optional fields omitted.
mkWafOverrideAction ::
  -- | 'type\''
  Types.WafOverrideActionType ->
  WafOverrideAction
mkWafOverrideAction type' = WafOverrideAction' {type'}

-- | @COUNT@ overrides the action specified by the individual rule within a @RuleGroup@ . If set to @NONE@ , the rule's action will take place.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
woaType :: Lens.Lens' WafOverrideAction Types.WafOverrideActionType
woaType = Lens.field @"type'"
{-# DEPRECATED woaType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON WafOverrideAction where
  toJSON WafOverrideAction {..} =
    Core.object (Core.catMaybes [Core.Just ("Type" Core..= type')])

instance Core.FromJSON WafOverrideAction where
  parseJSON =
    Core.withObject "WafOverrideAction" Core.$
      \x -> WafOverrideAction' Core.<$> (x Core..: "Type")
