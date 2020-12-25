{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.WafAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.WafAction
  ( WafAction (..),

    -- * Smart constructor
    mkWafAction,

    -- * Lenses
    waType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WAFRegional.Types.WafActionType as Types

-- | For the action that is associated with a rule in a @WebACL@ , specifies the action that you want AWS WAF to perform when a web request matches all of the conditions in a rule. For the default action in a @WebACL@ , specifies the action that you want AWS WAF to take when a web request doesn't match all of the conditions in any of the rules in a @WebACL@ .
--
-- /See:/ 'mkWafAction' smart constructor.
newtype WafAction = WafAction'
  { -- | Specifies how you want AWS WAF to respond to requests that match the settings in a @Rule@ . Valid settings include the following:
    --
    --
    --     * @ALLOW@ : AWS WAF allows requests
    --
    --
    --     * @BLOCK@ : AWS WAF blocks requests
    --
    --
    --     * @COUNT@ : AWS WAF increments a counter of the requests that match all of the conditions in the rule. AWS WAF then continues to inspect the web request based on the remaining rules in the web ACL. You can't specify @COUNT@ for the default action for a @WebACL@ .
    type' :: Types.WafActionType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'WafAction' value with any optional fields omitted.
mkWafAction ::
  -- | 'type\''
  Types.WafActionType ->
  WafAction
mkWafAction type' = WafAction' {type'}

-- | Specifies how you want AWS WAF to respond to requests that match the settings in a @Rule@ . Valid settings include the following:
--
--
--     * @ALLOW@ : AWS WAF allows requests
--
--
--     * @BLOCK@ : AWS WAF blocks requests
--
--
--     * @COUNT@ : AWS WAF increments a counter of the requests that match all of the conditions in the rule. AWS WAF then continues to inspect the web request based on the remaining rules in the web ACL. You can't specify @COUNT@ for the default action for a @WebACL@ .
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
waType :: Lens.Lens' WafAction Types.WafActionType
waType = Lens.field @"type'"
{-# DEPRECATED waType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON WafAction where
  toJSON WafAction {..} =
    Core.object (Core.catMaybes [Core.Just ("Type" Core..= type')])

instance Core.FromJSON WafAction where
  parseJSON =
    Core.withObject "WafAction" Core.$
      \x -> WafAction' Core.<$> (x Core..: "Type")
