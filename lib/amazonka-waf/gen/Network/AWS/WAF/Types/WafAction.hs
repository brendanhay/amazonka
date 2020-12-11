-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.WafAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.WafAction
  ( WafAction (..),

    -- * Smart constructor
    mkWafAction,

    -- * Lenses
    waType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WAF.Types.WafActionType

-- | For the action that is associated with a rule in a @WebACL@ , specifies the action that you want AWS WAF to perform when a web request matches all of the conditions in a rule. For the default action in a @WebACL@ , specifies the action that you want AWS WAF to take when a web request doesn't match all of the conditions in any of the rules in a @WebACL@ .
--
-- /See:/ 'mkWafAction' smart constructor.
newtype WafAction = WafAction' {type' :: WafActionType}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WafAction' with the minimum fields required to make a request.
--
-- * 'type'' - Specifies how you want AWS WAF to respond to requests that match the settings in a @Rule@ . Valid settings include the following:
--
--
--     * @ALLOW@ : AWS WAF allows requests
--
--
--     * @BLOCK@ : AWS WAF blocks requests
--
--
--     * @COUNT@ : AWS WAF increments a counter of the requests that match all of the conditions in the rule. AWS WAF then continues to inspect the web request based on the remaining rules in the web ACL. You can't specify @COUNT@ for the default action for a @WebACL@ .
mkWafAction ::
  -- | 'type''
  WafActionType ->
  WafAction
mkWafAction pType_ = WafAction' {type' = pType_}

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
waType :: Lens.Lens' WafAction WafActionType
waType = Lens.lens (type' :: WafAction -> WafActionType) (\s a -> s {type' = a} :: WafAction)
{-# DEPRECATED waType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON WafAction where
  parseJSON =
    Lude.withObject
      "WafAction"
      (\x -> WafAction' Lude.<$> (x Lude..: "Type"))

instance Lude.ToJSON WafAction where
  toJSON WafAction' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Type" Lude..= type')])
