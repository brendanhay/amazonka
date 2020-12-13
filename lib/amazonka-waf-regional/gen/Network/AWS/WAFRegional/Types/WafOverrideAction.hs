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
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WAFRegional.Types.WafOverrideActionType

-- | The action to take if any rule within the @RuleGroup@ matches a request.
--
-- /See:/ 'mkWafOverrideAction' smart constructor.
newtype WafOverrideAction = WafOverrideAction'
  { -- | @COUNT@ overrides the action specified by the individual rule within a @RuleGroup@ . If set to @NONE@ , the rule's action will take place.
    type' :: WafOverrideActionType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WafOverrideAction' with the minimum fields required to make a request.
--
-- * 'type'' - @COUNT@ overrides the action specified by the individual rule within a @RuleGroup@ . If set to @NONE@ , the rule's action will take place.
mkWafOverrideAction ::
  -- | 'type''
  WafOverrideActionType ->
  WafOverrideAction
mkWafOverrideAction pType_ = WafOverrideAction' {type' = pType_}

-- | @COUNT@ overrides the action specified by the individual rule within a @RuleGroup@ . If set to @NONE@ , the rule's action will take place.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
woaType :: Lens.Lens' WafOverrideAction WafOverrideActionType
woaType = Lens.lens (type' :: WafOverrideAction -> WafOverrideActionType) (\s a -> s {type' = a} :: WafOverrideAction)
{-# DEPRECATED woaType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON WafOverrideAction where
  parseJSON =
    Lude.withObject
      "WafOverrideAction"
      (\x -> WafOverrideAction' Lude.<$> (x Lude..: "Type"))

instance Lude.ToJSON WafOverrideAction where
  toJSON WafOverrideAction' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Type" Lude..= type')])
