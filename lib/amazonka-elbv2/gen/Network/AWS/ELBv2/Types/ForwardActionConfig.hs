{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.ForwardActionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.ForwardActionConfig
  ( ForwardActionConfig (..),

    -- * Smart constructor
    mkForwardActionConfig,

    -- * Lenses
    facTargetGroups,
    facTargetGroupStickinessConfig,
  )
where

import Network.AWS.ELBv2.Types.TargetGroupStickinessConfig
import Network.AWS.ELBv2.Types.TargetGroupTuple
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a forward action.
--
-- /See:/ 'mkForwardActionConfig' smart constructor.
data ForwardActionConfig = ForwardActionConfig'
  { targetGroups ::
      Lude.Maybe [TargetGroupTuple],
    targetGroupStickinessConfig ::
      Lude.Maybe TargetGroupStickinessConfig
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ForwardActionConfig' with the minimum fields required to make a request.
--
-- * 'targetGroupStickinessConfig' - The target group stickiness for the rule.
-- * 'targetGroups' - One or more target groups. For Network Load Balancers, you can specify a single target group.
mkForwardActionConfig ::
  ForwardActionConfig
mkForwardActionConfig =
  ForwardActionConfig'
    { targetGroups = Lude.Nothing,
      targetGroupStickinessConfig = Lude.Nothing
    }

-- | One or more target groups. For Network Load Balancers, you can specify a single target group.
--
-- /Note:/ Consider using 'targetGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
facTargetGroups :: Lens.Lens' ForwardActionConfig (Lude.Maybe [TargetGroupTuple])
facTargetGroups = Lens.lens (targetGroups :: ForwardActionConfig -> Lude.Maybe [TargetGroupTuple]) (\s a -> s {targetGroups = a} :: ForwardActionConfig)
{-# DEPRECATED facTargetGroups "Use generic-lens or generic-optics with 'targetGroups' instead." #-}

-- | The target group stickiness for the rule.
--
-- /Note:/ Consider using 'targetGroupStickinessConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
facTargetGroupStickinessConfig :: Lens.Lens' ForwardActionConfig (Lude.Maybe TargetGroupStickinessConfig)
facTargetGroupStickinessConfig = Lens.lens (targetGroupStickinessConfig :: ForwardActionConfig -> Lude.Maybe TargetGroupStickinessConfig) (\s a -> s {targetGroupStickinessConfig = a} :: ForwardActionConfig)
{-# DEPRECATED facTargetGroupStickinessConfig "Use generic-lens or generic-optics with 'targetGroupStickinessConfig' instead." #-}

instance Lude.FromXML ForwardActionConfig where
  parseXML x =
    ForwardActionConfig'
      Lude.<$> ( x Lude..@? "TargetGroups" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "TargetGroupStickinessConfig")

instance Lude.ToQuery ForwardActionConfig where
  toQuery ForwardActionConfig' {..} =
    Lude.mconcat
      [ "TargetGroups"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> targetGroups),
        "TargetGroupStickinessConfig" Lude.=: targetGroupStickinessConfig
      ]
