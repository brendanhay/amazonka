-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.TargetGroupStickinessConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.TargetGroupStickinessConfig
  ( TargetGroupStickinessConfig (..),

    -- * Smart constructor
    mkTargetGroupStickinessConfig,

    -- * Lenses
    tgscEnabled,
    tgscDurationSeconds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the target group stickiness for a rule.
--
-- /See:/ 'mkTargetGroupStickinessConfig' smart constructor.
data TargetGroupStickinessConfig = TargetGroupStickinessConfig'
  { enabled ::
      Lude.Maybe Lude.Bool,
    durationSeconds ::
      Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TargetGroupStickinessConfig' with the minimum fields required to make a request.
--
-- * 'durationSeconds' - The time period, in seconds, during which requests from a client should be routed to the same target group. The range is 1-604800 seconds (7 days).
-- * 'enabled' - Indicates whether target group stickiness is enabled.
mkTargetGroupStickinessConfig ::
  TargetGroupStickinessConfig
mkTargetGroupStickinessConfig =
  TargetGroupStickinessConfig'
    { enabled = Lude.Nothing,
      durationSeconds = Lude.Nothing
    }

-- | Indicates whether target group stickiness is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgscEnabled :: Lens.Lens' TargetGroupStickinessConfig (Lude.Maybe Lude.Bool)
tgscEnabled = Lens.lens (enabled :: TargetGroupStickinessConfig -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: TargetGroupStickinessConfig)
{-# DEPRECATED tgscEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The time period, in seconds, during which requests from a client should be routed to the same target group. The range is 1-604800 seconds (7 days).
--
-- /Note:/ Consider using 'durationSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgscDurationSeconds :: Lens.Lens' TargetGroupStickinessConfig (Lude.Maybe Lude.Int)
tgscDurationSeconds = Lens.lens (durationSeconds :: TargetGroupStickinessConfig -> Lude.Maybe Lude.Int) (\s a -> s {durationSeconds = a} :: TargetGroupStickinessConfig)
{-# DEPRECATED tgscDurationSeconds "Use generic-lens or generic-optics with 'durationSeconds' instead." #-}

instance Lude.FromXML TargetGroupStickinessConfig where
  parseXML x =
    TargetGroupStickinessConfig'
      Lude.<$> (x Lude..@? "Enabled") Lude.<*> (x Lude..@? "DurationSeconds")

instance Lude.ToQuery TargetGroupStickinessConfig where
  toQuery TargetGroupStickinessConfig' {..} =
    Lude.mconcat
      [ "Enabled" Lude.=: enabled,
        "DurationSeconds" Lude.=: durationSeconds
      ]
