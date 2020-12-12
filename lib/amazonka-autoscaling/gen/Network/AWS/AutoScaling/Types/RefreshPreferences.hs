{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.RefreshPreferences
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.RefreshPreferences
  ( RefreshPreferences (..),

    -- * Smart constructor
    mkRefreshPreferences,

    -- * Lenses
    rpMinHealthyPercentage,
    rpInstanceWarmup,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes information used to start an instance refresh.
--
-- /See:/ 'mkRefreshPreferences' smart constructor.
data RefreshPreferences = RefreshPreferences'
  { minHealthyPercentage ::
      Lude.Maybe Lude.Natural,
    instanceWarmup :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RefreshPreferences' with the minimum fields required to make a request.
--
-- * 'instanceWarmup' - The number of seconds until a newly launched instance is configured and ready to use. During this time, Amazon EC2 Auto Scaling does not immediately move on to the next replacement. The default is to use the value for the health check grace period defined for the group.
-- * 'minHealthyPercentage' - The amount of capacity in the Auto Scaling group that must remain healthy during an instance refresh to allow the operation to continue, as a percentage of the desired capacity of the Auto Scaling group (rounded up to the nearest integer). The default is @90@ .
mkRefreshPreferences ::
  RefreshPreferences
mkRefreshPreferences =
  RefreshPreferences'
    { minHealthyPercentage = Lude.Nothing,
      instanceWarmup = Lude.Nothing
    }

-- | The amount of capacity in the Auto Scaling group that must remain healthy during an instance refresh to allow the operation to continue, as a percentage of the desired capacity of the Auto Scaling group (rounded up to the nearest integer). The default is @90@ .
--
-- /Note:/ Consider using 'minHealthyPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpMinHealthyPercentage :: Lens.Lens' RefreshPreferences (Lude.Maybe Lude.Natural)
rpMinHealthyPercentage = Lens.lens (minHealthyPercentage :: RefreshPreferences -> Lude.Maybe Lude.Natural) (\s a -> s {minHealthyPercentage = a} :: RefreshPreferences)
{-# DEPRECATED rpMinHealthyPercentage "Use generic-lens or generic-optics with 'minHealthyPercentage' instead." #-}

-- | The number of seconds until a newly launched instance is configured and ready to use. During this time, Amazon EC2 Auto Scaling does not immediately move on to the next replacement. The default is to use the value for the health check grace period defined for the group.
--
-- /Note:/ Consider using 'instanceWarmup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpInstanceWarmup :: Lens.Lens' RefreshPreferences (Lude.Maybe Lude.Natural)
rpInstanceWarmup = Lens.lens (instanceWarmup :: RefreshPreferences -> Lude.Maybe Lude.Natural) (\s a -> s {instanceWarmup = a} :: RefreshPreferences)
{-# DEPRECATED rpInstanceWarmup "Use generic-lens or generic-optics with 'instanceWarmup' instead." #-}

instance Lude.ToQuery RefreshPreferences where
  toQuery RefreshPreferences' {..} =
    Lude.mconcat
      [ "MinHealthyPercentage" Lude.=: minHealthyPercentage,
        "InstanceWarmup" Lude.=: instanceWarmup
      ]
