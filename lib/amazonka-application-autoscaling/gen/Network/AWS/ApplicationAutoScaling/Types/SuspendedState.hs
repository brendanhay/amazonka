{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.SuspendedState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApplicationAutoScaling.Types.SuspendedState
  ( SuspendedState (..),

    -- * Smart constructor
    mkSuspendedState,

    -- * Lenses
    ssDynamicScalingInSuspended,
    ssScheduledScalingSuspended,
    ssDynamicScalingOutSuspended,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies whether the scaling activities for a scalable target are in a suspended state.
--
-- /See:/ 'mkSuspendedState' smart constructor.
data SuspendedState = SuspendedState'
  { dynamicScalingInSuspended ::
      Lude.Maybe Lude.Bool,
    scheduledScalingSuspended :: Lude.Maybe Lude.Bool,
    dynamicScalingOutSuspended :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SuspendedState' with the minimum fields required to make a request.
--
-- * 'dynamicScalingInSuspended' - Whether scale in by a target tracking scaling policy or a step scaling policy is suspended. Set the value to @true@ if you don't want Application Auto Scaling to remove capacity when a scaling policy is triggered. The default is @false@ .
-- * 'dynamicScalingOutSuspended' - Whether scale out by a target tracking scaling policy or a step scaling policy is suspended. Set the value to @true@ if you don't want Application Auto Scaling to add capacity when a scaling policy is triggered. The default is @false@ .
-- * 'scheduledScalingSuspended' - Whether scheduled scaling is suspended. Set the value to @true@ if you don't want Application Auto Scaling to add or remove capacity by initiating scheduled actions. The default is @false@ .
mkSuspendedState ::
  SuspendedState
mkSuspendedState =
  SuspendedState'
    { dynamicScalingInSuspended = Lude.Nothing,
      scheduledScalingSuspended = Lude.Nothing,
      dynamicScalingOutSuspended = Lude.Nothing
    }

-- | Whether scale in by a target tracking scaling policy or a step scaling policy is suspended. Set the value to @true@ if you don't want Application Auto Scaling to remove capacity when a scaling policy is triggered. The default is @false@ .
--
-- /Note:/ Consider using 'dynamicScalingInSuspended' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssDynamicScalingInSuspended :: Lens.Lens' SuspendedState (Lude.Maybe Lude.Bool)
ssDynamicScalingInSuspended = Lens.lens (dynamicScalingInSuspended :: SuspendedState -> Lude.Maybe Lude.Bool) (\s a -> s {dynamicScalingInSuspended = a} :: SuspendedState)
{-# DEPRECATED ssDynamicScalingInSuspended "Use generic-lens or generic-optics with 'dynamicScalingInSuspended' instead." #-}

-- | Whether scheduled scaling is suspended. Set the value to @true@ if you don't want Application Auto Scaling to add or remove capacity by initiating scheduled actions. The default is @false@ .
--
-- /Note:/ Consider using 'scheduledScalingSuspended' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssScheduledScalingSuspended :: Lens.Lens' SuspendedState (Lude.Maybe Lude.Bool)
ssScheduledScalingSuspended = Lens.lens (scheduledScalingSuspended :: SuspendedState -> Lude.Maybe Lude.Bool) (\s a -> s {scheduledScalingSuspended = a} :: SuspendedState)
{-# DEPRECATED ssScheduledScalingSuspended "Use generic-lens or generic-optics with 'scheduledScalingSuspended' instead." #-}

-- | Whether scale out by a target tracking scaling policy or a step scaling policy is suspended. Set the value to @true@ if you don't want Application Auto Scaling to add capacity when a scaling policy is triggered. The default is @false@ .
--
-- /Note:/ Consider using 'dynamicScalingOutSuspended' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssDynamicScalingOutSuspended :: Lens.Lens' SuspendedState (Lude.Maybe Lude.Bool)
ssDynamicScalingOutSuspended = Lens.lens (dynamicScalingOutSuspended :: SuspendedState -> Lude.Maybe Lude.Bool) (\s a -> s {dynamicScalingOutSuspended = a} :: SuspendedState)
{-# DEPRECATED ssDynamicScalingOutSuspended "Use generic-lens or generic-optics with 'dynamicScalingOutSuspended' instead." #-}

instance Lude.FromJSON SuspendedState where
  parseJSON =
    Lude.withObject
      "SuspendedState"
      ( \x ->
          SuspendedState'
            Lude.<$> (x Lude..:? "DynamicScalingInSuspended")
            Lude.<*> (x Lude..:? "ScheduledScalingSuspended")
            Lude.<*> (x Lude..:? "DynamicScalingOutSuspended")
      )

instance Lude.ToJSON SuspendedState where
  toJSON SuspendedState' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DynamicScalingInSuspended" Lude..=)
              Lude.<$> dynamicScalingInSuspended,
            ("ScheduledScalingSuspended" Lude..=)
              Lude.<$> scheduledScalingSuspended,
            ("DynamicScalingOutSuspended" Lude..=)
              Lude.<$> dynamicScalingOutSuspended
          ]
      )
