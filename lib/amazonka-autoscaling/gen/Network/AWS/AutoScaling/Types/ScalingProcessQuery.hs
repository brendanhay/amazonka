{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.ScalingProcessQuery
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.ScalingProcessQuery
  ( ScalingProcessQuery (..),

    -- * Smart constructor
    mkScalingProcessQuery,

    -- * Lenses
    spqScalingProcesses,
    spqAutoScalingGroupName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | /See:/ 'mkScalingProcessQuery' smart constructor.
data ScalingProcessQuery = ScalingProcessQuery'
  { scalingProcesses ::
      Lude.Maybe [Lude.Text],
    autoScalingGroupName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScalingProcessQuery' with the minimum fields required to make a request.
--
-- * 'autoScalingGroupName' - The name of the Auto Scaling group.
-- * 'scalingProcesses' - One or more of the following processes:
--
--
--     * @Launch@
--
--
--     * @Terminate@
--
--
--     * @AddToLoadBalancer@
--
--
--     * @AlarmNotification@
--
--
--     * @AZRebalance@
--
--
--     * @HealthCheck@
--
--
--     * @InstanceRefresh@
--
--
--     * @ReplaceUnhealthy@
--
--
--     * @ScheduledActions@
--
--
-- If you omit this parameter, all processes are specified.
mkScalingProcessQuery ::
  -- | 'autoScalingGroupName'
  Lude.Text ->
  ScalingProcessQuery
mkScalingProcessQuery pAutoScalingGroupName_ =
  ScalingProcessQuery'
    { scalingProcesses = Lude.Nothing,
      autoScalingGroupName = pAutoScalingGroupName_
    }

-- | One or more of the following processes:
--
--
--     * @Launch@
--
--
--     * @Terminate@
--
--
--     * @AddToLoadBalancer@
--
--
--     * @AlarmNotification@
--
--
--     * @AZRebalance@
--
--
--     * @HealthCheck@
--
--
--     * @InstanceRefresh@
--
--
--     * @ReplaceUnhealthy@
--
--
--     * @ScheduledActions@
--
--
-- If you omit this parameter, all processes are specified.
--
-- /Note:/ Consider using 'scalingProcesses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spqScalingProcesses :: Lens.Lens' ScalingProcessQuery (Lude.Maybe [Lude.Text])
spqScalingProcesses = Lens.lens (scalingProcesses :: ScalingProcessQuery -> Lude.Maybe [Lude.Text]) (\s a -> s {scalingProcesses = a} :: ScalingProcessQuery)
{-# DEPRECATED spqScalingProcesses "Use generic-lens or generic-optics with 'scalingProcesses' instead." #-}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spqAutoScalingGroupName :: Lens.Lens' ScalingProcessQuery Lude.Text
spqAutoScalingGroupName = Lens.lens (autoScalingGroupName :: ScalingProcessQuery -> Lude.Text) (\s a -> s {autoScalingGroupName = a} :: ScalingProcessQuery)
{-# DEPRECATED spqAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

instance Lude.ToQuery ScalingProcessQuery where
  toQuery ScalingProcessQuery' {..} =
    Lude.mconcat
      [ "ScalingProcesses"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> scalingProcesses),
        "AutoScalingGroupName" Lude.=: autoScalingGroupName
      ]
