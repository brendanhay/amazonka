{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DisableMetricsCollection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables group metrics for the specified Auto Scaling group.
module Network.AWS.AutoScaling.DisableMetricsCollection
  ( -- * Creating a request
    DisableMetricsCollection (..),
    mkDisableMetricsCollection,

    -- ** Request lenses
    dmcMetrics,
    dmcAutoScalingGroupName,

    -- * Destructuring the response
    DisableMetricsCollectionResponse (..),
    mkDisableMetricsCollectionResponse,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisableMetricsCollection' smart constructor.
data DisableMetricsCollection = DisableMetricsCollection'
  { metrics ::
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

-- | Creates a value of 'DisableMetricsCollection' with the minimum fields required to make a request.
--
-- * 'autoScalingGroupName' - The name of the Auto Scaling group.
-- * 'metrics' - Specifies one or more of the following metrics:
--
--
--     * @GroupMinSize@
--
--
--     * @GroupMaxSize@
--
--
--     * @GroupDesiredCapacity@
--
--
--     * @GroupInServiceInstances@
--
--
--     * @GroupPendingInstances@
--
--
--     * @GroupStandbyInstances@
--
--
--     * @GroupTerminatingInstances@
--
--
--     * @GroupTotalInstances@
--
--
--     * @GroupInServiceCapacity@
--
--
--     * @GroupPendingCapacity@
--
--
--     * @GroupStandbyCapacity@
--
--
--     * @GroupTerminatingCapacity@
--
--
--     * @GroupTotalCapacity@
--
--
-- If you omit this parameter, all metrics are disabled.
mkDisableMetricsCollection ::
  -- | 'autoScalingGroupName'
  Lude.Text ->
  DisableMetricsCollection
mkDisableMetricsCollection pAutoScalingGroupName_ =
  DisableMetricsCollection'
    { metrics = Lude.Nothing,
      autoScalingGroupName = pAutoScalingGroupName_
    }

-- | Specifies one or more of the following metrics:
--
--
--     * @GroupMinSize@
--
--
--     * @GroupMaxSize@
--
--
--     * @GroupDesiredCapacity@
--
--
--     * @GroupInServiceInstances@
--
--
--     * @GroupPendingInstances@
--
--
--     * @GroupStandbyInstances@
--
--
--     * @GroupTerminatingInstances@
--
--
--     * @GroupTotalInstances@
--
--
--     * @GroupInServiceCapacity@
--
--
--     * @GroupPendingCapacity@
--
--
--     * @GroupStandbyCapacity@
--
--
--     * @GroupTerminatingCapacity@
--
--
--     * @GroupTotalCapacity@
--
--
-- If you omit this parameter, all metrics are disabled.
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcMetrics :: Lens.Lens' DisableMetricsCollection (Lude.Maybe [Lude.Text])
dmcMetrics = Lens.lens (metrics :: DisableMetricsCollection -> Lude.Maybe [Lude.Text]) (\s a -> s {metrics = a} :: DisableMetricsCollection)
{-# DEPRECATED dmcMetrics "Use generic-lens or generic-optics with 'metrics' instead." #-}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcAutoScalingGroupName :: Lens.Lens' DisableMetricsCollection Lude.Text
dmcAutoScalingGroupName = Lens.lens (autoScalingGroupName :: DisableMetricsCollection -> Lude.Text) (\s a -> s {autoScalingGroupName = a} :: DisableMetricsCollection)
{-# DEPRECATED dmcAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

instance Lude.AWSRequest DisableMetricsCollection where
  type Rs DisableMetricsCollection = DisableMetricsCollectionResponse
  request = Req.postQuery autoScalingService
  response = Res.receiveNull DisableMetricsCollectionResponse'

instance Lude.ToHeaders DisableMetricsCollection where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DisableMetricsCollection where
  toPath = Lude.const "/"

instance Lude.ToQuery DisableMetricsCollection where
  toQuery DisableMetricsCollection' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DisableMetricsCollection" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "Metrics"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> metrics),
        "AutoScalingGroupName" Lude.=: autoScalingGroupName
      ]

-- | /See:/ 'mkDisableMetricsCollectionResponse' smart constructor.
data DisableMetricsCollectionResponse = DisableMetricsCollectionResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableMetricsCollectionResponse' with the minimum fields required to make a request.
mkDisableMetricsCollectionResponse ::
  DisableMetricsCollectionResponse
mkDisableMetricsCollectionResponse =
  DisableMetricsCollectionResponse'
