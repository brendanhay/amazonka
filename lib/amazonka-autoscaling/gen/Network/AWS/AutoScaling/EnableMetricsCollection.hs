{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.EnableMetricsCollection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables group metrics for the specified Auto Scaling group. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-monitoring.html Monitoring CloudWatch metrics for your Auto Scaling groups and instances> in the /Amazon EC2 Auto Scaling User Guide/ .
module Network.AWS.AutoScaling.EnableMetricsCollection
  ( -- * Creating a request
    EnableMetricsCollection (..),
    mkEnableMetricsCollection,

    -- ** Request lenses
    emcMetrics,
    emcAutoScalingGroupName,
    emcGranularity,

    -- * Destructuring the response
    EnableMetricsCollectionResponse (..),
    mkEnableMetricsCollectionResponse,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkEnableMetricsCollection' smart constructor.
data EnableMetricsCollection = EnableMetricsCollection'
  { metrics ::
      Lude.Maybe [Lude.Text],
    autoScalingGroupName :: Lude.Text,
    granularity :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableMetricsCollection' with the minimum fields required to make a request.
--
-- * 'autoScalingGroupName' - The name of the Auto Scaling group.
-- * 'granularity' - The granularity to associate with the metrics to collect. The only valid value is @1Minute@ .
-- * 'metrics' - Specifies which group-level metrics to start collecting. You can specify one or more of the following metrics:
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
-- The instance weighting feature supports the following additional metrics:
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
-- If you omit this parameter, all metrics are enabled.
mkEnableMetricsCollection ::
  -- | 'autoScalingGroupName'
  Lude.Text ->
  -- | 'granularity'
  Lude.Text ->
  EnableMetricsCollection
mkEnableMetricsCollection pAutoScalingGroupName_ pGranularity_ =
  EnableMetricsCollection'
    { metrics = Lude.Nothing,
      autoScalingGroupName = pAutoScalingGroupName_,
      granularity = pGranularity_
    }

-- | Specifies which group-level metrics to start collecting. You can specify one or more of the following metrics:
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
-- The instance weighting feature supports the following additional metrics:
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
-- If you omit this parameter, all metrics are enabled.
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emcMetrics :: Lens.Lens' EnableMetricsCollection (Lude.Maybe [Lude.Text])
emcMetrics = Lens.lens (metrics :: EnableMetricsCollection -> Lude.Maybe [Lude.Text]) (\s a -> s {metrics = a} :: EnableMetricsCollection)
{-# DEPRECATED emcMetrics "Use generic-lens or generic-optics with 'metrics' instead." #-}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emcAutoScalingGroupName :: Lens.Lens' EnableMetricsCollection Lude.Text
emcAutoScalingGroupName = Lens.lens (autoScalingGroupName :: EnableMetricsCollection -> Lude.Text) (\s a -> s {autoScalingGroupName = a} :: EnableMetricsCollection)
{-# DEPRECATED emcAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | The granularity to associate with the metrics to collect. The only valid value is @1Minute@ .
--
-- /Note:/ Consider using 'granularity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emcGranularity :: Lens.Lens' EnableMetricsCollection Lude.Text
emcGranularity = Lens.lens (granularity :: EnableMetricsCollection -> Lude.Text) (\s a -> s {granularity = a} :: EnableMetricsCollection)
{-# DEPRECATED emcGranularity "Use generic-lens or generic-optics with 'granularity' instead." #-}

instance Lude.AWSRequest EnableMetricsCollection where
  type Rs EnableMetricsCollection = EnableMetricsCollectionResponse
  request = Req.postQuery autoScalingService
  response = Res.receiveNull EnableMetricsCollectionResponse'

instance Lude.ToHeaders EnableMetricsCollection where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath EnableMetricsCollection where
  toPath = Lude.const "/"

instance Lude.ToQuery EnableMetricsCollection where
  toQuery EnableMetricsCollection' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("EnableMetricsCollection" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "Metrics"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> metrics),
        "AutoScalingGroupName" Lude.=: autoScalingGroupName,
        "Granularity" Lude.=: granularity
      ]

-- | /See:/ 'mkEnableMetricsCollectionResponse' smart constructor.
data EnableMetricsCollectionResponse = EnableMetricsCollectionResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableMetricsCollectionResponse' with the minimum fields required to make a request.
mkEnableMetricsCollectionResponse ::
  EnableMetricsCollectionResponse
mkEnableMetricsCollectionResponse =
  EnableMetricsCollectionResponse'
