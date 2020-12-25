{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.DeleteAnomalyDetector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified anomaly detection model from your account.
module Network.AWS.CloudWatch.DeleteAnomalyDetector
  ( -- * Creating a request
    DeleteAnomalyDetector (..),
    mkDeleteAnomalyDetector,

    -- ** Request lenses
    dadNamespace,
    dadMetricName,
    dadStat,
    dadDimensions,

    -- * Destructuring the response
    DeleteAnomalyDetectorResponse (..),
    mkDeleteAnomalyDetectorResponse,

    -- ** Response lenses
    dadrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudWatch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteAnomalyDetector' smart constructor.
data DeleteAnomalyDetector = DeleteAnomalyDetector'
  { -- | The namespace associated with the anomaly detection model to delete.
    namespace :: Types.Namespace,
    -- | The metric name associated with the anomaly detection model to delete.
    metricName :: Types.MetricName,
    -- | The statistic associated with the anomaly detection model to delete.
    stat :: Types.AnomalyDetectorMetricStat,
    -- | The metric dimensions associated with the anomaly detection model to delete.
    dimensions :: Core.Maybe [Types.Dimension]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAnomalyDetector' value with any optional fields omitted.
mkDeleteAnomalyDetector ::
  -- | 'namespace'
  Types.Namespace ->
  -- | 'metricName'
  Types.MetricName ->
  -- | 'stat'
  Types.AnomalyDetectorMetricStat ->
  DeleteAnomalyDetector
mkDeleteAnomalyDetector namespace metricName stat =
  DeleteAnomalyDetector'
    { namespace,
      metricName,
      stat,
      dimensions = Core.Nothing
    }

-- | The namespace associated with the anomaly detection model to delete.
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dadNamespace :: Lens.Lens' DeleteAnomalyDetector Types.Namespace
dadNamespace = Lens.field @"namespace"
{-# DEPRECATED dadNamespace "Use generic-lens or generic-optics with 'namespace' instead." #-}

-- | The metric name associated with the anomaly detection model to delete.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dadMetricName :: Lens.Lens' DeleteAnomalyDetector Types.MetricName
dadMetricName = Lens.field @"metricName"
{-# DEPRECATED dadMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | The statistic associated with the anomaly detection model to delete.
--
-- /Note:/ Consider using 'stat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dadStat :: Lens.Lens' DeleteAnomalyDetector Types.AnomalyDetectorMetricStat
dadStat = Lens.field @"stat"
{-# DEPRECATED dadStat "Use generic-lens or generic-optics with 'stat' instead." #-}

-- | The metric dimensions associated with the anomaly detection model to delete.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dadDimensions :: Lens.Lens' DeleteAnomalyDetector (Core.Maybe [Types.Dimension])
dadDimensions = Lens.field @"dimensions"
{-# DEPRECATED dadDimensions "Use generic-lens or generic-optics with 'dimensions' instead." #-}

instance Core.AWSRequest DeleteAnomalyDetector where
  type Rs DeleteAnomalyDetector = DeleteAnomalyDetectorResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DeleteAnomalyDetector")
                Core.<> (Core.pure ("Version", "2010-08-01"))
                Core.<> (Core.toQueryValue "Namespace" namespace)
                Core.<> (Core.toQueryValue "MetricName" metricName)
                Core.<> (Core.toQueryValue "Stat" stat)
                Core.<> ( Core.toQueryValue
                            "Dimensions"
                            (Core.toQueryList "member" Core.<$> dimensions)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "DeleteAnomalyDetectorResult"
      ( \s h x ->
          DeleteAnomalyDetectorResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteAnomalyDetectorResponse' smart constructor.
newtype DeleteAnomalyDetectorResponse = DeleteAnomalyDetectorResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAnomalyDetectorResponse' value with any optional fields omitted.
mkDeleteAnomalyDetectorResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteAnomalyDetectorResponse
mkDeleteAnomalyDetectorResponse responseStatus =
  DeleteAnomalyDetectorResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dadrrsResponseStatus :: Lens.Lens' DeleteAnomalyDetectorResponse Core.Int
dadrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dadrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
