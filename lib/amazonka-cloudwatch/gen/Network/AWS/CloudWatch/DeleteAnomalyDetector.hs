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
    dadMetricName,
    dadNamespace,
    dadStat,
    dadDimensions,

    -- * Destructuring the response
    DeleteAnomalyDetectorResponse (..),
    mkDeleteAnomalyDetectorResponse,

    -- ** Response lenses
    drsResponseStatus,
  )
where

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteAnomalyDetector' smart constructor.
data DeleteAnomalyDetector = DeleteAnomalyDetector'
  { -- | The metric name associated with the anomaly detection model to delete.
    metricName :: Lude.Text,
    -- | The namespace associated with the anomaly detection model to delete.
    namespace :: Lude.Text,
    -- | The statistic associated with the anomaly detection model to delete.
    stat :: Lude.Text,
    -- | The metric dimensions associated with the anomaly detection model to delete.
    dimensions :: Lude.Maybe [Dimension]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAnomalyDetector' with the minimum fields required to make a request.
--
-- * 'metricName' - The metric name associated with the anomaly detection model to delete.
-- * 'namespace' - The namespace associated with the anomaly detection model to delete.
-- * 'stat' - The statistic associated with the anomaly detection model to delete.
-- * 'dimensions' - The metric dimensions associated with the anomaly detection model to delete.
mkDeleteAnomalyDetector ::
  -- | 'metricName'
  Lude.Text ->
  -- | 'namespace'
  Lude.Text ->
  -- | 'stat'
  Lude.Text ->
  DeleteAnomalyDetector
mkDeleteAnomalyDetector pMetricName_ pNamespace_ pStat_ =
  DeleteAnomalyDetector'
    { metricName = pMetricName_,
      namespace = pNamespace_,
      stat = pStat_,
      dimensions = Lude.Nothing
    }

-- | The metric name associated with the anomaly detection model to delete.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dadMetricName :: Lens.Lens' DeleteAnomalyDetector Lude.Text
dadMetricName = Lens.lens (metricName :: DeleteAnomalyDetector -> Lude.Text) (\s a -> s {metricName = a} :: DeleteAnomalyDetector)
{-# DEPRECATED dadMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | The namespace associated with the anomaly detection model to delete.
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dadNamespace :: Lens.Lens' DeleteAnomalyDetector Lude.Text
dadNamespace = Lens.lens (namespace :: DeleteAnomalyDetector -> Lude.Text) (\s a -> s {namespace = a} :: DeleteAnomalyDetector)
{-# DEPRECATED dadNamespace "Use generic-lens or generic-optics with 'namespace' instead." #-}

-- | The statistic associated with the anomaly detection model to delete.
--
-- /Note:/ Consider using 'stat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dadStat :: Lens.Lens' DeleteAnomalyDetector Lude.Text
dadStat = Lens.lens (stat :: DeleteAnomalyDetector -> Lude.Text) (\s a -> s {stat = a} :: DeleteAnomalyDetector)
{-# DEPRECATED dadStat "Use generic-lens or generic-optics with 'stat' instead." #-}

-- | The metric dimensions associated with the anomaly detection model to delete.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dadDimensions :: Lens.Lens' DeleteAnomalyDetector (Lude.Maybe [Dimension])
dadDimensions = Lens.lens (dimensions :: DeleteAnomalyDetector -> Lude.Maybe [Dimension]) (\s a -> s {dimensions = a} :: DeleteAnomalyDetector)
{-# DEPRECATED dadDimensions "Use generic-lens or generic-optics with 'dimensions' instead." #-}

instance Lude.AWSRequest DeleteAnomalyDetector where
  type Rs DeleteAnomalyDetector = DeleteAnomalyDetectorResponse
  request = Req.postQuery cloudWatchService
  response =
    Res.receiveXMLWrapper
      "DeleteAnomalyDetectorResult"
      ( \s h x ->
          DeleteAnomalyDetectorResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteAnomalyDetector where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteAnomalyDetector where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteAnomalyDetector where
  toQuery DeleteAnomalyDetector' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteAnomalyDetector" :: Lude.ByteString),
        "Version" Lude.=: ("2010-08-01" :: Lude.ByteString),
        "MetricName" Lude.=: metricName,
        "Namespace" Lude.=: namespace,
        "Stat" Lude.=: stat,
        "Dimensions"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> dimensions)
      ]

-- | /See:/ 'mkDeleteAnomalyDetectorResponse' smart constructor.
newtype DeleteAnomalyDetectorResponse = DeleteAnomalyDetectorResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAnomalyDetectorResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteAnomalyDetectorResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteAnomalyDetectorResponse
mkDeleteAnomalyDetectorResponse pResponseStatus_ =
  DeleteAnomalyDetectorResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteAnomalyDetectorResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DeleteAnomalyDetectorResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteAnomalyDetectorResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
