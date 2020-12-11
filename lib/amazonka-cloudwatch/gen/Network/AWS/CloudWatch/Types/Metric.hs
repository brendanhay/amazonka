-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.Metric
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.Metric
  ( Metric (..),

    -- * Smart constructor
    mkMetric,

    -- * Lenses
    mMetricName,
    mNamespace,
    mDimensions,
  )
where

import Network.AWS.CloudWatch.Types.Dimension
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a specific metric.
--
-- /See:/ 'mkMetric' smart constructor.
data Metric = Metric'
  { metricName :: Lude.Maybe Lude.Text,
    namespace :: Lude.Maybe Lude.Text,
    dimensions :: Lude.Maybe [Dimension]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Metric' with the minimum fields required to make a request.
--
-- * 'dimensions' - The dimensions for the metric.
-- * 'metricName' - The name of the metric. This is a required field.
-- * 'namespace' - The namespace of the metric.
mkMetric ::
  Metric
mkMetric =
  Metric'
    { metricName = Lude.Nothing,
      namespace = Lude.Nothing,
      dimensions = Lude.Nothing
    }

-- | The name of the metric. This is a required field.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mMetricName :: Lens.Lens' Metric (Lude.Maybe Lude.Text)
mMetricName = Lens.lens (metricName :: Metric -> Lude.Maybe Lude.Text) (\s a -> s {metricName = a} :: Metric)
{-# DEPRECATED mMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | The namespace of the metric.
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mNamespace :: Lens.Lens' Metric (Lude.Maybe Lude.Text)
mNamespace = Lens.lens (namespace :: Metric -> Lude.Maybe Lude.Text) (\s a -> s {namespace = a} :: Metric)
{-# DEPRECATED mNamespace "Use generic-lens or generic-optics with 'namespace' instead." #-}

-- | The dimensions for the metric.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mDimensions :: Lens.Lens' Metric (Lude.Maybe [Dimension])
mDimensions = Lens.lens (dimensions :: Metric -> Lude.Maybe [Dimension]) (\s a -> s {dimensions = a} :: Metric)
{-# DEPRECATED mDimensions "Use generic-lens or generic-optics with 'dimensions' instead." #-}

instance Lude.FromXML Metric where
  parseXML x =
    Metric'
      Lude.<$> (x Lude..@? "MetricName")
      Lude.<*> (x Lude..@? "Namespace")
      Lude.<*> ( x Lude..@? "Dimensions" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )

instance Lude.ToQuery Metric where
  toQuery Metric' {..} =
    Lude.mconcat
      [ "MetricName" Lude.=: metricName,
        "Namespace" Lude.=: namespace,
        "Dimensions"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> dimensions)
      ]
