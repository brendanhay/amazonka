{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.MetricsConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.MetricsConfiguration
  ( MetricsConfiguration (..),

    -- * Smart constructor
    mkMetricsConfiguration,

    -- * Lenses
    mcFilter,
    mcId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.MetricsFilter

-- | Specifies a metrics configuration for the CloudWatch request metrics (specified by the metrics configuration ID) from an Amazon S3 bucket. If you're updating an existing metrics configuration, note that this is a full replacement of the existing metrics configuration. If you don't include the elements you want to keep, they are erased. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTMetricConfiguration.html PUT Bucket metrics> in the /Amazon Simple Storage Service API Reference/ .
--
-- /See:/ 'mkMetricsConfiguration' smart constructor.
data MetricsConfiguration = MetricsConfiguration'
  { filter ::
      Lude.Maybe MetricsFilter,
    id :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MetricsConfiguration' with the minimum fields required to make a request.
--
-- * 'filter' - Specifies a metrics configuration filter. The metrics configuration will only include objects that meet the filter's criteria. A filter must be a prefix, a tag, or a conjunction (MetricsAndOperator).
-- * 'id' - The ID used to identify the metrics configuration.
mkMetricsConfiguration ::
  -- | 'id'
  Lude.Text ->
  MetricsConfiguration
mkMetricsConfiguration pId_ =
  MetricsConfiguration' {filter = Lude.Nothing, id = pId_}

-- | Specifies a metrics configuration filter. The metrics configuration will only include objects that meet the filter's criteria. A filter must be a prefix, a tag, or a conjunction (MetricsAndOperator).
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcFilter :: Lens.Lens' MetricsConfiguration (Lude.Maybe MetricsFilter)
mcFilter = Lens.lens (filter :: MetricsConfiguration -> Lude.Maybe MetricsFilter) (\s a -> s {filter = a} :: MetricsConfiguration)
{-# DEPRECATED mcFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The ID used to identify the metrics configuration.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcId :: Lens.Lens' MetricsConfiguration Lude.Text
mcId = Lens.lens (id :: MetricsConfiguration -> Lude.Text) (\s a -> s {id = a} :: MetricsConfiguration)
{-# DEPRECATED mcId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromXML MetricsConfiguration where
  parseXML x =
    MetricsConfiguration'
      Lude.<$> (x Lude..@? "Filter") Lude.<*> (x Lude..@ "Id")

instance Lude.ToXML MetricsConfiguration where
  toXML MetricsConfiguration' {..} =
    Lude.mconcat ["Filter" Lude.@= filter, "Id" Lude.@= id]
