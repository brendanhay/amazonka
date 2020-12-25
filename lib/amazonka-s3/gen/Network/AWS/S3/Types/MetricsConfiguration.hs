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
    mcId,
    mcFilter,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.Id as Types
import qualified Network.AWS.S3.Types.MetricsFilter as Types

-- | Specifies a metrics configuration for the CloudWatch request metrics (specified by the metrics configuration ID) from an Amazon S3 bucket. If you're updating an existing metrics configuration, note that this is a full replacement of the existing metrics configuration. If you don't include the elements you want to keep, they are erased. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTMetricConfiguration.html PUT Bucket metrics> in the /Amazon Simple Storage Service API Reference/ .
--
-- /See:/ 'mkMetricsConfiguration' smart constructor.
data MetricsConfiguration = MetricsConfiguration'
  { -- | The ID used to identify the metrics configuration.
    id :: Types.Id,
    -- | Specifies a metrics configuration filter. The metrics configuration will only include objects that meet the filter's criteria. A filter must be a prefix, a tag, or a conjunction (MetricsAndOperator).
    filter :: Core.Maybe Types.MetricsFilter
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MetricsConfiguration' value with any optional fields omitted.
mkMetricsConfiguration ::
  -- | 'id'
  Types.Id ->
  MetricsConfiguration
mkMetricsConfiguration id =
  MetricsConfiguration' {id, filter = Core.Nothing}

-- | The ID used to identify the metrics configuration.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcId :: Lens.Lens' MetricsConfiguration Types.Id
mcId = Lens.field @"id"
{-# DEPRECATED mcId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Specifies a metrics configuration filter. The metrics configuration will only include objects that meet the filter's criteria. A filter must be a prefix, a tag, or a conjunction (MetricsAndOperator).
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcFilter :: Lens.Lens' MetricsConfiguration (Core.Maybe Types.MetricsFilter)
mcFilter = Lens.field @"filter"
{-# DEPRECATED mcFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

instance Core.ToXML MetricsConfiguration where
  toXML MetricsConfiguration {..} =
    Core.toXMLNode "Id" id
      Core.<> Core.toXMLNode "Filter" Core.<$> filter

instance Core.FromXML MetricsConfiguration where
  parseXML x =
    MetricsConfiguration'
      Core.<$> (x Core..@ "Id") Core.<*> (x Core..@? "Filter")
