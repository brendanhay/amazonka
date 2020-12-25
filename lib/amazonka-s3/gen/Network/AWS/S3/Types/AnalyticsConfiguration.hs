{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.AnalyticsConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.AnalyticsConfiguration
  ( AnalyticsConfiguration (..),

    -- * Smart constructor
    mkAnalyticsConfiguration,

    -- * Lenses
    acId,
    acStorageClassAnalysis,
    acFilter,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.AnalyticsFilter as Types
import qualified Network.AWS.S3.Types.AnalyticsId as Types
import qualified Network.AWS.S3.Types.StorageClassAnalysis as Types

-- | Specifies the configuration and any analyses for the analytics filter of an Amazon S3 bucket.
--
-- /See:/ 'mkAnalyticsConfiguration' smart constructor.
data AnalyticsConfiguration = AnalyticsConfiguration'
  { -- | The ID that identifies the analytics configuration.
    id :: Types.AnalyticsId,
    -- | Contains data related to access patterns to be collected and made available to analyze the tradeoffs between different storage classes.
    storageClassAnalysis :: Types.StorageClassAnalysis,
    -- | The filter used to describe a set of objects for analyses. A filter must have exactly one prefix, one tag, or one conjunction (AnalyticsAndOperator). If no filter is provided, all objects will be considered in any analysis.
    filter :: Core.Maybe Types.AnalyticsFilter
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AnalyticsConfiguration' value with any optional fields omitted.
mkAnalyticsConfiguration ::
  -- | 'id'
  Types.AnalyticsId ->
  -- | 'storageClassAnalysis'
  Types.StorageClassAnalysis ->
  AnalyticsConfiguration
mkAnalyticsConfiguration id storageClassAnalysis =
  AnalyticsConfiguration'
    { id,
      storageClassAnalysis,
      filter = Core.Nothing
    }

-- | The ID that identifies the analytics configuration.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acId :: Lens.Lens' AnalyticsConfiguration Types.AnalyticsId
acId = Lens.field @"id"
{-# DEPRECATED acId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Contains data related to access patterns to be collected and made available to analyze the tradeoffs between different storage classes.
--
-- /Note:/ Consider using 'storageClassAnalysis' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acStorageClassAnalysis :: Lens.Lens' AnalyticsConfiguration Types.StorageClassAnalysis
acStorageClassAnalysis = Lens.field @"storageClassAnalysis"
{-# DEPRECATED acStorageClassAnalysis "Use generic-lens or generic-optics with 'storageClassAnalysis' instead." #-}

-- | The filter used to describe a set of objects for analyses. A filter must have exactly one prefix, one tag, or one conjunction (AnalyticsAndOperator). If no filter is provided, all objects will be considered in any analysis.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acFilter :: Lens.Lens' AnalyticsConfiguration (Core.Maybe Types.AnalyticsFilter)
acFilter = Lens.field @"filter"
{-# DEPRECATED acFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

instance Core.ToXML AnalyticsConfiguration where
  toXML AnalyticsConfiguration {..} =
    Core.toXMLNode "Id" id
      Core.<> Core.toXMLNode "StorageClassAnalysis" storageClassAnalysis
      Core.<> Core.toXMLNode "Filter" Core.<$> filter

instance Core.FromXML AnalyticsConfiguration where
  parseXML x =
    AnalyticsConfiguration'
      Core.<$> (x Core..@ "Id")
      Core.<*> (x Core..@ "StorageClassAnalysis")
      Core.<*> (x Core..@? "Filter")
