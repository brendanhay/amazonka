{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.ServiceUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.ServiceUpdate
  ( ServiceUpdate (..),

    -- * Smart constructor
    mkServiceUpdate,

    -- * Lenses
    suAutoUpdateAfterRecommendedApplyByDate,
    suEngine,
    suEngineVersion,
    suEstimatedUpdateTime,
    suServiceUpdateDescription,
    suServiceUpdateEndDate,
    suServiceUpdateName,
    suServiceUpdateRecommendedApplyByDate,
    suServiceUpdateReleaseDate,
    suServiceUpdateSeverity,
    suServiceUpdateStatus,
    suServiceUpdateType,
  )
where

import qualified Network.AWS.ElastiCache.Types.ServiceUpdateSeverity as Types
import qualified Network.AWS.ElastiCache.Types.ServiceUpdateStatus as Types
import qualified Network.AWS.ElastiCache.Types.ServiceUpdateType as Types
import qualified Network.AWS.ElastiCache.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An update that you can apply to your Redis clusters.
--
-- /See:/ 'mkServiceUpdate' smart constructor.
data ServiceUpdate = ServiceUpdate'
  { -- | Indicates whether the service update will be automatically applied once the recommended apply-by date has expired.
    autoUpdateAfterRecommendedApplyByDate :: Core.Maybe Core.Bool,
    -- | The Elasticache engine to which the update applies. Either Redis or Memcached
    engine :: Core.Maybe Types.String,
    -- | The Elasticache engine version to which the update applies. Either Redis or Memcached engine version
    engineVersion :: Core.Maybe Types.String,
    -- | The estimated length of time the service update will take
    estimatedUpdateTime :: Core.Maybe Types.String,
    -- | Provides details of the service update
    serviceUpdateDescription :: Core.Maybe Types.String,
    -- | The date after which the service update is no longer available
    serviceUpdateEndDate :: Core.Maybe Core.UTCTime,
    -- | The unique ID of the service update
    serviceUpdateName :: Core.Maybe Types.String,
    -- | The recommendend date to apply the service update in order to ensure compliance. For information on compliance, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/elasticache-compliance.html#elasticache-compliance-self-service Self-Service Security Updates for Compliance> .
    serviceUpdateRecommendedApplyByDate :: Core.Maybe Core.UTCTime,
    -- | The date when the service update is initially available
    serviceUpdateReleaseDate :: Core.Maybe Core.UTCTime,
    -- | The severity of the service update
    serviceUpdateSeverity :: Core.Maybe Types.ServiceUpdateSeverity,
    -- | The status of the service update
    serviceUpdateStatus :: Core.Maybe Types.ServiceUpdateStatus,
    -- | Reflects the nature of the service update
    serviceUpdateType :: Core.Maybe Types.ServiceUpdateType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ServiceUpdate' value with any optional fields omitted.
mkServiceUpdate ::
  ServiceUpdate
mkServiceUpdate =
  ServiceUpdate'
    { autoUpdateAfterRecommendedApplyByDate =
        Core.Nothing,
      engine = Core.Nothing,
      engineVersion = Core.Nothing,
      estimatedUpdateTime = Core.Nothing,
      serviceUpdateDescription = Core.Nothing,
      serviceUpdateEndDate = Core.Nothing,
      serviceUpdateName = Core.Nothing,
      serviceUpdateRecommendedApplyByDate = Core.Nothing,
      serviceUpdateReleaseDate = Core.Nothing,
      serviceUpdateSeverity = Core.Nothing,
      serviceUpdateStatus = Core.Nothing,
      serviceUpdateType = Core.Nothing
    }

-- | Indicates whether the service update will be automatically applied once the recommended apply-by date has expired.
--
-- /Note:/ Consider using 'autoUpdateAfterRecommendedApplyByDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suAutoUpdateAfterRecommendedApplyByDate :: Lens.Lens' ServiceUpdate (Core.Maybe Core.Bool)
suAutoUpdateAfterRecommendedApplyByDate = Lens.field @"autoUpdateAfterRecommendedApplyByDate"
{-# DEPRECATED suAutoUpdateAfterRecommendedApplyByDate "Use generic-lens or generic-optics with 'autoUpdateAfterRecommendedApplyByDate' instead." #-}

-- | The Elasticache engine to which the update applies. Either Redis or Memcached
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suEngine :: Lens.Lens' ServiceUpdate (Core.Maybe Types.String)
suEngine = Lens.field @"engine"
{-# DEPRECATED suEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The Elasticache engine version to which the update applies. Either Redis or Memcached engine version
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suEngineVersion :: Lens.Lens' ServiceUpdate (Core.Maybe Types.String)
suEngineVersion = Lens.field @"engineVersion"
{-# DEPRECATED suEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | The estimated length of time the service update will take
--
-- /Note:/ Consider using 'estimatedUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suEstimatedUpdateTime :: Lens.Lens' ServiceUpdate (Core.Maybe Types.String)
suEstimatedUpdateTime = Lens.field @"estimatedUpdateTime"
{-# DEPRECATED suEstimatedUpdateTime "Use generic-lens or generic-optics with 'estimatedUpdateTime' instead." #-}

-- | Provides details of the service update
--
-- /Note:/ Consider using 'serviceUpdateDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suServiceUpdateDescription :: Lens.Lens' ServiceUpdate (Core.Maybe Types.String)
suServiceUpdateDescription = Lens.field @"serviceUpdateDescription"
{-# DEPRECATED suServiceUpdateDescription "Use generic-lens or generic-optics with 'serviceUpdateDescription' instead." #-}

-- | The date after which the service update is no longer available
--
-- /Note:/ Consider using 'serviceUpdateEndDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suServiceUpdateEndDate :: Lens.Lens' ServiceUpdate (Core.Maybe Core.UTCTime)
suServiceUpdateEndDate = Lens.field @"serviceUpdateEndDate"
{-# DEPRECATED suServiceUpdateEndDate "Use generic-lens or generic-optics with 'serviceUpdateEndDate' instead." #-}

-- | The unique ID of the service update
--
-- /Note:/ Consider using 'serviceUpdateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suServiceUpdateName :: Lens.Lens' ServiceUpdate (Core.Maybe Types.String)
suServiceUpdateName = Lens.field @"serviceUpdateName"
{-# DEPRECATED suServiceUpdateName "Use generic-lens or generic-optics with 'serviceUpdateName' instead." #-}

-- | The recommendend date to apply the service update in order to ensure compliance. For information on compliance, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/elasticache-compliance.html#elasticache-compliance-self-service Self-Service Security Updates for Compliance> .
--
-- /Note:/ Consider using 'serviceUpdateRecommendedApplyByDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suServiceUpdateRecommendedApplyByDate :: Lens.Lens' ServiceUpdate (Core.Maybe Core.UTCTime)
suServiceUpdateRecommendedApplyByDate = Lens.field @"serviceUpdateRecommendedApplyByDate"
{-# DEPRECATED suServiceUpdateRecommendedApplyByDate "Use generic-lens or generic-optics with 'serviceUpdateRecommendedApplyByDate' instead." #-}

-- | The date when the service update is initially available
--
-- /Note:/ Consider using 'serviceUpdateReleaseDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suServiceUpdateReleaseDate :: Lens.Lens' ServiceUpdate (Core.Maybe Core.UTCTime)
suServiceUpdateReleaseDate = Lens.field @"serviceUpdateReleaseDate"
{-# DEPRECATED suServiceUpdateReleaseDate "Use generic-lens or generic-optics with 'serviceUpdateReleaseDate' instead." #-}

-- | The severity of the service update
--
-- /Note:/ Consider using 'serviceUpdateSeverity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suServiceUpdateSeverity :: Lens.Lens' ServiceUpdate (Core.Maybe Types.ServiceUpdateSeverity)
suServiceUpdateSeverity = Lens.field @"serviceUpdateSeverity"
{-# DEPRECATED suServiceUpdateSeverity "Use generic-lens or generic-optics with 'serviceUpdateSeverity' instead." #-}

-- | The status of the service update
--
-- /Note:/ Consider using 'serviceUpdateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suServiceUpdateStatus :: Lens.Lens' ServiceUpdate (Core.Maybe Types.ServiceUpdateStatus)
suServiceUpdateStatus = Lens.field @"serviceUpdateStatus"
{-# DEPRECATED suServiceUpdateStatus "Use generic-lens or generic-optics with 'serviceUpdateStatus' instead." #-}

-- | Reflects the nature of the service update
--
-- /Note:/ Consider using 'serviceUpdateType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suServiceUpdateType :: Lens.Lens' ServiceUpdate (Core.Maybe Types.ServiceUpdateType)
suServiceUpdateType = Lens.field @"serviceUpdateType"
{-# DEPRECATED suServiceUpdateType "Use generic-lens or generic-optics with 'serviceUpdateType' instead." #-}

instance Core.FromXML ServiceUpdate where
  parseXML x =
    ServiceUpdate'
      Core.<$> (x Core..@? "AutoUpdateAfterRecommendedApplyByDate")
      Core.<*> (x Core..@? "Engine")
      Core.<*> (x Core..@? "EngineVersion")
      Core.<*> (x Core..@? "EstimatedUpdateTime")
      Core.<*> (x Core..@? "ServiceUpdateDescription")
      Core.<*> (x Core..@? "ServiceUpdateEndDate")
      Core.<*> (x Core..@? "ServiceUpdateName")
      Core.<*> (x Core..@? "ServiceUpdateRecommendedApplyByDate")
      Core.<*> (x Core..@? "ServiceUpdateReleaseDate")
      Core.<*> (x Core..@? "ServiceUpdateSeverity")
      Core.<*> (x Core..@? "ServiceUpdateStatus")
      Core.<*> (x Core..@? "ServiceUpdateType")
