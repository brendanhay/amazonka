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
    suEngineVersion,
    suServiceUpdateType,
    suServiceUpdateName,
    suEngine,
    suServiceUpdateReleaseDate,
    suAutoUpdateAfterRecommendedApplyByDate,
    suServiceUpdateSeverity,
    suServiceUpdateEndDate,
    suServiceUpdateDescription,
    suServiceUpdateRecommendedApplyByDate,
    suServiceUpdateStatus,
    suEstimatedUpdateTime,
  )
where

import Network.AWS.ElastiCache.Types.ServiceUpdateSeverity
import Network.AWS.ElastiCache.Types.ServiceUpdateStatus
import Network.AWS.ElastiCache.Types.ServiceUpdateType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An update that you can apply to your Redis clusters.
--
-- /See:/ 'mkServiceUpdate' smart constructor.
data ServiceUpdate = ServiceUpdate'
  { engineVersion ::
      Lude.Maybe Lude.Text,
    serviceUpdateType :: Lude.Maybe ServiceUpdateType,
    serviceUpdateName :: Lude.Maybe Lude.Text,
    engine :: Lude.Maybe Lude.Text,
    serviceUpdateReleaseDate :: Lude.Maybe Lude.ISO8601,
    autoUpdateAfterRecommendedApplyByDate :: Lude.Maybe Lude.Bool,
    serviceUpdateSeverity :: Lude.Maybe ServiceUpdateSeverity,
    serviceUpdateEndDate :: Lude.Maybe Lude.ISO8601,
    serviceUpdateDescription :: Lude.Maybe Lude.Text,
    serviceUpdateRecommendedApplyByDate :: Lude.Maybe Lude.ISO8601,
    serviceUpdateStatus :: Lude.Maybe ServiceUpdateStatus,
    estimatedUpdateTime :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ServiceUpdate' with the minimum fields required to make a request.
--
-- * 'autoUpdateAfterRecommendedApplyByDate' - Indicates whether the service update will be automatically applied once the recommended apply-by date has expired.
-- * 'engine' - The Elasticache engine to which the update applies. Either Redis or Memcached
-- * 'engineVersion' - The Elasticache engine version to which the update applies. Either Redis or Memcached engine version
-- * 'estimatedUpdateTime' - The estimated length of time the service update will take
-- * 'serviceUpdateDescription' - Provides details of the service update
-- * 'serviceUpdateEndDate' - The date after which the service update is no longer available
-- * 'serviceUpdateName' - The unique ID of the service update
-- * 'serviceUpdateRecommendedApplyByDate' - The recommendend date to apply the service update in order to ensure compliance. For information on compliance, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/elasticache-compliance.html#elasticache-compliance-self-service Self-Service Security Updates for Compliance> .
-- * 'serviceUpdateReleaseDate' - The date when the service update is initially available
-- * 'serviceUpdateSeverity' - The severity of the service update
-- * 'serviceUpdateStatus' - The status of the service update
-- * 'serviceUpdateType' - Reflects the nature of the service update
mkServiceUpdate ::
  ServiceUpdate
mkServiceUpdate =
  ServiceUpdate'
    { engineVersion = Lude.Nothing,
      serviceUpdateType = Lude.Nothing,
      serviceUpdateName = Lude.Nothing,
      engine = Lude.Nothing,
      serviceUpdateReleaseDate = Lude.Nothing,
      autoUpdateAfterRecommendedApplyByDate = Lude.Nothing,
      serviceUpdateSeverity = Lude.Nothing,
      serviceUpdateEndDate = Lude.Nothing,
      serviceUpdateDescription = Lude.Nothing,
      serviceUpdateRecommendedApplyByDate = Lude.Nothing,
      serviceUpdateStatus = Lude.Nothing,
      estimatedUpdateTime = Lude.Nothing
    }

-- | The Elasticache engine version to which the update applies. Either Redis or Memcached engine version
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suEngineVersion :: Lens.Lens' ServiceUpdate (Lude.Maybe Lude.Text)
suEngineVersion = Lens.lens (engineVersion :: ServiceUpdate -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: ServiceUpdate)
{-# DEPRECATED suEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | Reflects the nature of the service update
--
-- /Note:/ Consider using 'serviceUpdateType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suServiceUpdateType :: Lens.Lens' ServiceUpdate (Lude.Maybe ServiceUpdateType)
suServiceUpdateType = Lens.lens (serviceUpdateType :: ServiceUpdate -> Lude.Maybe ServiceUpdateType) (\s a -> s {serviceUpdateType = a} :: ServiceUpdate)
{-# DEPRECATED suServiceUpdateType "Use generic-lens or generic-optics with 'serviceUpdateType' instead." #-}

-- | The unique ID of the service update
--
-- /Note:/ Consider using 'serviceUpdateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suServiceUpdateName :: Lens.Lens' ServiceUpdate (Lude.Maybe Lude.Text)
suServiceUpdateName = Lens.lens (serviceUpdateName :: ServiceUpdate -> Lude.Maybe Lude.Text) (\s a -> s {serviceUpdateName = a} :: ServiceUpdate)
{-# DEPRECATED suServiceUpdateName "Use generic-lens or generic-optics with 'serviceUpdateName' instead." #-}

-- | The Elasticache engine to which the update applies. Either Redis or Memcached
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suEngine :: Lens.Lens' ServiceUpdate (Lude.Maybe Lude.Text)
suEngine = Lens.lens (engine :: ServiceUpdate -> Lude.Maybe Lude.Text) (\s a -> s {engine = a} :: ServiceUpdate)
{-# DEPRECATED suEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The date when the service update is initially available
--
-- /Note:/ Consider using 'serviceUpdateReleaseDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suServiceUpdateReleaseDate :: Lens.Lens' ServiceUpdate (Lude.Maybe Lude.ISO8601)
suServiceUpdateReleaseDate = Lens.lens (serviceUpdateReleaseDate :: ServiceUpdate -> Lude.Maybe Lude.ISO8601) (\s a -> s {serviceUpdateReleaseDate = a} :: ServiceUpdate)
{-# DEPRECATED suServiceUpdateReleaseDate "Use generic-lens or generic-optics with 'serviceUpdateReleaseDate' instead." #-}

-- | Indicates whether the service update will be automatically applied once the recommended apply-by date has expired.
--
-- /Note:/ Consider using 'autoUpdateAfterRecommendedApplyByDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suAutoUpdateAfterRecommendedApplyByDate :: Lens.Lens' ServiceUpdate (Lude.Maybe Lude.Bool)
suAutoUpdateAfterRecommendedApplyByDate = Lens.lens (autoUpdateAfterRecommendedApplyByDate :: ServiceUpdate -> Lude.Maybe Lude.Bool) (\s a -> s {autoUpdateAfterRecommendedApplyByDate = a} :: ServiceUpdate)
{-# DEPRECATED suAutoUpdateAfterRecommendedApplyByDate "Use generic-lens or generic-optics with 'autoUpdateAfterRecommendedApplyByDate' instead." #-}

-- | The severity of the service update
--
-- /Note:/ Consider using 'serviceUpdateSeverity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suServiceUpdateSeverity :: Lens.Lens' ServiceUpdate (Lude.Maybe ServiceUpdateSeverity)
suServiceUpdateSeverity = Lens.lens (serviceUpdateSeverity :: ServiceUpdate -> Lude.Maybe ServiceUpdateSeverity) (\s a -> s {serviceUpdateSeverity = a} :: ServiceUpdate)
{-# DEPRECATED suServiceUpdateSeverity "Use generic-lens or generic-optics with 'serviceUpdateSeverity' instead." #-}

-- | The date after which the service update is no longer available
--
-- /Note:/ Consider using 'serviceUpdateEndDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suServiceUpdateEndDate :: Lens.Lens' ServiceUpdate (Lude.Maybe Lude.ISO8601)
suServiceUpdateEndDate = Lens.lens (serviceUpdateEndDate :: ServiceUpdate -> Lude.Maybe Lude.ISO8601) (\s a -> s {serviceUpdateEndDate = a} :: ServiceUpdate)
{-# DEPRECATED suServiceUpdateEndDate "Use generic-lens or generic-optics with 'serviceUpdateEndDate' instead." #-}

-- | Provides details of the service update
--
-- /Note:/ Consider using 'serviceUpdateDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suServiceUpdateDescription :: Lens.Lens' ServiceUpdate (Lude.Maybe Lude.Text)
suServiceUpdateDescription = Lens.lens (serviceUpdateDescription :: ServiceUpdate -> Lude.Maybe Lude.Text) (\s a -> s {serviceUpdateDescription = a} :: ServiceUpdate)
{-# DEPRECATED suServiceUpdateDescription "Use generic-lens or generic-optics with 'serviceUpdateDescription' instead." #-}

-- | The recommendend date to apply the service update in order to ensure compliance. For information on compliance, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/elasticache-compliance.html#elasticache-compliance-self-service Self-Service Security Updates for Compliance> .
--
-- /Note:/ Consider using 'serviceUpdateRecommendedApplyByDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suServiceUpdateRecommendedApplyByDate :: Lens.Lens' ServiceUpdate (Lude.Maybe Lude.ISO8601)
suServiceUpdateRecommendedApplyByDate = Lens.lens (serviceUpdateRecommendedApplyByDate :: ServiceUpdate -> Lude.Maybe Lude.ISO8601) (\s a -> s {serviceUpdateRecommendedApplyByDate = a} :: ServiceUpdate)
{-# DEPRECATED suServiceUpdateRecommendedApplyByDate "Use generic-lens or generic-optics with 'serviceUpdateRecommendedApplyByDate' instead." #-}

-- | The status of the service update
--
-- /Note:/ Consider using 'serviceUpdateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suServiceUpdateStatus :: Lens.Lens' ServiceUpdate (Lude.Maybe ServiceUpdateStatus)
suServiceUpdateStatus = Lens.lens (serviceUpdateStatus :: ServiceUpdate -> Lude.Maybe ServiceUpdateStatus) (\s a -> s {serviceUpdateStatus = a} :: ServiceUpdate)
{-# DEPRECATED suServiceUpdateStatus "Use generic-lens or generic-optics with 'serviceUpdateStatus' instead." #-}

-- | The estimated length of time the service update will take
--
-- /Note:/ Consider using 'estimatedUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suEstimatedUpdateTime :: Lens.Lens' ServiceUpdate (Lude.Maybe Lude.Text)
suEstimatedUpdateTime = Lens.lens (estimatedUpdateTime :: ServiceUpdate -> Lude.Maybe Lude.Text) (\s a -> s {estimatedUpdateTime = a} :: ServiceUpdate)
{-# DEPRECATED suEstimatedUpdateTime "Use generic-lens or generic-optics with 'estimatedUpdateTime' instead." #-}

instance Lude.FromXML ServiceUpdate where
  parseXML x =
    ServiceUpdate'
      Lude.<$> (x Lude..@? "EngineVersion")
      Lude.<*> (x Lude..@? "ServiceUpdateType")
      Lude.<*> (x Lude..@? "ServiceUpdateName")
      Lude.<*> (x Lude..@? "Engine")
      Lude.<*> (x Lude..@? "ServiceUpdateReleaseDate")
      Lude.<*> (x Lude..@? "AutoUpdateAfterRecommendedApplyByDate")
      Lude.<*> (x Lude..@? "ServiceUpdateSeverity")
      Lude.<*> (x Lude..@? "ServiceUpdateEndDate")
      Lude.<*> (x Lude..@? "ServiceUpdateDescription")
      Lude.<*> (x Lude..@? "ServiceUpdateRecommendedApplyByDate")
      Lude.<*> (x Lude..@? "ServiceUpdateStatus")
      Lude.<*> (x Lude..@? "EstimatedUpdateTime")
