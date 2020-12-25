{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.OrganizationConformancePack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.OrganizationConformancePack
  ( OrganizationConformancePack (..),

    -- * Smart constructor
    mkOrganizationConformancePack,

    -- * Lenses
    ocpOrganizationConformancePackName,
    ocpOrganizationConformancePackArn,
    ocpLastUpdateTime,
    ocpConformancePackInputParameters,
    ocpDeliveryS3Bucket,
    ocpDeliveryS3KeyPrefix,
    ocpExcludedAccounts,
  )
where

import qualified Network.AWS.Config.Types.AccountId as Types
import qualified Network.AWS.Config.Types.ConformancePackInputParameter as Types
import qualified Network.AWS.Config.Types.DeliveryS3Bucket as Types
import qualified Network.AWS.Config.Types.DeliveryS3KeyPrefix as Types
import qualified Network.AWS.Config.Types.OrganizationConformancePackName as Types
import qualified Network.AWS.Config.Types.StringWithCharLimit256 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An organization conformance pack that has information about conformance packs that AWS Config creates in member accounts.
--
-- /See:/ 'mkOrganizationConformancePack' smart constructor.
data OrganizationConformancePack = OrganizationConformancePack'
  { -- | The name you assign to an organization conformance pack.
    organizationConformancePackName :: Types.OrganizationConformancePackName,
    -- | Amazon Resource Name (ARN) of organization conformance pack.
    organizationConformancePackArn :: Types.StringWithCharLimit256,
    -- | Last time when organization conformation pack was updated.
    lastUpdateTime :: Core.NominalDiffTime,
    -- | A list of @ConformancePackInputParameter@ objects.
    conformancePackInputParameters :: Core.Maybe [Types.ConformancePackInputParameter],
    -- | Location of an Amazon S3 bucket where AWS Config can deliver evaluation results and conformance pack template that is used to create a pack.
    deliveryS3Bucket :: Core.Maybe Types.DeliveryS3Bucket,
    -- | Any folder structure you want to add to an Amazon S3 bucket.
    deliveryS3KeyPrefix :: Core.Maybe Types.DeliveryS3KeyPrefix,
    -- | A comma-separated list of accounts excluded from organization conformance pack.
    excludedAccounts :: Core.Maybe [Types.AccountId]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'OrganizationConformancePack' value with any optional fields omitted.
mkOrganizationConformancePack ::
  -- | 'organizationConformancePackName'
  Types.OrganizationConformancePackName ->
  -- | 'organizationConformancePackArn'
  Types.StringWithCharLimit256 ->
  -- | 'lastUpdateTime'
  Core.NominalDiffTime ->
  OrganizationConformancePack
mkOrganizationConformancePack
  organizationConformancePackName
  organizationConformancePackArn
  lastUpdateTime =
    OrganizationConformancePack'
      { organizationConformancePackName,
        organizationConformancePackArn,
        lastUpdateTime,
        conformancePackInputParameters = Core.Nothing,
        deliveryS3Bucket = Core.Nothing,
        deliveryS3KeyPrefix = Core.Nothing,
        excludedAccounts = Core.Nothing
      }

-- | The name you assign to an organization conformance pack.
--
-- /Note:/ Consider using 'organizationConformancePackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocpOrganizationConformancePackName :: Lens.Lens' OrganizationConformancePack Types.OrganizationConformancePackName
ocpOrganizationConformancePackName = Lens.field @"organizationConformancePackName"
{-# DEPRECATED ocpOrganizationConformancePackName "Use generic-lens or generic-optics with 'organizationConformancePackName' instead." #-}

-- | Amazon Resource Name (ARN) of organization conformance pack.
--
-- /Note:/ Consider using 'organizationConformancePackArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocpOrganizationConformancePackArn :: Lens.Lens' OrganizationConformancePack Types.StringWithCharLimit256
ocpOrganizationConformancePackArn = Lens.field @"organizationConformancePackArn"
{-# DEPRECATED ocpOrganizationConformancePackArn "Use generic-lens or generic-optics with 'organizationConformancePackArn' instead." #-}

-- | Last time when organization conformation pack was updated.
--
-- /Note:/ Consider using 'lastUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocpLastUpdateTime :: Lens.Lens' OrganizationConformancePack Core.NominalDiffTime
ocpLastUpdateTime = Lens.field @"lastUpdateTime"
{-# DEPRECATED ocpLastUpdateTime "Use generic-lens or generic-optics with 'lastUpdateTime' instead." #-}

-- | A list of @ConformancePackInputParameter@ objects.
--
-- /Note:/ Consider using 'conformancePackInputParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocpConformancePackInputParameters :: Lens.Lens' OrganizationConformancePack (Core.Maybe [Types.ConformancePackInputParameter])
ocpConformancePackInputParameters = Lens.field @"conformancePackInputParameters"
{-# DEPRECATED ocpConformancePackInputParameters "Use generic-lens or generic-optics with 'conformancePackInputParameters' instead." #-}

-- | Location of an Amazon S3 bucket where AWS Config can deliver evaluation results and conformance pack template that is used to create a pack.
--
-- /Note:/ Consider using 'deliveryS3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocpDeliveryS3Bucket :: Lens.Lens' OrganizationConformancePack (Core.Maybe Types.DeliveryS3Bucket)
ocpDeliveryS3Bucket = Lens.field @"deliveryS3Bucket"
{-# DEPRECATED ocpDeliveryS3Bucket "Use generic-lens or generic-optics with 'deliveryS3Bucket' instead." #-}

-- | Any folder structure you want to add to an Amazon S3 bucket.
--
-- /Note:/ Consider using 'deliveryS3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocpDeliveryS3KeyPrefix :: Lens.Lens' OrganizationConformancePack (Core.Maybe Types.DeliveryS3KeyPrefix)
ocpDeliveryS3KeyPrefix = Lens.field @"deliveryS3KeyPrefix"
{-# DEPRECATED ocpDeliveryS3KeyPrefix "Use generic-lens or generic-optics with 'deliveryS3KeyPrefix' instead." #-}

-- | A comma-separated list of accounts excluded from organization conformance pack.
--
-- /Note:/ Consider using 'excludedAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocpExcludedAccounts :: Lens.Lens' OrganizationConformancePack (Core.Maybe [Types.AccountId])
ocpExcludedAccounts = Lens.field @"excludedAccounts"
{-# DEPRECATED ocpExcludedAccounts "Use generic-lens or generic-optics with 'excludedAccounts' instead." #-}

instance Core.FromJSON OrganizationConformancePack where
  parseJSON =
    Core.withObject "OrganizationConformancePack" Core.$
      \x ->
        OrganizationConformancePack'
          Core.<$> (x Core..: "OrganizationConformancePackName")
          Core.<*> (x Core..: "OrganizationConformancePackArn")
          Core.<*> (x Core..: "LastUpdateTime")
          Core.<*> (x Core..:? "ConformancePackInputParameters")
          Core.<*> (x Core..:? "DeliveryS3Bucket")
          Core.<*> (x Core..:? "DeliveryS3KeyPrefix")
          Core.<*> (x Core..:? "ExcludedAccounts")
