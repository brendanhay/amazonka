{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanDetails
  ( ProvisionedProductPlanDetails (..)
  -- * Smart constructor
  , mkProvisionedProductPlanDetails
  -- * Lenses
  , pppdCreatedTime
  , pppdNotificationArns
  , pppdPathId
  , pppdPlanId
  , pppdPlanName
  , pppdPlanType
  , pppdProductId
  , pppdProvisionProductId
  , pppdProvisionProductName
  , pppdProvisioningArtifactId
  , pppdProvisioningParameters
  , pppdStatus
  , pppdStatusMessage
  , pppdTags
  , pppdUpdatedTime
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.NotificationArn as Types
import qualified Network.AWS.ServiceCatalog.Types.PathId as Types
import qualified Network.AWS.ServiceCatalog.Types.PlanId as Types
import qualified Network.AWS.ServiceCatalog.Types.ProductId as Types
import qualified Network.AWS.ServiceCatalog.Types.ProvisionProductId as Types
import qualified Network.AWS.ServiceCatalog.Types.ProvisionedProductName as Types
import qualified Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanName as Types
import qualified Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanStatus as Types
import qualified Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanType as Types
import qualified Network.AWS.ServiceCatalog.Types.ProvisioningArtifactId as Types
import qualified Network.AWS.ServiceCatalog.Types.StatusMessage as Types
import qualified Network.AWS.ServiceCatalog.Types.Tag as Types
import qualified Network.AWS.ServiceCatalog.Types.UpdateProvisioningParameter as Types

-- | Information about a plan.
--
-- /See:/ 'mkProvisionedProductPlanDetails' smart constructor.
data ProvisionedProductPlanDetails = ProvisionedProductPlanDetails'
  { createdTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The UTC time stamp of the creation time.
  , notificationArns :: Core.Maybe [Types.NotificationArn]
    -- ^ Passed to CloudFormation. The SNS topic ARNs to which to publish stack-related events.
  , pathId :: Core.Maybe Types.PathId
    -- ^ The path identifier of the product. This value is optional if the product has a default path, and required if the product has more than one path. To list the paths for a product, use 'ListLaunchPaths' .
  , planId :: Core.Maybe Types.PlanId
    -- ^ The plan identifier.
  , planName :: Core.Maybe Types.ProvisionedProductPlanName
    -- ^ The name of the plan.
  , planType :: Core.Maybe Types.ProvisionedProductPlanType
    -- ^ The plan type.
  , productId :: Core.Maybe Types.ProductId
    -- ^ The product identifier.
  , provisionProductId :: Core.Maybe Types.ProvisionProductId
    -- ^ The product identifier.
  , provisionProductName :: Core.Maybe Types.ProvisionedProductName
    -- ^ The user-friendly name of the provisioned product.
  , provisioningArtifactId :: Core.Maybe Types.ProvisioningArtifactId
    -- ^ The identifier of the provisioning artifact.
  , provisioningParameters :: Core.Maybe [Types.UpdateProvisioningParameter]
    -- ^ Parameters specified by the administrator that are required for provisioning the product.
  , status :: Core.Maybe Types.ProvisionedProductPlanStatus
    -- ^ The status.
  , statusMessage :: Core.Maybe Types.StatusMessage
    -- ^ The status message.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ One or more tags.
  , updatedTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time when the plan was last updated.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ProvisionedProductPlanDetails' value with any optional fields omitted.
mkProvisionedProductPlanDetails
    :: ProvisionedProductPlanDetails
mkProvisionedProductPlanDetails
  = ProvisionedProductPlanDetails'{createdTime = Core.Nothing,
                                   notificationArns = Core.Nothing, pathId = Core.Nothing,
                                   planId = Core.Nothing, planName = Core.Nothing,
                                   planType = Core.Nothing, productId = Core.Nothing,
                                   provisionProductId = Core.Nothing,
                                   provisionProductName = Core.Nothing,
                                   provisioningArtifactId = Core.Nothing,
                                   provisioningParameters = Core.Nothing, status = Core.Nothing,
                                   statusMessage = Core.Nothing, tags = Core.Nothing,
                                   updatedTime = Core.Nothing}

-- | The UTC time stamp of the creation time.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pppdCreatedTime :: Lens.Lens' ProvisionedProductPlanDetails (Core.Maybe Core.NominalDiffTime)
pppdCreatedTime = Lens.field @"createdTime"
{-# INLINEABLE pppdCreatedTime #-}
{-# DEPRECATED createdTime "Use generic-lens or generic-optics with 'createdTime' instead"  #-}

-- | Passed to CloudFormation. The SNS topic ARNs to which to publish stack-related events.
--
-- /Note:/ Consider using 'notificationArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pppdNotificationArns :: Lens.Lens' ProvisionedProductPlanDetails (Core.Maybe [Types.NotificationArn])
pppdNotificationArns = Lens.field @"notificationArns"
{-# INLINEABLE pppdNotificationArns #-}
{-# DEPRECATED notificationArns "Use generic-lens or generic-optics with 'notificationArns' instead"  #-}

-- | The path identifier of the product. This value is optional if the product has a default path, and required if the product has more than one path. To list the paths for a product, use 'ListLaunchPaths' .
--
-- /Note:/ Consider using 'pathId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pppdPathId :: Lens.Lens' ProvisionedProductPlanDetails (Core.Maybe Types.PathId)
pppdPathId = Lens.field @"pathId"
{-# INLINEABLE pppdPathId #-}
{-# DEPRECATED pathId "Use generic-lens or generic-optics with 'pathId' instead"  #-}

-- | The plan identifier.
--
-- /Note:/ Consider using 'planId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pppdPlanId :: Lens.Lens' ProvisionedProductPlanDetails (Core.Maybe Types.PlanId)
pppdPlanId = Lens.field @"planId"
{-# INLINEABLE pppdPlanId #-}
{-# DEPRECATED planId "Use generic-lens or generic-optics with 'planId' instead"  #-}

-- | The name of the plan.
--
-- /Note:/ Consider using 'planName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pppdPlanName :: Lens.Lens' ProvisionedProductPlanDetails (Core.Maybe Types.ProvisionedProductPlanName)
pppdPlanName = Lens.field @"planName"
{-# INLINEABLE pppdPlanName #-}
{-# DEPRECATED planName "Use generic-lens or generic-optics with 'planName' instead"  #-}

-- | The plan type.
--
-- /Note:/ Consider using 'planType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pppdPlanType :: Lens.Lens' ProvisionedProductPlanDetails (Core.Maybe Types.ProvisionedProductPlanType)
pppdPlanType = Lens.field @"planType"
{-# INLINEABLE pppdPlanType #-}
{-# DEPRECATED planType "Use generic-lens or generic-optics with 'planType' instead"  #-}

-- | The product identifier.
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pppdProductId :: Lens.Lens' ProvisionedProductPlanDetails (Core.Maybe Types.ProductId)
pppdProductId = Lens.field @"productId"
{-# INLINEABLE pppdProductId #-}
{-# DEPRECATED productId "Use generic-lens or generic-optics with 'productId' instead"  #-}

-- | The product identifier.
--
-- /Note:/ Consider using 'provisionProductId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pppdProvisionProductId :: Lens.Lens' ProvisionedProductPlanDetails (Core.Maybe Types.ProvisionProductId)
pppdProvisionProductId = Lens.field @"provisionProductId"
{-# INLINEABLE pppdProvisionProductId #-}
{-# DEPRECATED provisionProductId "Use generic-lens or generic-optics with 'provisionProductId' instead"  #-}

-- | The user-friendly name of the provisioned product.
--
-- /Note:/ Consider using 'provisionProductName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pppdProvisionProductName :: Lens.Lens' ProvisionedProductPlanDetails (Core.Maybe Types.ProvisionedProductName)
pppdProvisionProductName = Lens.field @"provisionProductName"
{-# INLINEABLE pppdProvisionProductName #-}
{-# DEPRECATED provisionProductName "Use generic-lens or generic-optics with 'provisionProductName' instead"  #-}

-- | The identifier of the provisioning artifact.
--
-- /Note:/ Consider using 'provisioningArtifactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pppdProvisioningArtifactId :: Lens.Lens' ProvisionedProductPlanDetails (Core.Maybe Types.ProvisioningArtifactId)
pppdProvisioningArtifactId = Lens.field @"provisioningArtifactId"
{-# INLINEABLE pppdProvisioningArtifactId #-}
{-# DEPRECATED provisioningArtifactId "Use generic-lens or generic-optics with 'provisioningArtifactId' instead"  #-}

-- | Parameters specified by the administrator that are required for provisioning the product.
--
-- /Note:/ Consider using 'provisioningParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pppdProvisioningParameters :: Lens.Lens' ProvisionedProductPlanDetails (Core.Maybe [Types.UpdateProvisioningParameter])
pppdProvisioningParameters = Lens.field @"provisioningParameters"
{-# INLINEABLE pppdProvisioningParameters #-}
{-# DEPRECATED provisioningParameters "Use generic-lens or generic-optics with 'provisioningParameters' instead"  #-}

-- | The status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pppdStatus :: Lens.Lens' ProvisionedProductPlanDetails (Core.Maybe Types.ProvisionedProductPlanStatus)
pppdStatus = Lens.field @"status"
{-# INLINEABLE pppdStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The status message.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pppdStatusMessage :: Lens.Lens' ProvisionedProductPlanDetails (Core.Maybe Types.StatusMessage)
pppdStatusMessage = Lens.field @"statusMessage"
{-# INLINEABLE pppdStatusMessage #-}
{-# DEPRECATED statusMessage "Use generic-lens or generic-optics with 'statusMessage' instead"  #-}

-- | One or more tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pppdTags :: Lens.Lens' ProvisionedProductPlanDetails (Core.Maybe [Types.Tag])
pppdTags = Lens.field @"tags"
{-# INLINEABLE pppdTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The time when the plan was last updated.
--
-- /Note:/ Consider using 'updatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pppdUpdatedTime :: Lens.Lens' ProvisionedProductPlanDetails (Core.Maybe Core.NominalDiffTime)
pppdUpdatedTime = Lens.field @"updatedTime"
{-# INLINEABLE pppdUpdatedTime #-}
{-# DEPRECATED updatedTime "Use generic-lens or generic-optics with 'updatedTime' instead"  #-}

instance Core.FromJSON ProvisionedProductPlanDetails where
        parseJSON
          = Core.withObject "ProvisionedProductPlanDetails" Core.$
              \ x ->
                ProvisionedProductPlanDetails' Core.<$>
                  (x Core..:? "CreatedTime") Core.<*> x Core..:? "NotificationArns"
                    Core.<*> x Core..:? "PathId"
                    Core.<*> x Core..:? "PlanId"
                    Core.<*> x Core..:? "PlanName"
                    Core.<*> x Core..:? "PlanType"
                    Core.<*> x Core..:? "ProductId"
                    Core.<*> x Core..:? "ProvisionProductId"
                    Core.<*> x Core..:? "ProvisionProductName"
                    Core.<*> x Core..:? "ProvisioningArtifactId"
                    Core.<*> x Core..:? "ProvisioningParameters"
                    Core.<*> x Core..:? "Status"
                    Core.<*> x Core..:? "StatusMessage"
                    Core.<*> x Core..:? "Tags"
                    Core.<*> x Core..:? "UpdatedTime"
