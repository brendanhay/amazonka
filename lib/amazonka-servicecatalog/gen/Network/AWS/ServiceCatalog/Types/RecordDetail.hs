{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.RecordDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.RecordDetail
  ( RecordDetail (..),

    -- * Smart constructor
    mkRecordDetail,

    -- * Lenses
    rdCreatedTime,
    rdLaunchRoleArn,
    rdPathId,
    rdProductId,
    rdProvisionedProductId,
    rdProvisionedProductName,
    rdProvisionedProductType,
    rdProvisioningArtifactId,
    rdRecordErrors,
    rdRecordId,
    rdRecordTags,
    rdRecordType,
    rdStatus,
    rdUpdatedTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.LaunchRoleArn as Types
import qualified Network.AWS.ServiceCatalog.Types.PathId as Types
import qualified Network.AWS.ServiceCatalog.Types.ProductId as Types
import qualified Network.AWS.ServiceCatalog.Types.ProvisionedProductId as Types
import qualified Network.AWS.ServiceCatalog.Types.ProvisionedProductName as Types
import qualified Network.AWS.ServiceCatalog.Types.ProvisionedProductType as Types
import qualified Network.AWS.ServiceCatalog.Types.ProvisioningArtifactId as Types
import qualified Network.AWS.ServiceCatalog.Types.RecordError as Types
import qualified Network.AWS.ServiceCatalog.Types.RecordId as Types
import qualified Network.AWS.ServiceCatalog.Types.RecordStatus as Types
import qualified Network.AWS.ServiceCatalog.Types.RecordTag as Types
import qualified Network.AWS.ServiceCatalog.Types.RecordType as Types

-- | Information about a request operation.
--
-- /See:/ 'mkRecordDetail' smart constructor.
data RecordDetail = RecordDetail'
  { -- | The UTC time stamp of the creation time.
    createdTime :: Core.Maybe Core.NominalDiffTime,
    -- | The ARN of the launch role associated with the provisioned product.
    launchRoleArn :: Core.Maybe Types.LaunchRoleArn,
    -- | The path identifier.
    pathId :: Core.Maybe Types.PathId,
    -- | The product identifier.
    productId :: Core.Maybe Types.ProductId,
    -- | The identifier of the provisioned product.
    provisionedProductId :: Core.Maybe Types.ProvisionedProductId,
    -- | The user-friendly name of the provisioned product.
    provisionedProductName :: Core.Maybe Types.ProvisionedProductName,
    -- | The type of provisioned product. The supported values are @CFN_STACK@ and @CFN_STACKSET@ .
    provisionedProductType :: Core.Maybe Types.ProvisionedProductType,
    -- | The identifier of the provisioning artifact.
    provisioningArtifactId :: Core.Maybe Types.ProvisioningArtifactId,
    -- | The errors that occurred.
    recordErrors :: Core.Maybe [Types.RecordError],
    -- | The identifier of the record.
    recordId :: Core.Maybe Types.RecordId,
    -- | One or more tags.
    recordTags :: Core.Maybe [Types.RecordTag],
    -- | The record type.
    --
    --
    --     * @PROVISION_PRODUCT@
    --
    --
    --     * @UPDATE_PROVISIONED_PRODUCT@
    --
    --
    --     * @TERMINATE_PROVISIONED_PRODUCT@
    recordType :: Core.Maybe Types.RecordType,
    -- | The status of the provisioned product.
    --
    --
    --     * @CREATED@ - The request was created but the operation has not started.
    --
    --
    --     * @IN_PROGRESS@ - The requested operation is in progress.
    --
    --
    --     * @IN_PROGRESS_IN_ERROR@ - The provisioned product is under change but the requested operation failed and some remediation is occurring. For example, a rollback.
    --
    --
    --     * @SUCCEEDED@ - The requested operation has successfully completed.
    --
    --
    --     * @FAILED@ - The requested operation has unsuccessfully completed. Investigate using the error messages returned.
    status :: Core.Maybe Types.RecordStatus,
    -- | The time when the record was last updated.
    updatedTime :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'RecordDetail' value with any optional fields omitted.
mkRecordDetail ::
  RecordDetail
mkRecordDetail =
  RecordDetail'
    { createdTime = Core.Nothing,
      launchRoleArn = Core.Nothing,
      pathId = Core.Nothing,
      productId = Core.Nothing,
      provisionedProductId = Core.Nothing,
      provisionedProductName = Core.Nothing,
      provisionedProductType = Core.Nothing,
      provisioningArtifactId = Core.Nothing,
      recordErrors = Core.Nothing,
      recordId = Core.Nothing,
      recordTags = Core.Nothing,
      recordType = Core.Nothing,
      status = Core.Nothing,
      updatedTime = Core.Nothing
    }

-- | The UTC time stamp of the creation time.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdCreatedTime :: Lens.Lens' RecordDetail (Core.Maybe Core.NominalDiffTime)
rdCreatedTime = Lens.field @"createdTime"
{-# DEPRECATED rdCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | The ARN of the launch role associated with the provisioned product.
--
-- /Note:/ Consider using 'launchRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdLaunchRoleArn :: Lens.Lens' RecordDetail (Core.Maybe Types.LaunchRoleArn)
rdLaunchRoleArn = Lens.field @"launchRoleArn"
{-# DEPRECATED rdLaunchRoleArn "Use generic-lens or generic-optics with 'launchRoleArn' instead." #-}

-- | The path identifier.
--
-- /Note:/ Consider using 'pathId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdPathId :: Lens.Lens' RecordDetail (Core.Maybe Types.PathId)
rdPathId = Lens.field @"pathId"
{-# DEPRECATED rdPathId "Use generic-lens or generic-optics with 'pathId' instead." #-}

-- | The product identifier.
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdProductId :: Lens.Lens' RecordDetail (Core.Maybe Types.ProductId)
rdProductId = Lens.field @"productId"
{-# DEPRECATED rdProductId "Use generic-lens or generic-optics with 'productId' instead." #-}

-- | The identifier of the provisioned product.
--
-- /Note:/ Consider using 'provisionedProductId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdProvisionedProductId :: Lens.Lens' RecordDetail (Core.Maybe Types.ProvisionedProductId)
rdProvisionedProductId = Lens.field @"provisionedProductId"
{-# DEPRECATED rdProvisionedProductId "Use generic-lens or generic-optics with 'provisionedProductId' instead." #-}

-- | The user-friendly name of the provisioned product.
--
-- /Note:/ Consider using 'provisionedProductName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdProvisionedProductName :: Lens.Lens' RecordDetail (Core.Maybe Types.ProvisionedProductName)
rdProvisionedProductName = Lens.field @"provisionedProductName"
{-# DEPRECATED rdProvisionedProductName "Use generic-lens or generic-optics with 'provisionedProductName' instead." #-}

-- | The type of provisioned product. The supported values are @CFN_STACK@ and @CFN_STACKSET@ .
--
-- /Note:/ Consider using 'provisionedProductType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdProvisionedProductType :: Lens.Lens' RecordDetail (Core.Maybe Types.ProvisionedProductType)
rdProvisionedProductType = Lens.field @"provisionedProductType"
{-# DEPRECATED rdProvisionedProductType "Use generic-lens or generic-optics with 'provisionedProductType' instead." #-}

-- | The identifier of the provisioning artifact.
--
-- /Note:/ Consider using 'provisioningArtifactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdProvisioningArtifactId :: Lens.Lens' RecordDetail (Core.Maybe Types.ProvisioningArtifactId)
rdProvisioningArtifactId = Lens.field @"provisioningArtifactId"
{-# DEPRECATED rdProvisioningArtifactId "Use generic-lens or generic-optics with 'provisioningArtifactId' instead." #-}

-- | The errors that occurred.
--
-- /Note:/ Consider using 'recordErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdRecordErrors :: Lens.Lens' RecordDetail (Core.Maybe [Types.RecordError])
rdRecordErrors = Lens.field @"recordErrors"
{-# DEPRECATED rdRecordErrors "Use generic-lens or generic-optics with 'recordErrors' instead." #-}

-- | The identifier of the record.
--
-- /Note:/ Consider using 'recordId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdRecordId :: Lens.Lens' RecordDetail (Core.Maybe Types.RecordId)
rdRecordId = Lens.field @"recordId"
{-# DEPRECATED rdRecordId "Use generic-lens or generic-optics with 'recordId' instead." #-}

-- | One or more tags.
--
-- /Note:/ Consider using 'recordTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdRecordTags :: Lens.Lens' RecordDetail (Core.Maybe [Types.RecordTag])
rdRecordTags = Lens.field @"recordTags"
{-# DEPRECATED rdRecordTags "Use generic-lens or generic-optics with 'recordTags' instead." #-}

-- | The record type.
--
--
--     * @PROVISION_PRODUCT@
--
--
--     * @UPDATE_PROVISIONED_PRODUCT@
--
--
--     * @TERMINATE_PROVISIONED_PRODUCT@
--
--
--
-- /Note:/ Consider using 'recordType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdRecordType :: Lens.Lens' RecordDetail (Core.Maybe Types.RecordType)
rdRecordType = Lens.field @"recordType"
{-# DEPRECATED rdRecordType "Use generic-lens or generic-optics with 'recordType' instead." #-}

-- | The status of the provisioned product.
--
--
--     * @CREATED@ - The request was created but the operation has not started.
--
--
--     * @IN_PROGRESS@ - The requested operation is in progress.
--
--
--     * @IN_PROGRESS_IN_ERROR@ - The provisioned product is under change but the requested operation failed and some remediation is occurring. For example, a rollback.
--
--
--     * @SUCCEEDED@ - The requested operation has successfully completed.
--
--
--     * @FAILED@ - The requested operation has unsuccessfully completed. Investigate using the error messages returned.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdStatus :: Lens.Lens' RecordDetail (Core.Maybe Types.RecordStatus)
rdStatus = Lens.field @"status"
{-# DEPRECATED rdStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The time when the record was last updated.
--
-- /Note:/ Consider using 'updatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdUpdatedTime :: Lens.Lens' RecordDetail (Core.Maybe Core.NominalDiffTime)
rdUpdatedTime = Lens.field @"updatedTime"
{-# DEPRECATED rdUpdatedTime "Use generic-lens or generic-optics with 'updatedTime' instead." #-}

instance Core.FromJSON RecordDetail where
  parseJSON =
    Core.withObject "RecordDetail" Core.$
      \x ->
        RecordDetail'
          Core.<$> (x Core..:? "CreatedTime")
          Core.<*> (x Core..:? "LaunchRoleArn")
          Core.<*> (x Core..:? "PathId")
          Core.<*> (x Core..:? "ProductId")
          Core.<*> (x Core..:? "ProvisionedProductId")
          Core.<*> (x Core..:? "ProvisionedProductName")
          Core.<*> (x Core..:? "ProvisionedProductType")
          Core.<*> (x Core..:? "ProvisioningArtifactId")
          Core.<*> (x Core..:? "RecordErrors")
          Core.<*> (x Core..:? "RecordId")
          Core.<*> (x Core..:? "RecordTags")
          Core.<*> (x Core..:? "RecordType")
          Core.<*> (x Core..:? "Status")
          Core.<*> (x Core..:? "UpdatedTime")
