{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisionedProductAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisionedProductAttribute
  ( ProvisionedProductAttribute (..),

    -- * Smart constructor
    mkProvisionedProductAttribute,

    -- * Lenses
    ppaArn,
    ppaCreatedTime,
    ppaId,
    ppaIdempotencyToken,
    ppaLastProvisioningRecordId,
    ppaLastRecordId,
    ppaLastSuccessfulProvisioningRecordId,
    ppaName,
    ppaPhysicalId,
    ppaProductId,
    ppaProductName,
    ppaProvisioningArtifactId,
    ppaProvisioningArtifactName,
    ppaStatus,
    ppaStatusMessage,
    ppaTags,
    ppaType,
    ppaUserArn,
    ppaUserArnSession,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.Id as Types
import qualified Network.AWS.ServiceCatalog.Types.IdempotencyToken as Types
import qualified Network.AWS.ServiceCatalog.Types.LastProvisioningRecordId as Types
import qualified Network.AWS.ServiceCatalog.Types.LastRecordId as Types
import qualified Network.AWS.ServiceCatalog.Types.LastSuccessfulProvisioningRecordId as Types
import qualified Network.AWS.ServiceCatalog.Types.PhysicalId as Types
import qualified Network.AWS.ServiceCatalog.Types.ProductId as Types
import qualified Network.AWS.ServiceCatalog.Types.ProductName as Types
import qualified Network.AWS.ServiceCatalog.Types.ProvisionedProductNameOrArn as Types
import qualified Network.AWS.ServiceCatalog.Types.ProvisionedProductStatus as Types
import qualified Network.AWS.ServiceCatalog.Types.ProvisionedProductStatusMessage as Types
import qualified Network.AWS.ServiceCatalog.Types.ProvisioningArtifactId as Types
import qualified Network.AWS.ServiceCatalog.Types.ProvisioningArtifactName as Types
import qualified Network.AWS.ServiceCatalog.Types.Tag as Types
import qualified Network.AWS.ServiceCatalog.Types.Type as Types
import qualified Network.AWS.ServiceCatalog.Types.UserArn as Types
import qualified Network.AWS.ServiceCatalog.Types.UserArnSession as Types

-- | Information about a provisioned product.
--
-- /See:/ 'mkProvisionedProductAttribute' smart constructor.
data ProvisionedProductAttribute = ProvisionedProductAttribute'
  { -- | The ARN of the provisioned product.
    arn :: Core.Maybe Types.ProvisionedProductNameOrArn,
    -- | The UTC time stamp of the creation time.
    createdTime :: Core.Maybe Core.NominalDiffTime,
    -- | The identifier of the provisioned product.
    id :: Core.Maybe Types.Id,
    -- | A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
    idempotencyToken :: Core.Maybe Types.IdempotencyToken,
    -- | The record identifier of the last request performed on this provisioned product of the following types:
    --
    --
    --     * ProvisionedProduct
    --
    --
    --     * UpdateProvisionedProduct
    --
    --
    --     * ExecuteProvisionedProductPlan
    --
    --
    --     * TerminateProvisionedProduct
    lastProvisioningRecordId :: Core.Maybe Types.LastProvisioningRecordId,
    -- | The record identifier of the last request performed on this provisioned product.
    lastRecordId :: Core.Maybe Types.LastRecordId,
    -- | The record identifier of the last successful request performed on this provisioned product of the following types:
    --
    --
    --     * ProvisionedProduct
    --
    --
    --     * UpdateProvisionedProduct
    --
    --
    --     * ExecuteProvisionedProductPlan
    --
    --
    --     * TerminateProvisionedProduct
    lastSuccessfulProvisioningRecordId :: Core.Maybe Types.LastSuccessfulProvisioningRecordId,
    -- | The user-friendly name of the provisioned product.
    name :: Core.Maybe Types.ProvisionedProductNameOrArn,
    -- | The assigned identifier for the resource, such as an EC2 instance ID or an S3 bucket name.
    physicalId :: Core.Maybe Types.PhysicalId,
    -- | The product identifier.
    productId :: Core.Maybe Types.ProductId,
    -- | The name of the product.
    productName :: Core.Maybe Types.ProductName,
    -- | The identifier of the provisioning artifact.
    provisioningArtifactId :: Core.Maybe Types.ProvisioningArtifactId,
    -- | The name of the provisioning artifact.
    provisioningArtifactName :: Core.Maybe Types.ProvisioningArtifactName,
    -- | The current status of the provisioned product.
    --
    --
    --     * @AVAILABLE@ - Stable state, ready to perform any operation. The most recent operation succeeded and completed.
    --
    --
    --     * @UNDER_CHANGE@ - Transitive state. Operations performed might not have valid results. Wait for an @AVAILABLE@ status before performing operations.
    --
    --
    --     * @TAINTED@ - Stable state, ready to perform any operation. The stack has completed the requested operation but is not exactly what was requested. For example, a request to update to a new version failed and the stack rolled back to the current version.
    --
    --
    --     * @ERROR@ - An unexpected error occurred. The provisioned product exists but the stack is not running. For example, CloudFormation received a parameter value that was not valid and could not launch the stack.
    --
    --
    --     * @PLAN_IN_PROGRESS@ - Transitive state. The plan operations were performed to provision a new product, but resources have not yet been created. After reviewing the list of resources to be created, execute the plan. Wait for an @AVAILABLE@ status before performing operations.
    status :: Core.Maybe Types.ProvisionedProductStatus,
    -- | The current status message of the provisioned product.
    statusMessage :: Core.Maybe Types.ProvisionedProductStatusMessage,
    -- | One or more tags.
    tags :: Core.Maybe [Types.Tag],
    -- | The type of provisioned product. The supported values are @CFN_STACK@ and @CFN_STACKSET@ .
    type' :: Core.Maybe Types.Type,
    -- | The Amazon Resource Name (ARN) of the IAM user.
    userArn :: Core.Maybe Types.UserArn,
    -- | The ARN of the IAM user in the session. This ARN might contain a session ID.
    userArnSession :: Core.Maybe Types.UserArnSession
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ProvisionedProductAttribute' value with any optional fields omitted.
mkProvisionedProductAttribute ::
  ProvisionedProductAttribute
mkProvisionedProductAttribute =
  ProvisionedProductAttribute'
    { arn = Core.Nothing,
      createdTime = Core.Nothing,
      id = Core.Nothing,
      idempotencyToken = Core.Nothing,
      lastProvisioningRecordId = Core.Nothing,
      lastRecordId = Core.Nothing,
      lastSuccessfulProvisioningRecordId = Core.Nothing,
      name = Core.Nothing,
      physicalId = Core.Nothing,
      productId = Core.Nothing,
      productName = Core.Nothing,
      provisioningArtifactId = Core.Nothing,
      provisioningArtifactName = Core.Nothing,
      status = Core.Nothing,
      statusMessage = Core.Nothing,
      tags = Core.Nothing,
      type' = Core.Nothing,
      userArn = Core.Nothing,
      userArnSession = Core.Nothing
    }

-- | The ARN of the provisioned product.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppaArn :: Lens.Lens' ProvisionedProductAttribute (Core.Maybe Types.ProvisionedProductNameOrArn)
ppaArn = Lens.field @"arn"
{-# DEPRECATED ppaArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The UTC time stamp of the creation time.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppaCreatedTime :: Lens.Lens' ProvisionedProductAttribute (Core.Maybe Core.NominalDiffTime)
ppaCreatedTime = Lens.field @"createdTime"
{-# DEPRECATED ppaCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | The identifier of the provisioned product.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppaId :: Lens.Lens' ProvisionedProductAttribute (Core.Maybe Types.Id)
ppaId = Lens.field @"id"
{-# DEPRECATED ppaId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
--
-- /Note:/ Consider using 'idempotencyToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppaIdempotencyToken :: Lens.Lens' ProvisionedProductAttribute (Core.Maybe Types.IdempotencyToken)
ppaIdempotencyToken = Lens.field @"idempotencyToken"
{-# DEPRECATED ppaIdempotencyToken "Use generic-lens or generic-optics with 'idempotencyToken' instead." #-}

-- | The record identifier of the last request performed on this provisioned product of the following types:
--
--
--     * ProvisionedProduct
--
--
--     * UpdateProvisionedProduct
--
--
--     * ExecuteProvisionedProductPlan
--
--
--     * TerminateProvisionedProduct
--
--
--
-- /Note:/ Consider using 'lastProvisioningRecordId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppaLastProvisioningRecordId :: Lens.Lens' ProvisionedProductAttribute (Core.Maybe Types.LastProvisioningRecordId)
ppaLastProvisioningRecordId = Lens.field @"lastProvisioningRecordId"
{-# DEPRECATED ppaLastProvisioningRecordId "Use generic-lens or generic-optics with 'lastProvisioningRecordId' instead." #-}

-- | The record identifier of the last request performed on this provisioned product.
--
-- /Note:/ Consider using 'lastRecordId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppaLastRecordId :: Lens.Lens' ProvisionedProductAttribute (Core.Maybe Types.LastRecordId)
ppaLastRecordId = Lens.field @"lastRecordId"
{-# DEPRECATED ppaLastRecordId "Use generic-lens or generic-optics with 'lastRecordId' instead." #-}

-- | The record identifier of the last successful request performed on this provisioned product of the following types:
--
--
--     * ProvisionedProduct
--
--
--     * UpdateProvisionedProduct
--
--
--     * ExecuteProvisionedProductPlan
--
--
--     * TerminateProvisionedProduct
--
--
--
-- /Note:/ Consider using 'lastSuccessfulProvisioningRecordId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppaLastSuccessfulProvisioningRecordId :: Lens.Lens' ProvisionedProductAttribute (Core.Maybe Types.LastSuccessfulProvisioningRecordId)
ppaLastSuccessfulProvisioningRecordId = Lens.field @"lastSuccessfulProvisioningRecordId"
{-# DEPRECATED ppaLastSuccessfulProvisioningRecordId "Use generic-lens or generic-optics with 'lastSuccessfulProvisioningRecordId' instead." #-}

-- | The user-friendly name of the provisioned product.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppaName :: Lens.Lens' ProvisionedProductAttribute (Core.Maybe Types.ProvisionedProductNameOrArn)
ppaName = Lens.field @"name"
{-# DEPRECATED ppaName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The assigned identifier for the resource, such as an EC2 instance ID or an S3 bucket name.
--
-- /Note:/ Consider using 'physicalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppaPhysicalId :: Lens.Lens' ProvisionedProductAttribute (Core.Maybe Types.PhysicalId)
ppaPhysicalId = Lens.field @"physicalId"
{-# DEPRECATED ppaPhysicalId "Use generic-lens or generic-optics with 'physicalId' instead." #-}

-- | The product identifier.
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppaProductId :: Lens.Lens' ProvisionedProductAttribute (Core.Maybe Types.ProductId)
ppaProductId = Lens.field @"productId"
{-# DEPRECATED ppaProductId "Use generic-lens or generic-optics with 'productId' instead." #-}

-- | The name of the product.
--
-- /Note:/ Consider using 'productName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppaProductName :: Lens.Lens' ProvisionedProductAttribute (Core.Maybe Types.ProductName)
ppaProductName = Lens.field @"productName"
{-# DEPRECATED ppaProductName "Use generic-lens or generic-optics with 'productName' instead." #-}

-- | The identifier of the provisioning artifact.
--
-- /Note:/ Consider using 'provisioningArtifactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppaProvisioningArtifactId :: Lens.Lens' ProvisionedProductAttribute (Core.Maybe Types.ProvisioningArtifactId)
ppaProvisioningArtifactId = Lens.field @"provisioningArtifactId"
{-# DEPRECATED ppaProvisioningArtifactId "Use generic-lens or generic-optics with 'provisioningArtifactId' instead." #-}

-- | The name of the provisioning artifact.
--
-- /Note:/ Consider using 'provisioningArtifactName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppaProvisioningArtifactName :: Lens.Lens' ProvisionedProductAttribute (Core.Maybe Types.ProvisioningArtifactName)
ppaProvisioningArtifactName = Lens.field @"provisioningArtifactName"
{-# DEPRECATED ppaProvisioningArtifactName "Use generic-lens or generic-optics with 'provisioningArtifactName' instead." #-}

-- | The current status of the provisioned product.
--
--
--     * @AVAILABLE@ - Stable state, ready to perform any operation. The most recent operation succeeded and completed.
--
--
--     * @UNDER_CHANGE@ - Transitive state. Operations performed might not have valid results. Wait for an @AVAILABLE@ status before performing operations.
--
--
--     * @TAINTED@ - Stable state, ready to perform any operation. The stack has completed the requested operation but is not exactly what was requested. For example, a request to update to a new version failed and the stack rolled back to the current version.
--
--
--     * @ERROR@ - An unexpected error occurred. The provisioned product exists but the stack is not running. For example, CloudFormation received a parameter value that was not valid and could not launch the stack.
--
--
--     * @PLAN_IN_PROGRESS@ - Transitive state. The plan operations were performed to provision a new product, but resources have not yet been created. After reviewing the list of resources to be created, execute the plan. Wait for an @AVAILABLE@ status before performing operations.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppaStatus :: Lens.Lens' ProvisionedProductAttribute (Core.Maybe Types.ProvisionedProductStatus)
ppaStatus = Lens.field @"status"
{-# DEPRECATED ppaStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The current status message of the provisioned product.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppaStatusMessage :: Lens.Lens' ProvisionedProductAttribute (Core.Maybe Types.ProvisionedProductStatusMessage)
ppaStatusMessage = Lens.field @"statusMessage"
{-# DEPRECATED ppaStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | One or more tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppaTags :: Lens.Lens' ProvisionedProductAttribute (Core.Maybe [Types.Tag])
ppaTags = Lens.field @"tags"
{-# DEPRECATED ppaTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The type of provisioned product. The supported values are @CFN_STACK@ and @CFN_STACKSET@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppaType :: Lens.Lens' ProvisionedProductAttribute (Core.Maybe Types.Type)
ppaType = Lens.field @"type'"
{-# DEPRECATED ppaType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM user.
--
-- /Note:/ Consider using 'userArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppaUserArn :: Lens.Lens' ProvisionedProductAttribute (Core.Maybe Types.UserArn)
ppaUserArn = Lens.field @"userArn"
{-# DEPRECATED ppaUserArn "Use generic-lens or generic-optics with 'userArn' instead." #-}

-- | The ARN of the IAM user in the session. This ARN might contain a session ID.
--
-- /Note:/ Consider using 'userArnSession' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppaUserArnSession :: Lens.Lens' ProvisionedProductAttribute (Core.Maybe Types.UserArnSession)
ppaUserArnSession = Lens.field @"userArnSession"
{-# DEPRECATED ppaUserArnSession "Use generic-lens or generic-optics with 'userArnSession' instead." #-}

instance Core.FromJSON ProvisionedProductAttribute where
  parseJSON =
    Core.withObject "ProvisionedProductAttribute" Core.$
      \x ->
        ProvisionedProductAttribute'
          Core.<$> (x Core..:? "Arn")
          Core.<*> (x Core..:? "CreatedTime")
          Core.<*> (x Core..:? "Id")
          Core.<*> (x Core..:? "IdempotencyToken")
          Core.<*> (x Core..:? "LastProvisioningRecordId")
          Core.<*> (x Core..:? "LastRecordId")
          Core.<*> (x Core..:? "LastSuccessfulProvisioningRecordId")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "PhysicalId")
          Core.<*> (x Core..:? "ProductId")
          Core.<*> (x Core..:? "ProductName")
          Core.<*> (x Core..:? "ProvisioningArtifactId")
          Core.<*> (x Core..:? "ProvisioningArtifactName")
          Core.<*> (x Core..:? "Status")
          Core.<*> (x Core..:? "StatusMessage")
          Core.<*> (x Core..:? "Tags")
          Core.<*> (x Core..:? "Type")
          Core.<*> (x Core..:? "UserArn")
          Core.<*> (x Core..:? "UserArnSession")
