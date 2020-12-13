{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisionedProductDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisionedProductDetail
  ( ProvisionedProductDetail (..),

    -- * Smart constructor
    mkProvisionedProductDetail,

    -- * Lenses
    ppdLaunchRoleARN,
    ppdIdempotencyToken,
    ppdStatus,
    ppdLastSuccessfulProvisioningRecordId,
    ppdProvisioningArtifactId,
    ppdARN,
    ppdCreatedTime,
    ppdStatusMessage,
    ppdName,
    ppdLastRecordId,
    ppdId,
    ppdType,
    ppdLastProvisioningRecordId,
    ppdProductId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.ServiceCatalog.Types.ProvisionedProductStatus

-- | Information about a provisioned product.
--
-- /See:/ 'mkProvisionedProductDetail' smart constructor.
data ProvisionedProductDetail = ProvisionedProductDetail'
  { -- | The ARN of the launch role associated with the provisioned product.
    launchRoleARN :: Lude.Maybe Lude.Text,
    -- | A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
    idempotencyToken :: Lude.Maybe Lude.Text,
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
    status :: Lude.Maybe ProvisionedProductStatus,
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
    lastSuccessfulProvisioningRecordId :: Lude.Maybe Lude.Text,
    -- | The identifier of the provisioning artifact. For example, @pa-4abcdjnxjj6ne@ .
    provisioningArtifactId :: Lude.Maybe Lude.Text,
    -- | The ARN of the provisioned product.
    arn :: Lude.Maybe Lude.Text,
    -- | The UTC time stamp of the creation time.
    createdTime :: Lude.Maybe Lude.Timestamp,
    -- | The current status message of the provisioned product.
    statusMessage :: Lude.Maybe Lude.Text,
    -- | The user-friendly name of the provisioned product.
    name :: Lude.Maybe Lude.Text,
    -- | The record identifier of the last request performed on this provisioned product.
    lastRecordId :: Lude.Maybe Lude.Text,
    -- | The identifier of the provisioned product.
    id :: Lude.Maybe Lude.Text,
    -- | The type of provisioned product. The supported values are @CFN_STACK@ and @CFN_STACKSET@ .
    type' :: Lude.Maybe Lude.Text,
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
    lastProvisioningRecordId :: Lude.Maybe Lude.Text,
    -- | The product identifier. For example, @prod-abcdzk7xy33qa@ .
    productId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProvisionedProductDetail' with the minimum fields required to make a request.
--
-- * 'launchRoleARN' - The ARN of the launch role associated with the provisioned product.
-- * 'idempotencyToken' - A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
-- * 'status' - The current status of the provisioned product.
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
-- * 'lastSuccessfulProvisioningRecordId' - The record identifier of the last successful request performed on this provisioned product of the following types:
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
-- * 'provisioningArtifactId' - The identifier of the provisioning artifact. For example, @pa-4abcdjnxjj6ne@ .
-- * 'arn' - The ARN of the provisioned product.
-- * 'createdTime' - The UTC time stamp of the creation time.
-- * 'statusMessage' - The current status message of the provisioned product.
-- * 'name' - The user-friendly name of the provisioned product.
-- * 'lastRecordId' - The record identifier of the last request performed on this provisioned product.
-- * 'id' - The identifier of the provisioned product.
-- * 'type'' - The type of provisioned product. The supported values are @CFN_STACK@ and @CFN_STACKSET@ .
-- * 'lastProvisioningRecordId' - The record identifier of the last request performed on this provisioned product of the following types:
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
-- * 'productId' - The product identifier. For example, @prod-abcdzk7xy33qa@ .
mkProvisionedProductDetail ::
  ProvisionedProductDetail
mkProvisionedProductDetail =
  ProvisionedProductDetail'
    { launchRoleARN = Lude.Nothing,
      idempotencyToken = Lude.Nothing,
      status = Lude.Nothing,
      lastSuccessfulProvisioningRecordId = Lude.Nothing,
      provisioningArtifactId = Lude.Nothing,
      arn = Lude.Nothing,
      createdTime = Lude.Nothing,
      statusMessage = Lude.Nothing,
      name = Lude.Nothing,
      lastRecordId = Lude.Nothing,
      id = Lude.Nothing,
      type' = Lude.Nothing,
      lastProvisioningRecordId = Lude.Nothing,
      productId = Lude.Nothing
    }

-- | The ARN of the launch role associated with the provisioned product.
--
-- /Note:/ Consider using 'launchRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppdLaunchRoleARN :: Lens.Lens' ProvisionedProductDetail (Lude.Maybe Lude.Text)
ppdLaunchRoleARN = Lens.lens (launchRoleARN :: ProvisionedProductDetail -> Lude.Maybe Lude.Text) (\s a -> s {launchRoleARN = a} :: ProvisionedProductDetail)
{-# DEPRECATED ppdLaunchRoleARN "Use generic-lens or generic-optics with 'launchRoleARN' instead." #-}

-- | A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
--
-- /Note:/ Consider using 'idempotencyToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppdIdempotencyToken :: Lens.Lens' ProvisionedProductDetail (Lude.Maybe Lude.Text)
ppdIdempotencyToken = Lens.lens (idempotencyToken :: ProvisionedProductDetail -> Lude.Maybe Lude.Text) (\s a -> s {idempotencyToken = a} :: ProvisionedProductDetail)
{-# DEPRECATED ppdIdempotencyToken "Use generic-lens or generic-optics with 'idempotencyToken' instead." #-}

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
ppdStatus :: Lens.Lens' ProvisionedProductDetail (Lude.Maybe ProvisionedProductStatus)
ppdStatus = Lens.lens (status :: ProvisionedProductDetail -> Lude.Maybe ProvisionedProductStatus) (\s a -> s {status = a} :: ProvisionedProductDetail)
{-# DEPRECATED ppdStatus "Use generic-lens or generic-optics with 'status' instead." #-}

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
ppdLastSuccessfulProvisioningRecordId :: Lens.Lens' ProvisionedProductDetail (Lude.Maybe Lude.Text)
ppdLastSuccessfulProvisioningRecordId = Lens.lens (lastSuccessfulProvisioningRecordId :: ProvisionedProductDetail -> Lude.Maybe Lude.Text) (\s a -> s {lastSuccessfulProvisioningRecordId = a} :: ProvisionedProductDetail)
{-# DEPRECATED ppdLastSuccessfulProvisioningRecordId "Use generic-lens or generic-optics with 'lastSuccessfulProvisioningRecordId' instead." #-}

-- | The identifier of the provisioning artifact. For example, @pa-4abcdjnxjj6ne@ .
--
-- /Note:/ Consider using 'provisioningArtifactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppdProvisioningArtifactId :: Lens.Lens' ProvisionedProductDetail (Lude.Maybe Lude.Text)
ppdProvisioningArtifactId = Lens.lens (provisioningArtifactId :: ProvisionedProductDetail -> Lude.Maybe Lude.Text) (\s a -> s {provisioningArtifactId = a} :: ProvisionedProductDetail)
{-# DEPRECATED ppdProvisioningArtifactId "Use generic-lens or generic-optics with 'provisioningArtifactId' instead." #-}

-- | The ARN of the provisioned product.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppdARN :: Lens.Lens' ProvisionedProductDetail (Lude.Maybe Lude.Text)
ppdARN = Lens.lens (arn :: ProvisionedProductDetail -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: ProvisionedProductDetail)
{-# DEPRECATED ppdARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The UTC time stamp of the creation time.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppdCreatedTime :: Lens.Lens' ProvisionedProductDetail (Lude.Maybe Lude.Timestamp)
ppdCreatedTime = Lens.lens (createdTime :: ProvisionedProductDetail -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdTime = a} :: ProvisionedProductDetail)
{-# DEPRECATED ppdCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | The current status message of the provisioned product.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppdStatusMessage :: Lens.Lens' ProvisionedProductDetail (Lude.Maybe Lude.Text)
ppdStatusMessage = Lens.lens (statusMessage :: ProvisionedProductDetail -> Lude.Maybe Lude.Text) (\s a -> s {statusMessage = a} :: ProvisionedProductDetail)
{-# DEPRECATED ppdStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | The user-friendly name of the provisioned product.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppdName :: Lens.Lens' ProvisionedProductDetail (Lude.Maybe Lude.Text)
ppdName = Lens.lens (name :: ProvisionedProductDetail -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ProvisionedProductDetail)
{-# DEPRECATED ppdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The record identifier of the last request performed on this provisioned product.
--
-- /Note:/ Consider using 'lastRecordId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppdLastRecordId :: Lens.Lens' ProvisionedProductDetail (Lude.Maybe Lude.Text)
ppdLastRecordId = Lens.lens (lastRecordId :: ProvisionedProductDetail -> Lude.Maybe Lude.Text) (\s a -> s {lastRecordId = a} :: ProvisionedProductDetail)
{-# DEPRECATED ppdLastRecordId "Use generic-lens or generic-optics with 'lastRecordId' instead." #-}

-- | The identifier of the provisioned product.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppdId :: Lens.Lens' ProvisionedProductDetail (Lude.Maybe Lude.Text)
ppdId = Lens.lens (id :: ProvisionedProductDetail -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: ProvisionedProductDetail)
{-# DEPRECATED ppdId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The type of provisioned product. The supported values are @CFN_STACK@ and @CFN_STACKSET@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppdType :: Lens.Lens' ProvisionedProductDetail (Lude.Maybe Lude.Text)
ppdType = Lens.lens (type' :: ProvisionedProductDetail -> Lude.Maybe Lude.Text) (\s a -> s {type' = a} :: ProvisionedProductDetail)
{-# DEPRECATED ppdType "Use generic-lens or generic-optics with 'type'' instead." #-}

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
ppdLastProvisioningRecordId :: Lens.Lens' ProvisionedProductDetail (Lude.Maybe Lude.Text)
ppdLastProvisioningRecordId = Lens.lens (lastProvisioningRecordId :: ProvisionedProductDetail -> Lude.Maybe Lude.Text) (\s a -> s {lastProvisioningRecordId = a} :: ProvisionedProductDetail)
{-# DEPRECATED ppdLastProvisioningRecordId "Use generic-lens or generic-optics with 'lastProvisioningRecordId' instead." #-}

-- | The product identifier. For example, @prod-abcdzk7xy33qa@ .
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppdProductId :: Lens.Lens' ProvisionedProductDetail (Lude.Maybe Lude.Text)
ppdProductId = Lens.lens (productId :: ProvisionedProductDetail -> Lude.Maybe Lude.Text) (\s a -> s {productId = a} :: ProvisionedProductDetail)
{-# DEPRECATED ppdProductId "Use generic-lens or generic-optics with 'productId' instead." #-}

instance Lude.FromJSON ProvisionedProductDetail where
  parseJSON =
    Lude.withObject
      "ProvisionedProductDetail"
      ( \x ->
          ProvisionedProductDetail'
            Lude.<$> (x Lude..:? "LaunchRoleArn")
            Lude.<*> (x Lude..:? "IdempotencyToken")
            Lude.<*> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "LastSuccessfulProvisioningRecordId")
            Lude.<*> (x Lude..:? "ProvisioningArtifactId")
            Lude.<*> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "CreatedTime")
            Lude.<*> (x Lude..:? "StatusMessage")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "LastRecordId")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "Type")
            Lude.<*> (x Lude..:? "LastProvisioningRecordId")
            Lude.<*> (x Lude..:? "ProductId")
      )
