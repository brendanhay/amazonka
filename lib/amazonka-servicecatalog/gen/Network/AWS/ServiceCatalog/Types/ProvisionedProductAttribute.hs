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
    ppaIdempotencyToken,
    ppaStatus,
    ppaProductName,
    ppaLastSuccessfulProvisioningRecordId,
    ppaProvisioningArtifactId,
    ppaARN,
    ppaCreatedTime,
    ppaProvisioningArtifactName,
    ppaUserARN,
    ppaStatusMessage,
    ppaName,
    ppaLastRecordId,
    ppaUserARNSession,
    ppaId,
    ppaType,
    ppaPhysicalId,
    ppaLastProvisioningRecordId,
    ppaProductId,
    ppaTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.ServiceCatalog.Types.ProvisionedProductStatus
import Network.AWS.ServiceCatalog.Types.Tag

-- | Information about a provisioned product.
--
-- /See:/ 'mkProvisionedProductAttribute' smart constructor.
data ProvisionedProductAttribute = ProvisionedProductAttribute'
  { idempotencyToken ::
      Lude.Maybe Lude.Text,
    status ::
      Lude.Maybe ProvisionedProductStatus,
    productName :: Lude.Maybe Lude.Text,
    lastSuccessfulProvisioningRecordId ::
      Lude.Maybe Lude.Text,
    provisioningArtifactId ::
      Lude.Maybe Lude.Text,
    arn :: Lude.Maybe Lude.Text,
    createdTime ::
      Lude.Maybe Lude.Timestamp,
    provisioningArtifactName ::
      Lude.Maybe Lude.Text,
    userARN :: Lude.Maybe Lude.Text,
    statusMessage ::
      Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    lastRecordId ::
      Lude.Maybe Lude.Text,
    userARNSession ::
      Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe Lude.Text,
    physicalId :: Lude.Maybe Lude.Text,
    lastProvisioningRecordId ::
      Lude.Maybe Lude.Text,
    productId :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProvisionedProductAttribute' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the provisioned product.
-- * 'createdTime' - The UTC time stamp of the creation time.
-- * 'id' - The identifier of the provisioned product.
-- * 'idempotencyToken' - A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
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
-- * 'lastRecordId' - The record identifier of the last request performed on this provisioned product.
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
-- * 'name' - The user-friendly name of the provisioned product.
-- * 'physicalId' - The assigned identifier for the resource, such as an EC2 instance ID or an S3 bucket name.
-- * 'productId' - The product identifier.
-- * 'productName' - The name of the product.
-- * 'provisioningArtifactId' - The identifier of the provisioning artifact.
-- * 'provisioningArtifactName' - The name of the provisioning artifact.
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
-- * 'statusMessage' - The current status message of the provisioned product.
-- * 'tags' - One or more tags.
-- * 'type'' - The type of provisioned product. The supported values are @CFN_STACK@ and @CFN_STACKSET@ .
-- * 'userARN' - The Amazon Resource Name (ARN) of the IAM user.
-- * 'userARNSession' - The ARN of the IAM user in the session. This ARN might contain a session ID.
mkProvisionedProductAttribute ::
  ProvisionedProductAttribute
mkProvisionedProductAttribute =
  ProvisionedProductAttribute'
    { idempotencyToken = Lude.Nothing,
      status = Lude.Nothing,
      productName = Lude.Nothing,
      lastSuccessfulProvisioningRecordId = Lude.Nothing,
      provisioningArtifactId = Lude.Nothing,
      arn = Lude.Nothing,
      createdTime = Lude.Nothing,
      provisioningArtifactName = Lude.Nothing,
      userARN = Lude.Nothing,
      statusMessage = Lude.Nothing,
      name = Lude.Nothing,
      lastRecordId = Lude.Nothing,
      userARNSession = Lude.Nothing,
      id = Lude.Nothing,
      type' = Lude.Nothing,
      physicalId = Lude.Nothing,
      lastProvisioningRecordId = Lude.Nothing,
      productId = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
--
-- /Note:/ Consider using 'idempotencyToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppaIdempotencyToken :: Lens.Lens' ProvisionedProductAttribute (Lude.Maybe Lude.Text)
ppaIdempotencyToken = Lens.lens (idempotencyToken :: ProvisionedProductAttribute -> Lude.Maybe Lude.Text) (\s a -> s {idempotencyToken = a} :: ProvisionedProductAttribute)
{-# DEPRECATED ppaIdempotencyToken "Use generic-lens or generic-optics with 'idempotencyToken' instead." #-}

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
ppaStatus :: Lens.Lens' ProvisionedProductAttribute (Lude.Maybe ProvisionedProductStatus)
ppaStatus = Lens.lens (status :: ProvisionedProductAttribute -> Lude.Maybe ProvisionedProductStatus) (\s a -> s {status = a} :: ProvisionedProductAttribute)
{-# DEPRECATED ppaStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The name of the product.
--
-- /Note:/ Consider using 'productName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppaProductName :: Lens.Lens' ProvisionedProductAttribute (Lude.Maybe Lude.Text)
ppaProductName = Lens.lens (productName :: ProvisionedProductAttribute -> Lude.Maybe Lude.Text) (\s a -> s {productName = a} :: ProvisionedProductAttribute)
{-# DEPRECATED ppaProductName "Use generic-lens or generic-optics with 'productName' instead." #-}

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
ppaLastSuccessfulProvisioningRecordId :: Lens.Lens' ProvisionedProductAttribute (Lude.Maybe Lude.Text)
ppaLastSuccessfulProvisioningRecordId = Lens.lens (lastSuccessfulProvisioningRecordId :: ProvisionedProductAttribute -> Lude.Maybe Lude.Text) (\s a -> s {lastSuccessfulProvisioningRecordId = a} :: ProvisionedProductAttribute)
{-# DEPRECATED ppaLastSuccessfulProvisioningRecordId "Use generic-lens or generic-optics with 'lastSuccessfulProvisioningRecordId' instead." #-}

-- | The identifier of the provisioning artifact.
--
-- /Note:/ Consider using 'provisioningArtifactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppaProvisioningArtifactId :: Lens.Lens' ProvisionedProductAttribute (Lude.Maybe Lude.Text)
ppaProvisioningArtifactId = Lens.lens (provisioningArtifactId :: ProvisionedProductAttribute -> Lude.Maybe Lude.Text) (\s a -> s {provisioningArtifactId = a} :: ProvisionedProductAttribute)
{-# DEPRECATED ppaProvisioningArtifactId "Use generic-lens or generic-optics with 'provisioningArtifactId' instead." #-}

-- | The ARN of the provisioned product.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppaARN :: Lens.Lens' ProvisionedProductAttribute (Lude.Maybe Lude.Text)
ppaARN = Lens.lens (arn :: ProvisionedProductAttribute -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: ProvisionedProductAttribute)
{-# DEPRECATED ppaARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The UTC time stamp of the creation time.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppaCreatedTime :: Lens.Lens' ProvisionedProductAttribute (Lude.Maybe Lude.Timestamp)
ppaCreatedTime = Lens.lens (createdTime :: ProvisionedProductAttribute -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdTime = a} :: ProvisionedProductAttribute)
{-# DEPRECATED ppaCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | The name of the provisioning artifact.
--
-- /Note:/ Consider using 'provisioningArtifactName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppaProvisioningArtifactName :: Lens.Lens' ProvisionedProductAttribute (Lude.Maybe Lude.Text)
ppaProvisioningArtifactName = Lens.lens (provisioningArtifactName :: ProvisionedProductAttribute -> Lude.Maybe Lude.Text) (\s a -> s {provisioningArtifactName = a} :: ProvisionedProductAttribute)
{-# DEPRECATED ppaProvisioningArtifactName "Use generic-lens or generic-optics with 'provisioningArtifactName' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM user.
--
-- /Note:/ Consider using 'userARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppaUserARN :: Lens.Lens' ProvisionedProductAttribute (Lude.Maybe Lude.Text)
ppaUserARN = Lens.lens (userARN :: ProvisionedProductAttribute -> Lude.Maybe Lude.Text) (\s a -> s {userARN = a} :: ProvisionedProductAttribute)
{-# DEPRECATED ppaUserARN "Use generic-lens or generic-optics with 'userARN' instead." #-}

-- | The current status message of the provisioned product.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppaStatusMessage :: Lens.Lens' ProvisionedProductAttribute (Lude.Maybe Lude.Text)
ppaStatusMessage = Lens.lens (statusMessage :: ProvisionedProductAttribute -> Lude.Maybe Lude.Text) (\s a -> s {statusMessage = a} :: ProvisionedProductAttribute)
{-# DEPRECATED ppaStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | The user-friendly name of the provisioned product.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppaName :: Lens.Lens' ProvisionedProductAttribute (Lude.Maybe Lude.Text)
ppaName = Lens.lens (name :: ProvisionedProductAttribute -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ProvisionedProductAttribute)
{-# DEPRECATED ppaName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The record identifier of the last request performed on this provisioned product.
--
-- /Note:/ Consider using 'lastRecordId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppaLastRecordId :: Lens.Lens' ProvisionedProductAttribute (Lude.Maybe Lude.Text)
ppaLastRecordId = Lens.lens (lastRecordId :: ProvisionedProductAttribute -> Lude.Maybe Lude.Text) (\s a -> s {lastRecordId = a} :: ProvisionedProductAttribute)
{-# DEPRECATED ppaLastRecordId "Use generic-lens or generic-optics with 'lastRecordId' instead." #-}

-- | The ARN of the IAM user in the session. This ARN might contain a session ID.
--
-- /Note:/ Consider using 'userARNSession' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppaUserARNSession :: Lens.Lens' ProvisionedProductAttribute (Lude.Maybe Lude.Text)
ppaUserARNSession = Lens.lens (userARNSession :: ProvisionedProductAttribute -> Lude.Maybe Lude.Text) (\s a -> s {userARNSession = a} :: ProvisionedProductAttribute)
{-# DEPRECATED ppaUserARNSession "Use generic-lens or generic-optics with 'userARNSession' instead." #-}

-- | The identifier of the provisioned product.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppaId :: Lens.Lens' ProvisionedProductAttribute (Lude.Maybe Lude.Text)
ppaId = Lens.lens (id :: ProvisionedProductAttribute -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: ProvisionedProductAttribute)
{-# DEPRECATED ppaId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The type of provisioned product. The supported values are @CFN_STACK@ and @CFN_STACKSET@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppaType :: Lens.Lens' ProvisionedProductAttribute (Lude.Maybe Lude.Text)
ppaType = Lens.lens (type' :: ProvisionedProductAttribute -> Lude.Maybe Lude.Text) (\s a -> s {type' = a} :: ProvisionedProductAttribute)
{-# DEPRECATED ppaType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The assigned identifier for the resource, such as an EC2 instance ID or an S3 bucket name.
--
-- /Note:/ Consider using 'physicalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppaPhysicalId :: Lens.Lens' ProvisionedProductAttribute (Lude.Maybe Lude.Text)
ppaPhysicalId = Lens.lens (physicalId :: ProvisionedProductAttribute -> Lude.Maybe Lude.Text) (\s a -> s {physicalId = a} :: ProvisionedProductAttribute)
{-# DEPRECATED ppaPhysicalId "Use generic-lens or generic-optics with 'physicalId' instead." #-}

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
ppaLastProvisioningRecordId :: Lens.Lens' ProvisionedProductAttribute (Lude.Maybe Lude.Text)
ppaLastProvisioningRecordId = Lens.lens (lastProvisioningRecordId :: ProvisionedProductAttribute -> Lude.Maybe Lude.Text) (\s a -> s {lastProvisioningRecordId = a} :: ProvisionedProductAttribute)
{-# DEPRECATED ppaLastProvisioningRecordId "Use generic-lens or generic-optics with 'lastProvisioningRecordId' instead." #-}

-- | The product identifier.
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppaProductId :: Lens.Lens' ProvisionedProductAttribute (Lude.Maybe Lude.Text)
ppaProductId = Lens.lens (productId :: ProvisionedProductAttribute -> Lude.Maybe Lude.Text) (\s a -> s {productId = a} :: ProvisionedProductAttribute)
{-# DEPRECATED ppaProductId "Use generic-lens or generic-optics with 'productId' instead." #-}

-- | One or more tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppaTags :: Lens.Lens' ProvisionedProductAttribute (Lude.Maybe [Tag])
ppaTags = Lens.lens (tags :: ProvisionedProductAttribute -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: ProvisionedProductAttribute)
{-# DEPRECATED ppaTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON ProvisionedProductAttribute where
  parseJSON =
    Lude.withObject
      "ProvisionedProductAttribute"
      ( \x ->
          ProvisionedProductAttribute'
            Lude.<$> (x Lude..:? "IdempotencyToken")
            Lude.<*> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "ProductName")
            Lude.<*> (x Lude..:? "LastSuccessfulProvisioningRecordId")
            Lude.<*> (x Lude..:? "ProvisioningArtifactId")
            Lude.<*> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "CreatedTime")
            Lude.<*> (x Lude..:? "ProvisioningArtifactName")
            Lude.<*> (x Lude..:? "UserArn")
            Lude.<*> (x Lude..:? "StatusMessage")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "LastRecordId")
            Lude.<*> (x Lude..:? "UserArnSession")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "Type")
            Lude.<*> (x Lude..:? "PhysicalId")
            Lude.<*> (x Lude..:? "LastProvisioningRecordId")
            Lude.<*> (x Lude..:? "ProductId")
            Lude.<*> (x Lude..:? "Tags" Lude..!= Lude.mempty)
      )
