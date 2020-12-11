-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanDetails
  ( ProvisionedProductPlanDetails (..),

    -- * Smart constructor
    mkProvisionedProductPlanDetails,

    -- * Lenses
    pppdStatus,
    pppdProvisionProductId,
    pppdProvisioningArtifactId,
    pppdProvisionProductName,
    pppdCreatedTime,
    pppdNotificationARNs,
    pppdPlanId,
    pppdPlanName,
    pppdStatusMessage,
    pppdUpdatedTime,
    pppdPathId,
    pppdProvisioningParameters,
    pppdPlanType,
    pppdProductId,
    pppdTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanStatus
import Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanType
import Network.AWS.ServiceCatalog.Types.Tag
import Network.AWS.ServiceCatalog.Types.UpdateProvisioningParameter

-- | Information about a plan.
--
-- /See:/ 'mkProvisionedProductPlanDetails' smart constructor.
data ProvisionedProductPlanDetails = ProvisionedProductPlanDetails'
  { status ::
      Lude.Maybe
        ProvisionedProductPlanStatus,
    provisionProductId ::
      Lude.Maybe Lude.Text,
    provisioningArtifactId ::
      Lude.Maybe Lude.Text,
    provisionProductName ::
      Lude.Maybe Lude.Text,
    createdTime ::
      Lude.Maybe Lude.Timestamp,
    notificationARNs ::
      Lude.Maybe [Lude.Text],
    planId :: Lude.Maybe Lude.Text,
    planName ::
      Lude.Maybe Lude.Text,
    statusMessage ::
      Lude.Maybe Lude.Text,
    updatedTime ::
      Lude.Maybe Lude.Timestamp,
    pathId :: Lude.Maybe Lude.Text,
    provisioningParameters ::
      Lude.Maybe
        [UpdateProvisioningParameter],
    planType ::
      Lude.Maybe
        ProvisionedProductPlanType,
    productId ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ProvisionedProductPlanDetails' with the minimum fields required to make a request.
--
-- * 'createdTime' - The UTC time stamp of the creation time.
-- * 'notificationARNs' - Passed to CloudFormation. The SNS topic ARNs to which to publish stack-related events.
-- * 'pathId' - The path identifier of the product. This value is optional if the product has a default path, and required if the product has more than one path. To list the paths for a product, use 'ListLaunchPaths' .
-- * 'planId' - The plan identifier.
-- * 'planName' - The name of the plan.
-- * 'planType' - The plan type.
-- * 'productId' - The product identifier.
-- * 'provisionProductId' - The product identifier.
-- * 'provisionProductName' - The user-friendly name of the provisioned product.
-- * 'provisioningArtifactId' - The identifier of the provisioning artifact.
-- * 'provisioningParameters' - Parameters specified by the administrator that are required for provisioning the product.
-- * 'status' - The status.
-- * 'statusMessage' - The status message.
-- * 'tags' - One or more tags.
-- * 'updatedTime' - The time when the plan was last updated.
mkProvisionedProductPlanDetails ::
  ProvisionedProductPlanDetails
mkProvisionedProductPlanDetails =
  ProvisionedProductPlanDetails'
    { status = Lude.Nothing,
      provisionProductId = Lude.Nothing,
      provisioningArtifactId = Lude.Nothing,
      provisionProductName = Lude.Nothing,
      createdTime = Lude.Nothing,
      notificationARNs = Lude.Nothing,
      planId = Lude.Nothing,
      planName = Lude.Nothing,
      statusMessage = Lude.Nothing,
      updatedTime = Lude.Nothing,
      pathId = Lude.Nothing,
      provisioningParameters = Lude.Nothing,
      planType = Lude.Nothing,
      productId = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pppdStatus :: Lens.Lens' ProvisionedProductPlanDetails (Lude.Maybe ProvisionedProductPlanStatus)
pppdStatus = Lens.lens (status :: ProvisionedProductPlanDetails -> Lude.Maybe ProvisionedProductPlanStatus) (\s a -> s {status = a} :: ProvisionedProductPlanDetails)
{-# DEPRECATED pppdStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The product identifier.
--
-- /Note:/ Consider using 'provisionProductId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pppdProvisionProductId :: Lens.Lens' ProvisionedProductPlanDetails (Lude.Maybe Lude.Text)
pppdProvisionProductId = Lens.lens (provisionProductId :: ProvisionedProductPlanDetails -> Lude.Maybe Lude.Text) (\s a -> s {provisionProductId = a} :: ProvisionedProductPlanDetails)
{-# DEPRECATED pppdProvisionProductId "Use generic-lens or generic-optics with 'provisionProductId' instead." #-}

-- | The identifier of the provisioning artifact.
--
-- /Note:/ Consider using 'provisioningArtifactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pppdProvisioningArtifactId :: Lens.Lens' ProvisionedProductPlanDetails (Lude.Maybe Lude.Text)
pppdProvisioningArtifactId = Lens.lens (provisioningArtifactId :: ProvisionedProductPlanDetails -> Lude.Maybe Lude.Text) (\s a -> s {provisioningArtifactId = a} :: ProvisionedProductPlanDetails)
{-# DEPRECATED pppdProvisioningArtifactId "Use generic-lens or generic-optics with 'provisioningArtifactId' instead." #-}

-- | The user-friendly name of the provisioned product.
--
-- /Note:/ Consider using 'provisionProductName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pppdProvisionProductName :: Lens.Lens' ProvisionedProductPlanDetails (Lude.Maybe Lude.Text)
pppdProvisionProductName = Lens.lens (provisionProductName :: ProvisionedProductPlanDetails -> Lude.Maybe Lude.Text) (\s a -> s {provisionProductName = a} :: ProvisionedProductPlanDetails)
{-# DEPRECATED pppdProvisionProductName "Use generic-lens or generic-optics with 'provisionProductName' instead." #-}

-- | The UTC time stamp of the creation time.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pppdCreatedTime :: Lens.Lens' ProvisionedProductPlanDetails (Lude.Maybe Lude.Timestamp)
pppdCreatedTime = Lens.lens (createdTime :: ProvisionedProductPlanDetails -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdTime = a} :: ProvisionedProductPlanDetails)
{-# DEPRECATED pppdCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | Passed to CloudFormation. The SNS topic ARNs to which to publish stack-related events.
--
-- /Note:/ Consider using 'notificationARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pppdNotificationARNs :: Lens.Lens' ProvisionedProductPlanDetails (Lude.Maybe [Lude.Text])
pppdNotificationARNs = Lens.lens (notificationARNs :: ProvisionedProductPlanDetails -> Lude.Maybe [Lude.Text]) (\s a -> s {notificationARNs = a} :: ProvisionedProductPlanDetails)
{-# DEPRECATED pppdNotificationARNs "Use generic-lens or generic-optics with 'notificationARNs' instead." #-}

-- | The plan identifier.
--
-- /Note:/ Consider using 'planId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pppdPlanId :: Lens.Lens' ProvisionedProductPlanDetails (Lude.Maybe Lude.Text)
pppdPlanId = Lens.lens (planId :: ProvisionedProductPlanDetails -> Lude.Maybe Lude.Text) (\s a -> s {planId = a} :: ProvisionedProductPlanDetails)
{-# DEPRECATED pppdPlanId "Use generic-lens or generic-optics with 'planId' instead." #-}

-- | The name of the plan.
--
-- /Note:/ Consider using 'planName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pppdPlanName :: Lens.Lens' ProvisionedProductPlanDetails (Lude.Maybe Lude.Text)
pppdPlanName = Lens.lens (planName :: ProvisionedProductPlanDetails -> Lude.Maybe Lude.Text) (\s a -> s {planName = a} :: ProvisionedProductPlanDetails)
{-# DEPRECATED pppdPlanName "Use generic-lens or generic-optics with 'planName' instead." #-}

-- | The status message.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pppdStatusMessage :: Lens.Lens' ProvisionedProductPlanDetails (Lude.Maybe Lude.Text)
pppdStatusMessage = Lens.lens (statusMessage :: ProvisionedProductPlanDetails -> Lude.Maybe Lude.Text) (\s a -> s {statusMessage = a} :: ProvisionedProductPlanDetails)
{-# DEPRECATED pppdStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | The time when the plan was last updated.
--
-- /Note:/ Consider using 'updatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pppdUpdatedTime :: Lens.Lens' ProvisionedProductPlanDetails (Lude.Maybe Lude.Timestamp)
pppdUpdatedTime = Lens.lens (updatedTime :: ProvisionedProductPlanDetails -> Lude.Maybe Lude.Timestamp) (\s a -> s {updatedTime = a} :: ProvisionedProductPlanDetails)
{-# DEPRECATED pppdUpdatedTime "Use generic-lens or generic-optics with 'updatedTime' instead." #-}

-- | The path identifier of the product. This value is optional if the product has a default path, and required if the product has more than one path. To list the paths for a product, use 'ListLaunchPaths' .
--
-- /Note:/ Consider using 'pathId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pppdPathId :: Lens.Lens' ProvisionedProductPlanDetails (Lude.Maybe Lude.Text)
pppdPathId = Lens.lens (pathId :: ProvisionedProductPlanDetails -> Lude.Maybe Lude.Text) (\s a -> s {pathId = a} :: ProvisionedProductPlanDetails)
{-# DEPRECATED pppdPathId "Use generic-lens or generic-optics with 'pathId' instead." #-}

-- | Parameters specified by the administrator that are required for provisioning the product.
--
-- /Note:/ Consider using 'provisioningParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pppdProvisioningParameters :: Lens.Lens' ProvisionedProductPlanDetails (Lude.Maybe [UpdateProvisioningParameter])
pppdProvisioningParameters = Lens.lens (provisioningParameters :: ProvisionedProductPlanDetails -> Lude.Maybe [UpdateProvisioningParameter]) (\s a -> s {provisioningParameters = a} :: ProvisionedProductPlanDetails)
{-# DEPRECATED pppdProvisioningParameters "Use generic-lens or generic-optics with 'provisioningParameters' instead." #-}

-- | The plan type.
--
-- /Note:/ Consider using 'planType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pppdPlanType :: Lens.Lens' ProvisionedProductPlanDetails (Lude.Maybe ProvisionedProductPlanType)
pppdPlanType = Lens.lens (planType :: ProvisionedProductPlanDetails -> Lude.Maybe ProvisionedProductPlanType) (\s a -> s {planType = a} :: ProvisionedProductPlanDetails)
{-# DEPRECATED pppdPlanType "Use generic-lens or generic-optics with 'planType' instead." #-}

-- | The product identifier.
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pppdProductId :: Lens.Lens' ProvisionedProductPlanDetails (Lude.Maybe Lude.Text)
pppdProductId = Lens.lens (productId :: ProvisionedProductPlanDetails -> Lude.Maybe Lude.Text) (\s a -> s {productId = a} :: ProvisionedProductPlanDetails)
{-# DEPRECATED pppdProductId "Use generic-lens or generic-optics with 'productId' instead." #-}

-- | One or more tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pppdTags :: Lens.Lens' ProvisionedProductPlanDetails (Lude.Maybe [Tag])
pppdTags = Lens.lens (tags :: ProvisionedProductPlanDetails -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: ProvisionedProductPlanDetails)
{-# DEPRECATED pppdTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON ProvisionedProductPlanDetails where
  parseJSON =
    Lude.withObject
      "ProvisionedProductPlanDetails"
      ( \x ->
          ProvisionedProductPlanDetails'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "ProvisionProductId")
            Lude.<*> (x Lude..:? "ProvisioningArtifactId")
            Lude.<*> (x Lude..:? "ProvisionProductName")
            Lude.<*> (x Lude..:? "CreatedTime")
            Lude.<*> (x Lude..:? "NotificationArns" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "PlanId")
            Lude.<*> (x Lude..:? "PlanName")
            Lude.<*> (x Lude..:? "StatusMessage")
            Lude.<*> (x Lude..:? "UpdatedTime")
            Lude.<*> (x Lude..:? "PathId")
            Lude.<*> (x Lude..:? "ProvisioningParameters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "PlanType")
            Lude.<*> (x Lude..:? "ProductId")
            Lude.<*> (x Lude..:? "Tags" Lude..!= Lude.mempty)
      )
