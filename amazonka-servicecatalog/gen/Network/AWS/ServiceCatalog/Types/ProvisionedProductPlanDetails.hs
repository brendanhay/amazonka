{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanStatus
import Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanType
import Network.AWS.ServiceCatalog.Types.Tag
import Network.AWS.ServiceCatalog.Types.UpdateProvisioningParameter

-- | Information about a plan.
--
-- /See:/ 'newProvisionedProductPlanDetails' smart constructor.
data ProvisionedProductPlanDetails = ProvisionedProductPlanDetails'
  { -- | The product identifier.
    provisionProductId :: Core.Maybe Core.Text,
    -- | The status message.
    statusMessage :: Core.Maybe Core.Text,
    -- | The status.
    status :: Core.Maybe ProvisionedProductPlanStatus,
    -- | Passed to CloudFormation. The SNS topic ARNs to which to publish
    -- stack-related events.
    notificationArns :: Core.Maybe [Core.Text],
    -- | The time when the plan was last updated.
    updatedTime :: Core.Maybe Core.POSIX,
    -- | The UTC time stamp of the creation time.
    createdTime :: Core.Maybe Core.POSIX,
    -- | The identifier of the provisioning artifact.
    provisioningArtifactId :: Core.Maybe Core.Text,
    -- | The name of the plan.
    planName :: Core.Maybe Core.Text,
    -- | One or more tags.
    tags :: Core.Maybe [Tag],
    -- | The plan identifier.
    planId :: Core.Maybe Core.Text,
    -- | The product identifier.
    productId :: Core.Maybe Core.Text,
    -- | Parameters specified by the administrator that are required for
    -- provisioning the product.
    provisioningParameters :: Core.Maybe [UpdateProvisioningParameter],
    -- | The plan type.
    planType :: Core.Maybe ProvisionedProductPlanType,
    -- | The path identifier of the product. This value is optional if the
    -- product has a default path, and required if the product has more than
    -- one path. To list the paths for a product, use ListLaunchPaths.
    pathId :: Core.Maybe Core.Text,
    -- | The user-friendly name of the provisioned product.
    provisionProductName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ProvisionedProductPlanDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'provisionProductId', 'provisionedProductPlanDetails_provisionProductId' - The product identifier.
--
-- 'statusMessage', 'provisionedProductPlanDetails_statusMessage' - The status message.
--
-- 'status', 'provisionedProductPlanDetails_status' - The status.
--
-- 'notificationArns', 'provisionedProductPlanDetails_notificationArns' - Passed to CloudFormation. The SNS topic ARNs to which to publish
-- stack-related events.
--
-- 'updatedTime', 'provisionedProductPlanDetails_updatedTime' - The time when the plan was last updated.
--
-- 'createdTime', 'provisionedProductPlanDetails_createdTime' - The UTC time stamp of the creation time.
--
-- 'provisioningArtifactId', 'provisionedProductPlanDetails_provisioningArtifactId' - The identifier of the provisioning artifact.
--
-- 'planName', 'provisionedProductPlanDetails_planName' - The name of the plan.
--
-- 'tags', 'provisionedProductPlanDetails_tags' - One or more tags.
--
-- 'planId', 'provisionedProductPlanDetails_planId' - The plan identifier.
--
-- 'productId', 'provisionedProductPlanDetails_productId' - The product identifier.
--
-- 'provisioningParameters', 'provisionedProductPlanDetails_provisioningParameters' - Parameters specified by the administrator that are required for
-- provisioning the product.
--
-- 'planType', 'provisionedProductPlanDetails_planType' - The plan type.
--
-- 'pathId', 'provisionedProductPlanDetails_pathId' - The path identifier of the product. This value is optional if the
-- product has a default path, and required if the product has more than
-- one path. To list the paths for a product, use ListLaunchPaths.
--
-- 'provisionProductName', 'provisionedProductPlanDetails_provisionProductName' - The user-friendly name of the provisioned product.
newProvisionedProductPlanDetails ::
  ProvisionedProductPlanDetails
newProvisionedProductPlanDetails =
  ProvisionedProductPlanDetails'
    { provisionProductId =
        Core.Nothing,
      statusMessage = Core.Nothing,
      status = Core.Nothing,
      notificationArns = Core.Nothing,
      updatedTime = Core.Nothing,
      createdTime = Core.Nothing,
      provisioningArtifactId = Core.Nothing,
      planName = Core.Nothing,
      tags = Core.Nothing,
      planId = Core.Nothing,
      productId = Core.Nothing,
      provisioningParameters = Core.Nothing,
      planType = Core.Nothing,
      pathId = Core.Nothing,
      provisionProductName = Core.Nothing
    }

-- | The product identifier.
provisionedProductPlanDetails_provisionProductId :: Lens.Lens' ProvisionedProductPlanDetails (Core.Maybe Core.Text)
provisionedProductPlanDetails_provisionProductId = Lens.lens (\ProvisionedProductPlanDetails' {provisionProductId} -> provisionProductId) (\s@ProvisionedProductPlanDetails' {} a -> s {provisionProductId = a} :: ProvisionedProductPlanDetails)

-- | The status message.
provisionedProductPlanDetails_statusMessage :: Lens.Lens' ProvisionedProductPlanDetails (Core.Maybe Core.Text)
provisionedProductPlanDetails_statusMessage = Lens.lens (\ProvisionedProductPlanDetails' {statusMessage} -> statusMessage) (\s@ProvisionedProductPlanDetails' {} a -> s {statusMessage = a} :: ProvisionedProductPlanDetails)

-- | The status.
provisionedProductPlanDetails_status :: Lens.Lens' ProvisionedProductPlanDetails (Core.Maybe ProvisionedProductPlanStatus)
provisionedProductPlanDetails_status = Lens.lens (\ProvisionedProductPlanDetails' {status} -> status) (\s@ProvisionedProductPlanDetails' {} a -> s {status = a} :: ProvisionedProductPlanDetails)

-- | Passed to CloudFormation. The SNS topic ARNs to which to publish
-- stack-related events.
provisionedProductPlanDetails_notificationArns :: Lens.Lens' ProvisionedProductPlanDetails (Core.Maybe [Core.Text])
provisionedProductPlanDetails_notificationArns = Lens.lens (\ProvisionedProductPlanDetails' {notificationArns} -> notificationArns) (\s@ProvisionedProductPlanDetails' {} a -> s {notificationArns = a} :: ProvisionedProductPlanDetails) Core.. Lens.mapping Lens._Coerce

-- | The time when the plan was last updated.
provisionedProductPlanDetails_updatedTime :: Lens.Lens' ProvisionedProductPlanDetails (Core.Maybe Core.UTCTime)
provisionedProductPlanDetails_updatedTime = Lens.lens (\ProvisionedProductPlanDetails' {updatedTime} -> updatedTime) (\s@ProvisionedProductPlanDetails' {} a -> s {updatedTime = a} :: ProvisionedProductPlanDetails) Core.. Lens.mapping Core._Time

-- | The UTC time stamp of the creation time.
provisionedProductPlanDetails_createdTime :: Lens.Lens' ProvisionedProductPlanDetails (Core.Maybe Core.UTCTime)
provisionedProductPlanDetails_createdTime = Lens.lens (\ProvisionedProductPlanDetails' {createdTime} -> createdTime) (\s@ProvisionedProductPlanDetails' {} a -> s {createdTime = a} :: ProvisionedProductPlanDetails) Core.. Lens.mapping Core._Time

-- | The identifier of the provisioning artifact.
provisionedProductPlanDetails_provisioningArtifactId :: Lens.Lens' ProvisionedProductPlanDetails (Core.Maybe Core.Text)
provisionedProductPlanDetails_provisioningArtifactId = Lens.lens (\ProvisionedProductPlanDetails' {provisioningArtifactId} -> provisioningArtifactId) (\s@ProvisionedProductPlanDetails' {} a -> s {provisioningArtifactId = a} :: ProvisionedProductPlanDetails)

-- | The name of the plan.
provisionedProductPlanDetails_planName :: Lens.Lens' ProvisionedProductPlanDetails (Core.Maybe Core.Text)
provisionedProductPlanDetails_planName = Lens.lens (\ProvisionedProductPlanDetails' {planName} -> planName) (\s@ProvisionedProductPlanDetails' {} a -> s {planName = a} :: ProvisionedProductPlanDetails)

-- | One or more tags.
provisionedProductPlanDetails_tags :: Lens.Lens' ProvisionedProductPlanDetails (Core.Maybe [Tag])
provisionedProductPlanDetails_tags = Lens.lens (\ProvisionedProductPlanDetails' {tags} -> tags) (\s@ProvisionedProductPlanDetails' {} a -> s {tags = a} :: ProvisionedProductPlanDetails) Core.. Lens.mapping Lens._Coerce

-- | The plan identifier.
provisionedProductPlanDetails_planId :: Lens.Lens' ProvisionedProductPlanDetails (Core.Maybe Core.Text)
provisionedProductPlanDetails_planId = Lens.lens (\ProvisionedProductPlanDetails' {planId} -> planId) (\s@ProvisionedProductPlanDetails' {} a -> s {planId = a} :: ProvisionedProductPlanDetails)

-- | The product identifier.
provisionedProductPlanDetails_productId :: Lens.Lens' ProvisionedProductPlanDetails (Core.Maybe Core.Text)
provisionedProductPlanDetails_productId = Lens.lens (\ProvisionedProductPlanDetails' {productId} -> productId) (\s@ProvisionedProductPlanDetails' {} a -> s {productId = a} :: ProvisionedProductPlanDetails)

-- | Parameters specified by the administrator that are required for
-- provisioning the product.
provisionedProductPlanDetails_provisioningParameters :: Lens.Lens' ProvisionedProductPlanDetails (Core.Maybe [UpdateProvisioningParameter])
provisionedProductPlanDetails_provisioningParameters = Lens.lens (\ProvisionedProductPlanDetails' {provisioningParameters} -> provisioningParameters) (\s@ProvisionedProductPlanDetails' {} a -> s {provisioningParameters = a} :: ProvisionedProductPlanDetails) Core.. Lens.mapping Lens._Coerce

-- | The plan type.
provisionedProductPlanDetails_planType :: Lens.Lens' ProvisionedProductPlanDetails (Core.Maybe ProvisionedProductPlanType)
provisionedProductPlanDetails_planType = Lens.lens (\ProvisionedProductPlanDetails' {planType} -> planType) (\s@ProvisionedProductPlanDetails' {} a -> s {planType = a} :: ProvisionedProductPlanDetails)

-- | The path identifier of the product. This value is optional if the
-- product has a default path, and required if the product has more than
-- one path. To list the paths for a product, use ListLaunchPaths.
provisionedProductPlanDetails_pathId :: Lens.Lens' ProvisionedProductPlanDetails (Core.Maybe Core.Text)
provisionedProductPlanDetails_pathId = Lens.lens (\ProvisionedProductPlanDetails' {pathId} -> pathId) (\s@ProvisionedProductPlanDetails' {} a -> s {pathId = a} :: ProvisionedProductPlanDetails)

-- | The user-friendly name of the provisioned product.
provisionedProductPlanDetails_provisionProductName :: Lens.Lens' ProvisionedProductPlanDetails (Core.Maybe Core.Text)
provisionedProductPlanDetails_provisionProductName = Lens.lens (\ProvisionedProductPlanDetails' {provisionProductName} -> provisionProductName) (\s@ProvisionedProductPlanDetails' {} a -> s {provisionProductName = a} :: ProvisionedProductPlanDetails)

instance Core.FromJSON ProvisionedProductPlanDetails where
  parseJSON =
    Core.withObject
      "ProvisionedProductPlanDetails"
      ( \x ->
          ProvisionedProductPlanDetails'
            Core.<$> (x Core..:? "ProvisionProductId")
            Core.<*> (x Core..:? "StatusMessage")
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "NotificationArns" Core..!= Core.mempty)
            Core.<*> (x Core..:? "UpdatedTime")
            Core.<*> (x Core..:? "CreatedTime")
            Core.<*> (x Core..:? "ProvisioningArtifactId")
            Core.<*> (x Core..:? "PlanName")
            Core.<*> (x Core..:? "Tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "PlanId")
            Core.<*> (x Core..:? "ProductId")
            Core.<*> ( x Core..:? "ProvisioningParameters"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "PlanType")
            Core.<*> (x Core..:? "PathId")
            Core.<*> (x Core..:? "ProvisionProductName")
      )

instance Core.Hashable ProvisionedProductPlanDetails

instance Core.NFData ProvisionedProductPlanDetails
