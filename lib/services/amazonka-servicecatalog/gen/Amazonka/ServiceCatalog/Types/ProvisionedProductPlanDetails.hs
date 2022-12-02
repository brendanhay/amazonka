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
-- Module      : Amazonka.ServiceCatalog.Types.ProvisionedProductPlanDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.ProvisionedProductPlanDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ServiceCatalog.Types.ProvisionedProductPlanStatus
import Amazonka.ServiceCatalog.Types.ProvisionedProductPlanType
import Amazonka.ServiceCatalog.Types.Tag
import Amazonka.ServiceCatalog.Types.UpdateProvisioningParameter

-- | Information about a plan.
--
-- /See:/ 'newProvisionedProductPlanDetails' smart constructor.
data ProvisionedProductPlanDetails = ProvisionedProductPlanDetails'
  { -- | One or more tags.
    tags :: Prelude.Maybe [Tag],
    -- | The path identifier of the product. This value is optional if the
    -- product has a default path, and required if the product has more than
    -- one path. To list the paths for a product, use ListLaunchPaths.
    pathId :: Prelude.Maybe Prelude.Text,
    -- | The UTC time stamp of the creation time.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | The plan identifier.
    planId :: Prelude.Maybe Prelude.Text,
    -- | The product identifier.
    provisionProductId :: Prelude.Maybe Prelude.Text,
    -- | The plan type.
    planType :: Prelude.Maybe ProvisionedProductPlanType,
    -- | Passed to CloudFormation. The SNS topic ARNs to which to publish
    -- stack-related events.
    notificationArns :: Prelude.Maybe [Prelude.Text],
    -- | The name of the plan.
    planName :: Prelude.Maybe Prelude.Text,
    -- | The product identifier.
    productId :: Prelude.Maybe Prelude.Text,
    -- | The status.
    status :: Prelude.Maybe ProvisionedProductPlanStatus,
    -- | Parameters specified by the administrator that are required for
    -- provisioning the product.
    provisioningParameters :: Prelude.Maybe [UpdateProvisioningParameter],
    -- | The identifier of the provisioning artifact.
    provisioningArtifactId :: Prelude.Maybe Prelude.Text,
    -- | The status message.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The user-friendly name of the provisioned product.
    provisionProductName :: Prelude.Maybe Prelude.Text,
    -- | The UTC time stamp when the plan was last updated.
    updatedTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProvisionedProductPlanDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'provisionedProductPlanDetails_tags' - One or more tags.
--
-- 'pathId', 'provisionedProductPlanDetails_pathId' - The path identifier of the product. This value is optional if the
-- product has a default path, and required if the product has more than
-- one path. To list the paths for a product, use ListLaunchPaths.
--
-- 'createdTime', 'provisionedProductPlanDetails_createdTime' - The UTC time stamp of the creation time.
--
-- 'planId', 'provisionedProductPlanDetails_planId' - The plan identifier.
--
-- 'provisionProductId', 'provisionedProductPlanDetails_provisionProductId' - The product identifier.
--
-- 'planType', 'provisionedProductPlanDetails_planType' - The plan type.
--
-- 'notificationArns', 'provisionedProductPlanDetails_notificationArns' - Passed to CloudFormation. The SNS topic ARNs to which to publish
-- stack-related events.
--
-- 'planName', 'provisionedProductPlanDetails_planName' - The name of the plan.
--
-- 'productId', 'provisionedProductPlanDetails_productId' - The product identifier.
--
-- 'status', 'provisionedProductPlanDetails_status' - The status.
--
-- 'provisioningParameters', 'provisionedProductPlanDetails_provisioningParameters' - Parameters specified by the administrator that are required for
-- provisioning the product.
--
-- 'provisioningArtifactId', 'provisionedProductPlanDetails_provisioningArtifactId' - The identifier of the provisioning artifact.
--
-- 'statusMessage', 'provisionedProductPlanDetails_statusMessage' - The status message.
--
-- 'provisionProductName', 'provisionedProductPlanDetails_provisionProductName' - The user-friendly name of the provisioned product.
--
-- 'updatedTime', 'provisionedProductPlanDetails_updatedTime' - The UTC time stamp when the plan was last updated.
newProvisionedProductPlanDetails ::
  ProvisionedProductPlanDetails
newProvisionedProductPlanDetails =
  ProvisionedProductPlanDetails'
    { tags =
        Prelude.Nothing,
      pathId = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      planId = Prelude.Nothing,
      provisionProductId = Prelude.Nothing,
      planType = Prelude.Nothing,
      notificationArns = Prelude.Nothing,
      planName = Prelude.Nothing,
      productId = Prelude.Nothing,
      status = Prelude.Nothing,
      provisioningParameters = Prelude.Nothing,
      provisioningArtifactId = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      provisionProductName = Prelude.Nothing,
      updatedTime = Prelude.Nothing
    }

-- | One or more tags.
provisionedProductPlanDetails_tags :: Lens.Lens' ProvisionedProductPlanDetails (Prelude.Maybe [Tag])
provisionedProductPlanDetails_tags = Lens.lens (\ProvisionedProductPlanDetails' {tags} -> tags) (\s@ProvisionedProductPlanDetails' {} a -> s {tags = a} :: ProvisionedProductPlanDetails) Prelude.. Lens.mapping Lens.coerced

-- | The path identifier of the product. This value is optional if the
-- product has a default path, and required if the product has more than
-- one path. To list the paths for a product, use ListLaunchPaths.
provisionedProductPlanDetails_pathId :: Lens.Lens' ProvisionedProductPlanDetails (Prelude.Maybe Prelude.Text)
provisionedProductPlanDetails_pathId = Lens.lens (\ProvisionedProductPlanDetails' {pathId} -> pathId) (\s@ProvisionedProductPlanDetails' {} a -> s {pathId = a} :: ProvisionedProductPlanDetails)

-- | The UTC time stamp of the creation time.
provisionedProductPlanDetails_createdTime :: Lens.Lens' ProvisionedProductPlanDetails (Prelude.Maybe Prelude.UTCTime)
provisionedProductPlanDetails_createdTime = Lens.lens (\ProvisionedProductPlanDetails' {createdTime} -> createdTime) (\s@ProvisionedProductPlanDetails' {} a -> s {createdTime = a} :: ProvisionedProductPlanDetails) Prelude.. Lens.mapping Data._Time

-- | The plan identifier.
provisionedProductPlanDetails_planId :: Lens.Lens' ProvisionedProductPlanDetails (Prelude.Maybe Prelude.Text)
provisionedProductPlanDetails_planId = Lens.lens (\ProvisionedProductPlanDetails' {planId} -> planId) (\s@ProvisionedProductPlanDetails' {} a -> s {planId = a} :: ProvisionedProductPlanDetails)

-- | The product identifier.
provisionedProductPlanDetails_provisionProductId :: Lens.Lens' ProvisionedProductPlanDetails (Prelude.Maybe Prelude.Text)
provisionedProductPlanDetails_provisionProductId = Lens.lens (\ProvisionedProductPlanDetails' {provisionProductId} -> provisionProductId) (\s@ProvisionedProductPlanDetails' {} a -> s {provisionProductId = a} :: ProvisionedProductPlanDetails)

-- | The plan type.
provisionedProductPlanDetails_planType :: Lens.Lens' ProvisionedProductPlanDetails (Prelude.Maybe ProvisionedProductPlanType)
provisionedProductPlanDetails_planType = Lens.lens (\ProvisionedProductPlanDetails' {planType} -> planType) (\s@ProvisionedProductPlanDetails' {} a -> s {planType = a} :: ProvisionedProductPlanDetails)

-- | Passed to CloudFormation. The SNS topic ARNs to which to publish
-- stack-related events.
provisionedProductPlanDetails_notificationArns :: Lens.Lens' ProvisionedProductPlanDetails (Prelude.Maybe [Prelude.Text])
provisionedProductPlanDetails_notificationArns = Lens.lens (\ProvisionedProductPlanDetails' {notificationArns} -> notificationArns) (\s@ProvisionedProductPlanDetails' {} a -> s {notificationArns = a} :: ProvisionedProductPlanDetails) Prelude.. Lens.mapping Lens.coerced

-- | The name of the plan.
provisionedProductPlanDetails_planName :: Lens.Lens' ProvisionedProductPlanDetails (Prelude.Maybe Prelude.Text)
provisionedProductPlanDetails_planName = Lens.lens (\ProvisionedProductPlanDetails' {planName} -> planName) (\s@ProvisionedProductPlanDetails' {} a -> s {planName = a} :: ProvisionedProductPlanDetails)

-- | The product identifier.
provisionedProductPlanDetails_productId :: Lens.Lens' ProvisionedProductPlanDetails (Prelude.Maybe Prelude.Text)
provisionedProductPlanDetails_productId = Lens.lens (\ProvisionedProductPlanDetails' {productId} -> productId) (\s@ProvisionedProductPlanDetails' {} a -> s {productId = a} :: ProvisionedProductPlanDetails)

-- | The status.
provisionedProductPlanDetails_status :: Lens.Lens' ProvisionedProductPlanDetails (Prelude.Maybe ProvisionedProductPlanStatus)
provisionedProductPlanDetails_status = Lens.lens (\ProvisionedProductPlanDetails' {status} -> status) (\s@ProvisionedProductPlanDetails' {} a -> s {status = a} :: ProvisionedProductPlanDetails)

-- | Parameters specified by the administrator that are required for
-- provisioning the product.
provisionedProductPlanDetails_provisioningParameters :: Lens.Lens' ProvisionedProductPlanDetails (Prelude.Maybe [UpdateProvisioningParameter])
provisionedProductPlanDetails_provisioningParameters = Lens.lens (\ProvisionedProductPlanDetails' {provisioningParameters} -> provisioningParameters) (\s@ProvisionedProductPlanDetails' {} a -> s {provisioningParameters = a} :: ProvisionedProductPlanDetails) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the provisioning artifact.
provisionedProductPlanDetails_provisioningArtifactId :: Lens.Lens' ProvisionedProductPlanDetails (Prelude.Maybe Prelude.Text)
provisionedProductPlanDetails_provisioningArtifactId = Lens.lens (\ProvisionedProductPlanDetails' {provisioningArtifactId} -> provisioningArtifactId) (\s@ProvisionedProductPlanDetails' {} a -> s {provisioningArtifactId = a} :: ProvisionedProductPlanDetails)

-- | The status message.
provisionedProductPlanDetails_statusMessage :: Lens.Lens' ProvisionedProductPlanDetails (Prelude.Maybe Prelude.Text)
provisionedProductPlanDetails_statusMessage = Lens.lens (\ProvisionedProductPlanDetails' {statusMessage} -> statusMessage) (\s@ProvisionedProductPlanDetails' {} a -> s {statusMessage = a} :: ProvisionedProductPlanDetails)

-- | The user-friendly name of the provisioned product.
provisionedProductPlanDetails_provisionProductName :: Lens.Lens' ProvisionedProductPlanDetails (Prelude.Maybe Prelude.Text)
provisionedProductPlanDetails_provisionProductName = Lens.lens (\ProvisionedProductPlanDetails' {provisionProductName} -> provisionProductName) (\s@ProvisionedProductPlanDetails' {} a -> s {provisionProductName = a} :: ProvisionedProductPlanDetails)

-- | The UTC time stamp when the plan was last updated.
provisionedProductPlanDetails_updatedTime :: Lens.Lens' ProvisionedProductPlanDetails (Prelude.Maybe Prelude.UTCTime)
provisionedProductPlanDetails_updatedTime = Lens.lens (\ProvisionedProductPlanDetails' {updatedTime} -> updatedTime) (\s@ProvisionedProductPlanDetails' {} a -> s {updatedTime = a} :: ProvisionedProductPlanDetails) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON ProvisionedProductPlanDetails where
  parseJSON =
    Data.withObject
      "ProvisionedProductPlanDetails"
      ( \x ->
          ProvisionedProductPlanDetails'
            Prelude.<$> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "PathId")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "PlanId")
            Prelude.<*> (x Data..:? "ProvisionProductId")
            Prelude.<*> (x Data..:? "PlanType")
            Prelude.<*> ( x Data..:? "NotificationArns"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "PlanName")
            Prelude.<*> (x Data..:? "ProductId")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> ( x Data..:? "ProvisioningParameters"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ProvisioningArtifactId")
            Prelude.<*> (x Data..:? "StatusMessage")
            Prelude.<*> (x Data..:? "ProvisionProductName")
            Prelude.<*> (x Data..:? "UpdatedTime")
      )

instance
  Prelude.Hashable
    ProvisionedProductPlanDetails
  where
  hashWithSalt _salt ProvisionedProductPlanDetails' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` pathId
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` planId
      `Prelude.hashWithSalt` provisionProductId
      `Prelude.hashWithSalt` planType
      `Prelude.hashWithSalt` notificationArns
      `Prelude.hashWithSalt` planName
      `Prelude.hashWithSalt` productId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` provisioningParameters
      `Prelude.hashWithSalt` provisioningArtifactId
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` provisionProductName
      `Prelude.hashWithSalt` updatedTime

instance Prelude.NFData ProvisionedProductPlanDetails where
  rnf ProvisionedProductPlanDetails' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf pathId
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf planId
      `Prelude.seq` Prelude.rnf provisionProductId
      `Prelude.seq` Prelude.rnf planType
      `Prelude.seq` Prelude.rnf notificationArns
      `Prelude.seq` Prelude.rnf planName
      `Prelude.seq` Prelude.rnf productId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf provisioningParameters
      `Prelude.seq` Prelude.rnf provisioningArtifactId
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf provisionProductName
      `Prelude.seq` Prelude.rnf updatedTime
