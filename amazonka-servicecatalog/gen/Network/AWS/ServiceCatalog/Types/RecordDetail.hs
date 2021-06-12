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
-- Module      : Network.AWS.ServiceCatalog.Types.RecordDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.RecordDetail where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.ServiceCatalog.Types.RecordError
import Network.AWS.ServiceCatalog.Types.RecordStatus
import Network.AWS.ServiceCatalog.Types.RecordTag

-- | Information about a request operation.
--
-- /See:/ 'newRecordDetail' smart constructor.
data RecordDetail = RecordDetail'
  { -- | One or more tags.
    recordTags :: Core.Maybe [RecordTag],
    -- | The status of the provisioned product.
    --
    -- -   @CREATED@ - The request was created but the operation has not
    --     started.
    --
    -- -   @IN_PROGRESS@ - The requested operation is in progress.
    --
    -- -   @IN_PROGRESS_IN_ERROR@ - The provisioned product is under change but
    --     the requested operation failed and some remediation is occurring.
    --     For example, a rollback.
    --
    -- -   @SUCCEEDED@ - The requested operation has successfully completed.
    --
    -- -   @FAILED@ - The requested operation has unsuccessfully completed.
    --     Investigate using the error messages returned.
    status :: Core.Maybe RecordStatus,
    -- | The user-friendly name of the provisioned product.
    provisionedProductName :: Core.Maybe Core.Text,
    -- | The errors that occurred.
    recordErrors :: Core.Maybe [RecordError],
    -- | The identifier of the provisioned product.
    provisionedProductId :: Core.Maybe Core.Text,
    -- | The type of provisioned product. The supported values are @CFN_STACK@
    -- and @CFN_STACKSET@.
    provisionedProductType :: Core.Maybe Core.Text,
    -- | The identifier of the record.
    recordId :: Core.Maybe Core.Text,
    -- | The time when the record was last updated.
    updatedTime :: Core.Maybe Core.POSIX,
    -- | The UTC time stamp of the creation time.
    createdTime :: Core.Maybe Core.POSIX,
    -- | The identifier of the provisioning artifact.
    provisioningArtifactId :: Core.Maybe Core.Text,
    -- | The ARN of the launch role associated with the provisioned product.
    launchRoleArn :: Core.Maybe Core.Text,
    -- | The product identifier.
    productId :: Core.Maybe Core.Text,
    -- | The path identifier.
    pathId :: Core.Maybe Core.Text,
    -- | The record type.
    --
    -- -   @PROVISION_PRODUCT@
    --
    -- -   @UPDATE_PROVISIONED_PRODUCT@
    --
    -- -   @TERMINATE_PROVISIONED_PRODUCT@
    recordType :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RecordDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recordTags', 'recordDetail_recordTags' - One or more tags.
--
-- 'status', 'recordDetail_status' - The status of the provisioned product.
--
-- -   @CREATED@ - The request was created but the operation has not
--     started.
--
-- -   @IN_PROGRESS@ - The requested operation is in progress.
--
-- -   @IN_PROGRESS_IN_ERROR@ - The provisioned product is under change but
--     the requested operation failed and some remediation is occurring.
--     For example, a rollback.
--
-- -   @SUCCEEDED@ - The requested operation has successfully completed.
--
-- -   @FAILED@ - The requested operation has unsuccessfully completed.
--     Investigate using the error messages returned.
--
-- 'provisionedProductName', 'recordDetail_provisionedProductName' - The user-friendly name of the provisioned product.
--
-- 'recordErrors', 'recordDetail_recordErrors' - The errors that occurred.
--
-- 'provisionedProductId', 'recordDetail_provisionedProductId' - The identifier of the provisioned product.
--
-- 'provisionedProductType', 'recordDetail_provisionedProductType' - The type of provisioned product. The supported values are @CFN_STACK@
-- and @CFN_STACKSET@.
--
-- 'recordId', 'recordDetail_recordId' - The identifier of the record.
--
-- 'updatedTime', 'recordDetail_updatedTime' - The time when the record was last updated.
--
-- 'createdTime', 'recordDetail_createdTime' - The UTC time stamp of the creation time.
--
-- 'provisioningArtifactId', 'recordDetail_provisioningArtifactId' - The identifier of the provisioning artifact.
--
-- 'launchRoleArn', 'recordDetail_launchRoleArn' - The ARN of the launch role associated with the provisioned product.
--
-- 'productId', 'recordDetail_productId' - The product identifier.
--
-- 'pathId', 'recordDetail_pathId' - The path identifier.
--
-- 'recordType', 'recordDetail_recordType' - The record type.
--
-- -   @PROVISION_PRODUCT@
--
-- -   @UPDATE_PROVISIONED_PRODUCT@
--
-- -   @TERMINATE_PROVISIONED_PRODUCT@
newRecordDetail ::
  RecordDetail
newRecordDetail =
  RecordDetail'
    { recordTags = Core.Nothing,
      status = Core.Nothing,
      provisionedProductName = Core.Nothing,
      recordErrors = Core.Nothing,
      provisionedProductId = Core.Nothing,
      provisionedProductType = Core.Nothing,
      recordId = Core.Nothing,
      updatedTime = Core.Nothing,
      createdTime = Core.Nothing,
      provisioningArtifactId = Core.Nothing,
      launchRoleArn = Core.Nothing,
      productId = Core.Nothing,
      pathId = Core.Nothing,
      recordType = Core.Nothing
    }

-- | One or more tags.
recordDetail_recordTags :: Lens.Lens' RecordDetail (Core.Maybe [RecordTag])
recordDetail_recordTags = Lens.lens (\RecordDetail' {recordTags} -> recordTags) (\s@RecordDetail' {} a -> s {recordTags = a} :: RecordDetail) Core.. Lens.mapping Lens._Coerce

-- | The status of the provisioned product.
--
-- -   @CREATED@ - The request was created but the operation has not
--     started.
--
-- -   @IN_PROGRESS@ - The requested operation is in progress.
--
-- -   @IN_PROGRESS_IN_ERROR@ - The provisioned product is under change but
--     the requested operation failed and some remediation is occurring.
--     For example, a rollback.
--
-- -   @SUCCEEDED@ - The requested operation has successfully completed.
--
-- -   @FAILED@ - The requested operation has unsuccessfully completed.
--     Investigate using the error messages returned.
recordDetail_status :: Lens.Lens' RecordDetail (Core.Maybe RecordStatus)
recordDetail_status = Lens.lens (\RecordDetail' {status} -> status) (\s@RecordDetail' {} a -> s {status = a} :: RecordDetail)

-- | The user-friendly name of the provisioned product.
recordDetail_provisionedProductName :: Lens.Lens' RecordDetail (Core.Maybe Core.Text)
recordDetail_provisionedProductName = Lens.lens (\RecordDetail' {provisionedProductName} -> provisionedProductName) (\s@RecordDetail' {} a -> s {provisionedProductName = a} :: RecordDetail)

-- | The errors that occurred.
recordDetail_recordErrors :: Lens.Lens' RecordDetail (Core.Maybe [RecordError])
recordDetail_recordErrors = Lens.lens (\RecordDetail' {recordErrors} -> recordErrors) (\s@RecordDetail' {} a -> s {recordErrors = a} :: RecordDetail) Core.. Lens.mapping Lens._Coerce

-- | The identifier of the provisioned product.
recordDetail_provisionedProductId :: Lens.Lens' RecordDetail (Core.Maybe Core.Text)
recordDetail_provisionedProductId = Lens.lens (\RecordDetail' {provisionedProductId} -> provisionedProductId) (\s@RecordDetail' {} a -> s {provisionedProductId = a} :: RecordDetail)

-- | The type of provisioned product. The supported values are @CFN_STACK@
-- and @CFN_STACKSET@.
recordDetail_provisionedProductType :: Lens.Lens' RecordDetail (Core.Maybe Core.Text)
recordDetail_provisionedProductType = Lens.lens (\RecordDetail' {provisionedProductType} -> provisionedProductType) (\s@RecordDetail' {} a -> s {provisionedProductType = a} :: RecordDetail)

-- | The identifier of the record.
recordDetail_recordId :: Lens.Lens' RecordDetail (Core.Maybe Core.Text)
recordDetail_recordId = Lens.lens (\RecordDetail' {recordId} -> recordId) (\s@RecordDetail' {} a -> s {recordId = a} :: RecordDetail)

-- | The time when the record was last updated.
recordDetail_updatedTime :: Lens.Lens' RecordDetail (Core.Maybe Core.UTCTime)
recordDetail_updatedTime = Lens.lens (\RecordDetail' {updatedTime} -> updatedTime) (\s@RecordDetail' {} a -> s {updatedTime = a} :: RecordDetail) Core.. Lens.mapping Core._Time

-- | The UTC time stamp of the creation time.
recordDetail_createdTime :: Lens.Lens' RecordDetail (Core.Maybe Core.UTCTime)
recordDetail_createdTime = Lens.lens (\RecordDetail' {createdTime} -> createdTime) (\s@RecordDetail' {} a -> s {createdTime = a} :: RecordDetail) Core.. Lens.mapping Core._Time

-- | The identifier of the provisioning artifact.
recordDetail_provisioningArtifactId :: Lens.Lens' RecordDetail (Core.Maybe Core.Text)
recordDetail_provisioningArtifactId = Lens.lens (\RecordDetail' {provisioningArtifactId} -> provisioningArtifactId) (\s@RecordDetail' {} a -> s {provisioningArtifactId = a} :: RecordDetail)

-- | The ARN of the launch role associated with the provisioned product.
recordDetail_launchRoleArn :: Lens.Lens' RecordDetail (Core.Maybe Core.Text)
recordDetail_launchRoleArn = Lens.lens (\RecordDetail' {launchRoleArn} -> launchRoleArn) (\s@RecordDetail' {} a -> s {launchRoleArn = a} :: RecordDetail)

-- | The product identifier.
recordDetail_productId :: Lens.Lens' RecordDetail (Core.Maybe Core.Text)
recordDetail_productId = Lens.lens (\RecordDetail' {productId} -> productId) (\s@RecordDetail' {} a -> s {productId = a} :: RecordDetail)

-- | The path identifier.
recordDetail_pathId :: Lens.Lens' RecordDetail (Core.Maybe Core.Text)
recordDetail_pathId = Lens.lens (\RecordDetail' {pathId} -> pathId) (\s@RecordDetail' {} a -> s {pathId = a} :: RecordDetail)

-- | The record type.
--
-- -   @PROVISION_PRODUCT@
--
-- -   @UPDATE_PROVISIONED_PRODUCT@
--
-- -   @TERMINATE_PROVISIONED_PRODUCT@
recordDetail_recordType :: Lens.Lens' RecordDetail (Core.Maybe Core.Text)
recordDetail_recordType = Lens.lens (\RecordDetail' {recordType} -> recordType) (\s@RecordDetail' {} a -> s {recordType = a} :: RecordDetail)

instance Core.FromJSON RecordDetail where
  parseJSON =
    Core.withObject
      "RecordDetail"
      ( \x ->
          RecordDetail'
            Core.<$> (x Core..:? "RecordTags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "ProvisionedProductName")
            Core.<*> (x Core..:? "RecordErrors" Core..!= Core.mempty)
            Core.<*> (x Core..:? "ProvisionedProductId")
            Core.<*> (x Core..:? "ProvisionedProductType")
            Core.<*> (x Core..:? "RecordId")
            Core.<*> (x Core..:? "UpdatedTime")
            Core.<*> (x Core..:? "CreatedTime")
            Core.<*> (x Core..:? "ProvisioningArtifactId")
            Core.<*> (x Core..:? "LaunchRoleArn")
            Core.<*> (x Core..:? "ProductId")
            Core.<*> (x Core..:? "PathId")
            Core.<*> (x Core..:? "RecordType")
      )

instance Core.Hashable RecordDetail

instance Core.NFData RecordDetail
