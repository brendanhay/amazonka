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
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.ServiceCatalog.Types.RecordError
import Network.AWS.ServiceCatalog.Types.RecordStatus
import Network.AWS.ServiceCatalog.Types.RecordTag

-- | Information about a request operation.
--
-- /See:/ 'newRecordDetail' smart constructor.
data RecordDetail = RecordDetail'
  { -- | The ARN of the launch role associated with the provisioned product.
    launchRoleArn :: Prelude.Maybe Prelude.Text,
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
    status :: Prelude.Maybe RecordStatus,
    -- | One or more tags.
    recordTags :: Prelude.Maybe [RecordTag],
    -- | The user-friendly name of the provisioned product.
    provisionedProductName :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the provisioning artifact.
    provisioningArtifactId :: Prelude.Maybe Prelude.Text,
    -- | The UTC time stamp of the creation time.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | The record type.
    --
    -- -   @PROVISION_PRODUCT@
    --
    -- -   @UPDATE_PROVISIONED_PRODUCT@
    --
    -- -   @TERMINATE_PROVISIONED_PRODUCT@
    recordType :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the record.
    recordId :: Prelude.Maybe Prelude.Text,
    -- | The type of provisioned product. The supported values are @CFN_STACK@
    -- and @CFN_STACKSET@.
    provisionedProductType :: Prelude.Maybe Prelude.Text,
    -- | The time when the record was last updated.
    updatedTime :: Prelude.Maybe Core.POSIX,
    -- | The path identifier.
    pathId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the provisioned product.
    provisionedProductId :: Prelude.Maybe Prelude.Text,
    -- | The errors that occurred.
    recordErrors :: Prelude.Maybe [RecordError],
    -- | The product identifier.
    productId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecordDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchRoleArn', 'recordDetail_launchRoleArn' - The ARN of the launch role associated with the provisioned product.
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
-- 'recordTags', 'recordDetail_recordTags' - One or more tags.
--
-- 'provisionedProductName', 'recordDetail_provisionedProductName' - The user-friendly name of the provisioned product.
--
-- 'provisioningArtifactId', 'recordDetail_provisioningArtifactId' - The identifier of the provisioning artifact.
--
-- 'createdTime', 'recordDetail_createdTime' - The UTC time stamp of the creation time.
--
-- 'recordType', 'recordDetail_recordType' - The record type.
--
-- -   @PROVISION_PRODUCT@
--
-- -   @UPDATE_PROVISIONED_PRODUCT@
--
-- -   @TERMINATE_PROVISIONED_PRODUCT@
--
-- 'recordId', 'recordDetail_recordId' - The identifier of the record.
--
-- 'provisionedProductType', 'recordDetail_provisionedProductType' - The type of provisioned product. The supported values are @CFN_STACK@
-- and @CFN_STACKSET@.
--
-- 'updatedTime', 'recordDetail_updatedTime' - The time when the record was last updated.
--
-- 'pathId', 'recordDetail_pathId' - The path identifier.
--
-- 'provisionedProductId', 'recordDetail_provisionedProductId' - The identifier of the provisioned product.
--
-- 'recordErrors', 'recordDetail_recordErrors' - The errors that occurred.
--
-- 'productId', 'recordDetail_productId' - The product identifier.
newRecordDetail ::
  RecordDetail
newRecordDetail =
  RecordDetail'
    { launchRoleArn = Prelude.Nothing,
      status = Prelude.Nothing,
      recordTags = Prelude.Nothing,
      provisionedProductName = Prelude.Nothing,
      provisioningArtifactId = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      recordType = Prelude.Nothing,
      recordId = Prelude.Nothing,
      provisionedProductType = Prelude.Nothing,
      updatedTime = Prelude.Nothing,
      pathId = Prelude.Nothing,
      provisionedProductId = Prelude.Nothing,
      recordErrors = Prelude.Nothing,
      productId = Prelude.Nothing
    }

-- | The ARN of the launch role associated with the provisioned product.
recordDetail_launchRoleArn :: Lens.Lens' RecordDetail (Prelude.Maybe Prelude.Text)
recordDetail_launchRoleArn = Lens.lens (\RecordDetail' {launchRoleArn} -> launchRoleArn) (\s@RecordDetail' {} a -> s {launchRoleArn = a} :: RecordDetail)

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
recordDetail_status :: Lens.Lens' RecordDetail (Prelude.Maybe RecordStatus)
recordDetail_status = Lens.lens (\RecordDetail' {status} -> status) (\s@RecordDetail' {} a -> s {status = a} :: RecordDetail)

-- | One or more tags.
recordDetail_recordTags :: Lens.Lens' RecordDetail (Prelude.Maybe [RecordTag])
recordDetail_recordTags = Lens.lens (\RecordDetail' {recordTags} -> recordTags) (\s@RecordDetail' {} a -> s {recordTags = a} :: RecordDetail) Prelude.. Lens.mapping Lens.coerced

-- | The user-friendly name of the provisioned product.
recordDetail_provisionedProductName :: Lens.Lens' RecordDetail (Prelude.Maybe Prelude.Text)
recordDetail_provisionedProductName = Lens.lens (\RecordDetail' {provisionedProductName} -> provisionedProductName) (\s@RecordDetail' {} a -> s {provisionedProductName = a} :: RecordDetail)

-- | The identifier of the provisioning artifact.
recordDetail_provisioningArtifactId :: Lens.Lens' RecordDetail (Prelude.Maybe Prelude.Text)
recordDetail_provisioningArtifactId = Lens.lens (\RecordDetail' {provisioningArtifactId} -> provisioningArtifactId) (\s@RecordDetail' {} a -> s {provisioningArtifactId = a} :: RecordDetail)

-- | The UTC time stamp of the creation time.
recordDetail_createdTime :: Lens.Lens' RecordDetail (Prelude.Maybe Prelude.UTCTime)
recordDetail_createdTime = Lens.lens (\RecordDetail' {createdTime} -> createdTime) (\s@RecordDetail' {} a -> s {createdTime = a} :: RecordDetail) Prelude.. Lens.mapping Core._Time

-- | The record type.
--
-- -   @PROVISION_PRODUCT@
--
-- -   @UPDATE_PROVISIONED_PRODUCT@
--
-- -   @TERMINATE_PROVISIONED_PRODUCT@
recordDetail_recordType :: Lens.Lens' RecordDetail (Prelude.Maybe Prelude.Text)
recordDetail_recordType = Lens.lens (\RecordDetail' {recordType} -> recordType) (\s@RecordDetail' {} a -> s {recordType = a} :: RecordDetail)

-- | The identifier of the record.
recordDetail_recordId :: Lens.Lens' RecordDetail (Prelude.Maybe Prelude.Text)
recordDetail_recordId = Lens.lens (\RecordDetail' {recordId} -> recordId) (\s@RecordDetail' {} a -> s {recordId = a} :: RecordDetail)

-- | The type of provisioned product. The supported values are @CFN_STACK@
-- and @CFN_STACKSET@.
recordDetail_provisionedProductType :: Lens.Lens' RecordDetail (Prelude.Maybe Prelude.Text)
recordDetail_provisionedProductType = Lens.lens (\RecordDetail' {provisionedProductType} -> provisionedProductType) (\s@RecordDetail' {} a -> s {provisionedProductType = a} :: RecordDetail)

-- | The time when the record was last updated.
recordDetail_updatedTime :: Lens.Lens' RecordDetail (Prelude.Maybe Prelude.UTCTime)
recordDetail_updatedTime = Lens.lens (\RecordDetail' {updatedTime} -> updatedTime) (\s@RecordDetail' {} a -> s {updatedTime = a} :: RecordDetail) Prelude.. Lens.mapping Core._Time

-- | The path identifier.
recordDetail_pathId :: Lens.Lens' RecordDetail (Prelude.Maybe Prelude.Text)
recordDetail_pathId = Lens.lens (\RecordDetail' {pathId} -> pathId) (\s@RecordDetail' {} a -> s {pathId = a} :: RecordDetail)

-- | The identifier of the provisioned product.
recordDetail_provisionedProductId :: Lens.Lens' RecordDetail (Prelude.Maybe Prelude.Text)
recordDetail_provisionedProductId = Lens.lens (\RecordDetail' {provisionedProductId} -> provisionedProductId) (\s@RecordDetail' {} a -> s {provisionedProductId = a} :: RecordDetail)

-- | The errors that occurred.
recordDetail_recordErrors :: Lens.Lens' RecordDetail (Prelude.Maybe [RecordError])
recordDetail_recordErrors = Lens.lens (\RecordDetail' {recordErrors} -> recordErrors) (\s@RecordDetail' {} a -> s {recordErrors = a} :: RecordDetail) Prelude.. Lens.mapping Lens.coerced

-- | The product identifier.
recordDetail_productId :: Lens.Lens' RecordDetail (Prelude.Maybe Prelude.Text)
recordDetail_productId = Lens.lens (\RecordDetail' {productId} -> productId) (\s@RecordDetail' {} a -> s {productId = a} :: RecordDetail)

instance Core.FromJSON RecordDetail where
  parseJSON =
    Core.withObject
      "RecordDetail"
      ( \x ->
          RecordDetail'
            Prelude.<$> (x Core..:? "LaunchRoleArn")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "RecordTags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ProvisionedProductName")
            Prelude.<*> (x Core..:? "ProvisioningArtifactId")
            Prelude.<*> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "RecordType")
            Prelude.<*> (x Core..:? "RecordId")
            Prelude.<*> (x Core..:? "ProvisionedProductType")
            Prelude.<*> (x Core..:? "UpdatedTime")
            Prelude.<*> (x Core..:? "PathId")
            Prelude.<*> (x Core..:? "ProvisionedProductId")
            Prelude.<*> (x Core..:? "RecordErrors" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ProductId")
      )

instance Prelude.Hashable RecordDetail

instance Prelude.NFData RecordDetail
