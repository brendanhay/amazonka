{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.ServiceCatalog.Types.RecordError
import Network.AWS.ServiceCatalog.Types.RecordStatus
import Network.AWS.ServiceCatalog.Types.RecordTag

-- | Information about a request operation.
--
-- /See:/ 'newRecordDetail' smart constructor.
data RecordDetail = RecordDetail'
  { -- | One or more tags.
    recordTags :: Prelude.Maybe [RecordTag],
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
    -- | The user-friendly name of the provisioned product.
    provisionedProductName :: Prelude.Maybe Prelude.Text,
    -- | The errors that occurred.
    recordErrors :: Prelude.Maybe [RecordError],
    -- | The identifier of the provisioned product.
    provisionedProductId :: Prelude.Maybe Prelude.Text,
    -- | The type of provisioned product. The supported values are @CFN_STACK@
    -- and @CFN_STACKSET@.
    provisionedProductType :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the record.
    recordId :: Prelude.Maybe Prelude.Text,
    -- | The time when the record was last updated.
    updatedTime :: Prelude.Maybe Prelude.POSIX,
    -- | The UTC time stamp of the creation time.
    createdTime :: Prelude.Maybe Prelude.POSIX,
    -- | The identifier of the provisioning artifact.
    provisioningArtifactId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the launch role associated with the provisioned product.
    launchRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The product identifier.
    productId :: Prelude.Maybe Prelude.Text,
    -- | The path identifier.
    pathId :: Prelude.Maybe Prelude.Text,
    -- | The record type.
    --
    -- -   @PROVISION_PRODUCT@
    --
    -- -   @UPDATE_PROVISIONED_PRODUCT@
    --
    -- -   @TERMINATE_PROVISIONED_PRODUCT@
    recordType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { recordTags = Prelude.Nothing,
      status = Prelude.Nothing,
      provisionedProductName = Prelude.Nothing,
      recordErrors = Prelude.Nothing,
      provisionedProductId = Prelude.Nothing,
      provisionedProductType = Prelude.Nothing,
      recordId = Prelude.Nothing,
      updatedTime = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      provisioningArtifactId = Prelude.Nothing,
      launchRoleArn = Prelude.Nothing,
      productId = Prelude.Nothing,
      pathId = Prelude.Nothing,
      recordType = Prelude.Nothing
    }

-- | One or more tags.
recordDetail_recordTags :: Lens.Lens' RecordDetail (Prelude.Maybe [RecordTag])
recordDetail_recordTags = Lens.lens (\RecordDetail' {recordTags} -> recordTags) (\s@RecordDetail' {} a -> s {recordTags = a} :: RecordDetail) Prelude.. Lens.mapping Prelude._Coerce

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

-- | The user-friendly name of the provisioned product.
recordDetail_provisionedProductName :: Lens.Lens' RecordDetail (Prelude.Maybe Prelude.Text)
recordDetail_provisionedProductName = Lens.lens (\RecordDetail' {provisionedProductName} -> provisionedProductName) (\s@RecordDetail' {} a -> s {provisionedProductName = a} :: RecordDetail)

-- | The errors that occurred.
recordDetail_recordErrors :: Lens.Lens' RecordDetail (Prelude.Maybe [RecordError])
recordDetail_recordErrors = Lens.lens (\RecordDetail' {recordErrors} -> recordErrors) (\s@RecordDetail' {} a -> s {recordErrors = a} :: RecordDetail) Prelude.. Lens.mapping Prelude._Coerce

-- | The identifier of the provisioned product.
recordDetail_provisionedProductId :: Lens.Lens' RecordDetail (Prelude.Maybe Prelude.Text)
recordDetail_provisionedProductId = Lens.lens (\RecordDetail' {provisionedProductId} -> provisionedProductId) (\s@RecordDetail' {} a -> s {provisionedProductId = a} :: RecordDetail)

-- | The type of provisioned product. The supported values are @CFN_STACK@
-- and @CFN_STACKSET@.
recordDetail_provisionedProductType :: Lens.Lens' RecordDetail (Prelude.Maybe Prelude.Text)
recordDetail_provisionedProductType = Lens.lens (\RecordDetail' {provisionedProductType} -> provisionedProductType) (\s@RecordDetail' {} a -> s {provisionedProductType = a} :: RecordDetail)

-- | The identifier of the record.
recordDetail_recordId :: Lens.Lens' RecordDetail (Prelude.Maybe Prelude.Text)
recordDetail_recordId = Lens.lens (\RecordDetail' {recordId} -> recordId) (\s@RecordDetail' {} a -> s {recordId = a} :: RecordDetail)

-- | The time when the record was last updated.
recordDetail_updatedTime :: Lens.Lens' RecordDetail (Prelude.Maybe Prelude.UTCTime)
recordDetail_updatedTime = Lens.lens (\RecordDetail' {updatedTime} -> updatedTime) (\s@RecordDetail' {} a -> s {updatedTime = a} :: RecordDetail) Prelude.. Lens.mapping Prelude._Time

-- | The UTC time stamp of the creation time.
recordDetail_createdTime :: Lens.Lens' RecordDetail (Prelude.Maybe Prelude.UTCTime)
recordDetail_createdTime = Lens.lens (\RecordDetail' {createdTime} -> createdTime) (\s@RecordDetail' {} a -> s {createdTime = a} :: RecordDetail) Prelude.. Lens.mapping Prelude._Time

-- | The identifier of the provisioning artifact.
recordDetail_provisioningArtifactId :: Lens.Lens' RecordDetail (Prelude.Maybe Prelude.Text)
recordDetail_provisioningArtifactId = Lens.lens (\RecordDetail' {provisioningArtifactId} -> provisioningArtifactId) (\s@RecordDetail' {} a -> s {provisioningArtifactId = a} :: RecordDetail)

-- | The ARN of the launch role associated with the provisioned product.
recordDetail_launchRoleArn :: Lens.Lens' RecordDetail (Prelude.Maybe Prelude.Text)
recordDetail_launchRoleArn = Lens.lens (\RecordDetail' {launchRoleArn} -> launchRoleArn) (\s@RecordDetail' {} a -> s {launchRoleArn = a} :: RecordDetail)

-- | The product identifier.
recordDetail_productId :: Lens.Lens' RecordDetail (Prelude.Maybe Prelude.Text)
recordDetail_productId = Lens.lens (\RecordDetail' {productId} -> productId) (\s@RecordDetail' {} a -> s {productId = a} :: RecordDetail)

-- | The path identifier.
recordDetail_pathId :: Lens.Lens' RecordDetail (Prelude.Maybe Prelude.Text)
recordDetail_pathId = Lens.lens (\RecordDetail' {pathId} -> pathId) (\s@RecordDetail' {} a -> s {pathId = a} :: RecordDetail)

-- | The record type.
--
-- -   @PROVISION_PRODUCT@
--
-- -   @UPDATE_PROVISIONED_PRODUCT@
--
-- -   @TERMINATE_PROVISIONED_PRODUCT@
recordDetail_recordType :: Lens.Lens' RecordDetail (Prelude.Maybe Prelude.Text)
recordDetail_recordType = Lens.lens (\RecordDetail' {recordType} -> recordType) (\s@RecordDetail' {} a -> s {recordType = a} :: RecordDetail)

instance Prelude.FromJSON RecordDetail where
  parseJSON =
    Prelude.withObject
      "RecordDetail"
      ( \x ->
          RecordDetail'
            Prelude.<$> ( x Prelude..:? "RecordTags"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "ProvisionedProductName")
            Prelude.<*> ( x Prelude..:? "RecordErrors"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "ProvisionedProductId")
            Prelude.<*> (x Prelude..:? "ProvisionedProductType")
            Prelude.<*> (x Prelude..:? "RecordId")
            Prelude.<*> (x Prelude..:? "UpdatedTime")
            Prelude.<*> (x Prelude..:? "CreatedTime")
            Prelude.<*> (x Prelude..:? "ProvisioningArtifactId")
            Prelude.<*> (x Prelude..:? "LaunchRoleArn")
            Prelude.<*> (x Prelude..:? "ProductId")
            Prelude.<*> (x Prelude..:? "PathId")
            Prelude.<*> (x Prelude..:? "RecordType")
      )

instance Prelude.Hashable RecordDetail

instance Prelude.NFData RecordDetail
