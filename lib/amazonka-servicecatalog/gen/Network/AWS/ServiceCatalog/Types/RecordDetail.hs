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
    rdLaunchRoleARN,
    rdStatus,
    rdRecordTags,
    rdProvisionedProductName,
    rdProvisioningArtifactId,
    rdCreatedTime,
    rdRecordType,
    rdRecordId,
    rdProvisionedProductType,
    rdUpdatedTime,
    rdPathId,
    rdProvisionedProductId,
    rdRecordErrors,
    rdProductId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.ServiceCatalog.Types.RecordError
import Network.AWS.ServiceCatalog.Types.RecordStatus
import Network.AWS.ServiceCatalog.Types.RecordTag

-- | Information about a request operation.
--
-- /See:/ 'mkRecordDetail' smart constructor.
data RecordDetail = RecordDetail'
  { launchRoleARN ::
      Lude.Maybe Lude.Text,
    status :: Lude.Maybe RecordStatus,
    recordTags :: Lude.Maybe [RecordTag],
    provisionedProductName :: Lude.Maybe Lude.Text,
    provisioningArtifactId :: Lude.Maybe Lude.Text,
    createdTime :: Lude.Maybe Lude.Timestamp,
    recordType :: Lude.Maybe Lude.Text,
    recordId :: Lude.Maybe Lude.Text,
    provisionedProductType :: Lude.Maybe Lude.Text,
    updatedTime :: Lude.Maybe Lude.Timestamp,
    pathId :: Lude.Maybe Lude.Text,
    provisionedProductId :: Lude.Maybe Lude.Text,
    recordErrors :: Lude.Maybe [RecordError],
    productId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RecordDetail' with the minimum fields required to make a request.
--
-- * 'createdTime' - The UTC time stamp of the creation time.
-- * 'launchRoleARN' - The ARN of the launch role associated with the provisioned product.
-- * 'pathId' - The path identifier.
-- * 'productId' - The product identifier.
-- * 'provisionedProductId' - The identifier of the provisioned product.
-- * 'provisionedProductName' - The user-friendly name of the provisioned product.
-- * 'provisionedProductType' - The type of provisioned product. The supported values are @CFN_STACK@ and @CFN_STACKSET@ .
-- * 'provisioningArtifactId' - The identifier of the provisioning artifact.
-- * 'recordErrors' - The errors that occurred.
-- * 'recordId' - The identifier of the record.
-- * 'recordTags' - One or more tags.
-- * 'recordType' - The record type.
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
-- * 'status' - The status of the provisioned product.
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
-- * 'updatedTime' - The time when the record was last updated.
mkRecordDetail ::
  RecordDetail
mkRecordDetail =
  RecordDetail'
    { launchRoleARN = Lude.Nothing,
      status = Lude.Nothing,
      recordTags = Lude.Nothing,
      provisionedProductName = Lude.Nothing,
      provisioningArtifactId = Lude.Nothing,
      createdTime = Lude.Nothing,
      recordType = Lude.Nothing,
      recordId = Lude.Nothing,
      provisionedProductType = Lude.Nothing,
      updatedTime = Lude.Nothing,
      pathId = Lude.Nothing,
      provisionedProductId = Lude.Nothing,
      recordErrors = Lude.Nothing,
      productId = Lude.Nothing
    }

-- | The ARN of the launch role associated with the provisioned product.
--
-- /Note:/ Consider using 'launchRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdLaunchRoleARN :: Lens.Lens' RecordDetail (Lude.Maybe Lude.Text)
rdLaunchRoleARN = Lens.lens (launchRoleARN :: RecordDetail -> Lude.Maybe Lude.Text) (\s a -> s {launchRoleARN = a} :: RecordDetail)
{-# DEPRECATED rdLaunchRoleARN "Use generic-lens or generic-optics with 'launchRoleARN' instead." #-}

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
rdStatus :: Lens.Lens' RecordDetail (Lude.Maybe RecordStatus)
rdStatus = Lens.lens (status :: RecordDetail -> Lude.Maybe RecordStatus) (\s a -> s {status = a} :: RecordDetail)
{-# DEPRECATED rdStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | One or more tags.
--
-- /Note:/ Consider using 'recordTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdRecordTags :: Lens.Lens' RecordDetail (Lude.Maybe [RecordTag])
rdRecordTags = Lens.lens (recordTags :: RecordDetail -> Lude.Maybe [RecordTag]) (\s a -> s {recordTags = a} :: RecordDetail)
{-# DEPRECATED rdRecordTags "Use generic-lens or generic-optics with 'recordTags' instead." #-}

-- | The user-friendly name of the provisioned product.
--
-- /Note:/ Consider using 'provisionedProductName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdProvisionedProductName :: Lens.Lens' RecordDetail (Lude.Maybe Lude.Text)
rdProvisionedProductName = Lens.lens (provisionedProductName :: RecordDetail -> Lude.Maybe Lude.Text) (\s a -> s {provisionedProductName = a} :: RecordDetail)
{-# DEPRECATED rdProvisionedProductName "Use generic-lens or generic-optics with 'provisionedProductName' instead." #-}

-- | The identifier of the provisioning artifact.
--
-- /Note:/ Consider using 'provisioningArtifactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdProvisioningArtifactId :: Lens.Lens' RecordDetail (Lude.Maybe Lude.Text)
rdProvisioningArtifactId = Lens.lens (provisioningArtifactId :: RecordDetail -> Lude.Maybe Lude.Text) (\s a -> s {provisioningArtifactId = a} :: RecordDetail)
{-# DEPRECATED rdProvisioningArtifactId "Use generic-lens or generic-optics with 'provisioningArtifactId' instead." #-}

-- | The UTC time stamp of the creation time.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdCreatedTime :: Lens.Lens' RecordDetail (Lude.Maybe Lude.Timestamp)
rdCreatedTime = Lens.lens (createdTime :: RecordDetail -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdTime = a} :: RecordDetail)
{-# DEPRECATED rdCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

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
rdRecordType :: Lens.Lens' RecordDetail (Lude.Maybe Lude.Text)
rdRecordType = Lens.lens (recordType :: RecordDetail -> Lude.Maybe Lude.Text) (\s a -> s {recordType = a} :: RecordDetail)
{-# DEPRECATED rdRecordType "Use generic-lens or generic-optics with 'recordType' instead." #-}

-- | The identifier of the record.
--
-- /Note:/ Consider using 'recordId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdRecordId :: Lens.Lens' RecordDetail (Lude.Maybe Lude.Text)
rdRecordId = Lens.lens (recordId :: RecordDetail -> Lude.Maybe Lude.Text) (\s a -> s {recordId = a} :: RecordDetail)
{-# DEPRECATED rdRecordId "Use generic-lens or generic-optics with 'recordId' instead." #-}

-- | The type of provisioned product. The supported values are @CFN_STACK@ and @CFN_STACKSET@ .
--
-- /Note:/ Consider using 'provisionedProductType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdProvisionedProductType :: Lens.Lens' RecordDetail (Lude.Maybe Lude.Text)
rdProvisionedProductType = Lens.lens (provisionedProductType :: RecordDetail -> Lude.Maybe Lude.Text) (\s a -> s {provisionedProductType = a} :: RecordDetail)
{-# DEPRECATED rdProvisionedProductType "Use generic-lens or generic-optics with 'provisionedProductType' instead." #-}

-- | The time when the record was last updated.
--
-- /Note:/ Consider using 'updatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdUpdatedTime :: Lens.Lens' RecordDetail (Lude.Maybe Lude.Timestamp)
rdUpdatedTime = Lens.lens (updatedTime :: RecordDetail -> Lude.Maybe Lude.Timestamp) (\s a -> s {updatedTime = a} :: RecordDetail)
{-# DEPRECATED rdUpdatedTime "Use generic-lens or generic-optics with 'updatedTime' instead." #-}

-- | The path identifier.
--
-- /Note:/ Consider using 'pathId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdPathId :: Lens.Lens' RecordDetail (Lude.Maybe Lude.Text)
rdPathId = Lens.lens (pathId :: RecordDetail -> Lude.Maybe Lude.Text) (\s a -> s {pathId = a} :: RecordDetail)
{-# DEPRECATED rdPathId "Use generic-lens or generic-optics with 'pathId' instead." #-}

-- | The identifier of the provisioned product.
--
-- /Note:/ Consider using 'provisionedProductId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdProvisionedProductId :: Lens.Lens' RecordDetail (Lude.Maybe Lude.Text)
rdProvisionedProductId = Lens.lens (provisionedProductId :: RecordDetail -> Lude.Maybe Lude.Text) (\s a -> s {provisionedProductId = a} :: RecordDetail)
{-# DEPRECATED rdProvisionedProductId "Use generic-lens or generic-optics with 'provisionedProductId' instead." #-}

-- | The errors that occurred.
--
-- /Note:/ Consider using 'recordErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdRecordErrors :: Lens.Lens' RecordDetail (Lude.Maybe [RecordError])
rdRecordErrors = Lens.lens (recordErrors :: RecordDetail -> Lude.Maybe [RecordError]) (\s a -> s {recordErrors = a} :: RecordDetail)
{-# DEPRECATED rdRecordErrors "Use generic-lens or generic-optics with 'recordErrors' instead." #-}

-- | The product identifier.
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdProductId :: Lens.Lens' RecordDetail (Lude.Maybe Lude.Text)
rdProductId = Lens.lens (productId :: RecordDetail -> Lude.Maybe Lude.Text) (\s a -> s {productId = a} :: RecordDetail)
{-# DEPRECATED rdProductId "Use generic-lens or generic-optics with 'productId' instead." #-}

instance Lude.FromJSON RecordDetail where
  parseJSON =
    Lude.withObject
      "RecordDetail"
      ( \x ->
          RecordDetail'
            Lude.<$> (x Lude..:? "LaunchRoleArn")
            Lude.<*> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "RecordTags" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ProvisionedProductName")
            Lude.<*> (x Lude..:? "ProvisioningArtifactId")
            Lude.<*> (x Lude..:? "CreatedTime")
            Lude.<*> (x Lude..:? "RecordType")
            Lude.<*> (x Lude..:? "RecordId")
            Lude.<*> (x Lude..:? "ProvisionedProductType")
            Lude.<*> (x Lude..:? "UpdatedTime")
            Lude.<*> (x Lude..:? "PathId")
            Lude.<*> (x Lude..:? "ProvisionedProductId")
            Lude.<*> (x Lude..:? "RecordErrors" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ProductId")
      )
