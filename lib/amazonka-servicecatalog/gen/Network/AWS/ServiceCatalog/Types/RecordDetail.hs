{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.RecordDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.RecordDetail where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.ServiceCatalog.Types.RecordError
import Network.AWS.ServiceCatalog.Types.RecordStatus
import Network.AWS.ServiceCatalog.Types.RecordTag

-- | Information about a request operation.
--
--
--
-- /See:/ 'recordDetail' smart constructor.
data RecordDetail = RecordDetail'
  { _rdLaunchRoleARN ::
      !(Maybe Text),
    _rdStatus :: !(Maybe RecordStatus),
    _rdRecordTags :: !(Maybe [RecordTag]),
    _rdProvisionedProductName :: !(Maybe Text),
    _rdProvisioningArtifactId :: !(Maybe Text),
    _rdCreatedTime :: !(Maybe POSIX),
    _rdRecordType :: !(Maybe Text),
    _rdRecordId :: !(Maybe Text),
    _rdProvisionedProductType :: !(Maybe Text),
    _rdUpdatedTime :: !(Maybe POSIX),
    _rdPathId :: !(Maybe Text),
    _rdProvisionedProductId :: !(Maybe Text),
    _rdRecordErrors :: !(Maybe [RecordError]),
    _rdProductId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RecordDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdLaunchRoleARN' - The ARN of the launch role associated with the provisioned product.
--
-- * 'rdStatus' - The status of the provisioned product.     * @CREATED@ - The request was created but the operation has not started.     * @IN_PROGRESS@ - The requested operation is in progress.     * @IN_PROGRESS_IN_ERROR@ - The provisioned product is under change but the requested operation failed and some remediation is occurring. For example, a rollback.     * @SUCCEEDED@ - The requested operation has successfully completed.     * @FAILED@ - The requested operation has unsuccessfully completed. Investigate using the error messages returned.
--
-- * 'rdRecordTags' - One or more tags.
--
-- * 'rdProvisionedProductName' - The user-friendly name of the provisioned product.
--
-- * 'rdProvisioningArtifactId' - The identifier of the provisioning artifact.
--
-- * 'rdCreatedTime' - The UTC time stamp of the creation time.
--
-- * 'rdRecordType' - The record type.     * @PROVISION_PRODUCT@      * @UPDATE_PROVISIONED_PRODUCT@      * @TERMINATE_PROVISIONED_PRODUCT@
--
-- * 'rdRecordId' - The identifier of the record.
--
-- * 'rdProvisionedProductType' - The type of provisioned product. The supported values are @CFN_STACK@ and @CFN_STACKSET@ .
--
-- * 'rdUpdatedTime' - The time when the record was last updated.
--
-- * 'rdPathId' - The path identifier.
--
-- * 'rdProvisionedProductId' - The identifier of the provisioned product.
--
-- * 'rdRecordErrors' - The errors that occurred.
--
-- * 'rdProductId' - The product identifier.
recordDetail ::
  RecordDetail
recordDetail =
  RecordDetail'
    { _rdLaunchRoleARN = Nothing,
      _rdStatus = Nothing,
      _rdRecordTags = Nothing,
      _rdProvisionedProductName = Nothing,
      _rdProvisioningArtifactId = Nothing,
      _rdCreatedTime = Nothing,
      _rdRecordType = Nothing,
      _rdRecordId = Nothing,
      _rdProvisionedProductType = Nothing,
      _rdUpdatedTime = Nothing,
      _rdPathId = Nothing,
      _rdProvisionedProductId = Nothing,
      _rdRecordErrors = Nothing,
      _rdProductId = Nothing
    }

-- | The ARN of the launch role associated with the provisioned product.
rdLaunchRoleARN :: Lens' RecordDetail (Maybe Text)
rdLaunchRoleARN = lens _rdLaunchRoleARN (\s a -> s {_rdLaunchRoleARN = a})

-- | The status of the provisioned product.     * @CREATED@ - The request was created but the operation has not started.     * @IN_PROGRESS@ - The requested operation is in progress.     * @IN_PROGRESS_IN_ERROR@ - The provisioned product is under change but the requested operation failed and some remediation is occurring. For example, a rollback.     * @SUCCEEDED@ - The requested operation has successfully completed.     * @FAILED@ - The requested operation has unsuccessfully completed. Investigate using the error messages returned.
rdStatus :: Lens' RecordDetail (Maybe RecordStatus)
rdStatus = lens _rdStatus (\s a -> s {_rdStatus = a})

-- | One or more tags.
rdRecordTags :: Lens' RecordDetail [RecordTag]
rdRecordTags = lens _rdRecordTags (\s a -> s {_rdRecordTags = a}) . _Default . _Coerce

-- | The user-friendly name of the provisioned product.
rdProvisionedProductName :: Lens' RecordDetail (Maybe Text)
rdProvisionedProductName = lens _rdProvisionedProductName (\s a -> s {_rdProvisionedProductName = a})

-- | The identifier of the provisioning artifact.
rdProvisioningArtifactId :: Lens' RecordDetail (Maybe Text)
rdProvisioningArtifactId = lens _rdProvisioningArtifactId (\s a -> s {_rdProvisioningArtifactId = a})

-- | The UTC time stamp of the creation time.
rdCreatedTime :: Lens' RecordDetail (Maybe UTCTime)
rdCreatedTime = lens _rdCreatedTime (\s a -> s {_rdCreatedTime = a}) . mapping _Time

-- | The record type.     * @PROVISION_PRODUCT@      * @UPDATE_PROVISIONED_PRODUCT@      * @TERMINATE_PROVISIONED_PRODUCT@
rdRecordType :: Lens' RecordDetail (Maybe Text)
rdRecordType = lens _rdRecordType (\s a -> s {_rdRecordType = a})

-- | The identifier of the record.
rdRecordId :: Lens' RecordDetail (Maybe Text)
rdRecordId = lens _rdRecordId (\s a -> s {_rdRecordId = a})

-- | The type of provisioned product. The supported values are @CFN_STACK@ and @CFN_STACKSET@ .
rdProvisionedProductType :: Lens' RecordDetail (Maybe Text)
rdProvisionedProductType = lens _rdProvisionedProductType (\s a -> s {_rdProvisionedProductType = a})

-- | The time when the record was last updated.
rdUpdatedTime :: Lens' RecordDetail (Maybe UTCTime)
rdUpdatedTime = lens _rdUpdatedTime (\s a -> s {_rdUpdatedTime = a}) . mapping _Time

-- | The path identifier.
rdPathId :: Lens' RecordDetail (Maybe Text)
rdPathId = lens _rdPathId (\s a -> s {_rdPathId = a})

-- | The identifier of the provisioned product.
rdProvisionedProductId :: Lens' RecordDetail (Maybe Text)
rdProvisionedProductId = lens _rdProvisionedProductId (\s a -> s {_rdProvisionedProductId = a})

-- | The errors that occurred.
rdRecordErrors :: Lens' RecordDetail [RecordError]
rdRecordErrors = lens _rdRecordErrors (\s a -> s {_rdRecordErrors = a}) . _Default . _Coerce

-- | The product identifier.
rdProductId :: Lens' RecordDetail (Maybe Text)
rdProductId = lens _rdProductId (\s a -> s {_rdProductId = a})

instance FromJSON RecordDetail where
  parseJSON =
    withObject
      "RecordDetail"
      ( \x ->
          RecordDetail'
            <$> (x .:? "LaunchRoleArn")
            <*> (x .:? "Status")
            <*> (x .:? "RecordTags" .!= mempty)
            <*> (x .:? "ProvisionedProductName")
            <*> (x .:? "ProvisioningArtifactId")
            <*> (x .:? "CreatedTime")
            <*> (x .:? "RecordType")
            <*> (x .:? "RecordId")
            <*> (x .:? "ProvisionedProductType")
            <*> (x .:? "UpdatedTime")
            <*> (x .:? "PathId")
            <*> (x .:? "ProvisionedProductId")
            <*> (x .:? "RecordErrors" .!= mempty)
            <*> (x .:? "ProductId")
      )

instance Hashable RecordDetail

instance NFData RecordDetail
