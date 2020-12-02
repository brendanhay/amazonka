{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisionedProductAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisionedProductAttribute where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.ServiceCatalog.Types.ProvisionedProductStatus
import Network.AWS.ServiceCatalog.Types.Tag

-- | Information about a provisioned product.
--
--
--
-- /See:/ 'provisionedProductAttribute' smart constructor.
data ProvisionedProductAttribute = ProvisionedProductAttribute'
  { _ppaIdempotencyToken ::
      !(Maybe Text),
    _ppaStatus ::
      !(Maybe ProvisionedProductStatus),
    _ppaProductName :: !(Maybe Text),
    _ppaLastSuccessfulProvisioningRecordId ::
      !(Maybe Text),
    _ppaProvisioningArtifactId ::
      !(Maybe Text),
    _ppaARN :: !(Maybe Text),
    _ppaCreatedTime :: !(Maybe POSIX),
    _ppaProvisioningArtifactName ::
      !(Maybe Text),
    _ppaUserARN :: !(Maybe Text),
    _ppaStatusMessage :: !(Maybe Text),
    _ppaName :: !(Maybe Text),
    _ppaLastRecordId :: !(Maybe Text),
    _ppaUserARNSession :: !(Maybe Text),
    _ppaId :: !(Maybe Text),
    _ppaType :: !(Maybe Text),
    _ppaPhysicalId :: !(Maybe Text),
    _ppaLastProvisioningRecordId ::
      !(Maybe Text),
    _ppaProductId :: !(Maybe Text),
    _ppaTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProvisionedProductAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppaIdempotencyToken' - A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
--
-- * 'ppaStatus' - The current status of the provisioned product.     * @AVAILABLE@ - Stable state, ready to perform any operation. The most recent operation succeeded and completed.     * @UNDER_CHANGE@ - Transitive state. Operations performed might not have valid results. Wait for an @AVAILABLE@ status before performing operations.     * @TAINTED@ - Stable state, ready to perform any operation. The stack has completed the requested operation but is not exactly what was requested. For example, a request to update to a new version failed and the stack rolled back to the current version.     * @ERROR@ - An unexpected error occurred. The provisioned product exists but the stack is not running. For example, CloudFormation received a parameter value that was not valid and could not launch the stack.     * @PLAN_IN_PROGRESS@ - Transitive state. The plan operations were performed to provision a new product, but resources have not yet been created. After reviewing the list of resources to be created, execute the plan. Wait for an @AVAILABLE@ status before performing operations.
--
-- * 'ppaProductName' - The name of the product.
--
-- * 'ppaLastSuccessfulProvisioningRecordId' - The record identifier of the last successful request performed on this provisioned product of the following types:     * ProvisionedProduct      * UpdateProvisionedProduct      * ExecuteProvisionedProductPlan      * TerminateProvisionedProduct
--
-- * 'ppaProvisioningArtifactId' - The identifier of the provisioning artifact.
--
-- * 'ppaARN' - The ARN of the provisioned product.
--
-- * 'ppaCreatedTime' - The UTC time stamp of the creation time.
--
-- * 'ppaProvisioningArtifactName' - The name of the provisioning artifact.
--
-- * 'ppaUserARN' - The Amazon Resource Name (ARN) of the IAM user.
--
-- * 'ppaStatusMessage' - The current status message of the provisioned product.
--
-- * 'ppaName' - The user-friendly name of the provisioned product.
--
-- * 'ppaLastRecordId' - The record identifier of the last request performed on this provisioned product.
--
-- * 'ppaUserARNSession' - The ARN of the IAM user in the session. This ARN might contain a session ID.
--
-- * 'ppaId' - The identifier of the provisioned product.
--
-- * 'ppaType' - The type of provisioned product. The supported values are @CFN_STACK@ and @CFN_STACKSET@ .
--
-- * 'ppaPhysicalId' - The assigned identifier for the resource, such as an EC2 instance ID or an S3 bucket name.
--
-- * 'ppaLastProvisioningRecordId' - The record identifier of the last request performed on this provisioned product of the following types:     * ProvisionedProduct      * UpdateProvisionedProduct      * ExecuteProvisionedProductPlan      * TerminateProvisionedProduct
--
-- * 'ppaProductId' - The product identifier.
--
-- * 'ppaTags' - One or more tags.
provisionedProductAttribute ::
  ProvisionedProductAttribute
provisionedProductAttribute =
  ProvisionedProductAttribute'
    { _ppaIdempotencyToken = Nothing,
      _ppaStatus = Nothing,
      _ppaProductName = Nothing,
      _ppaLastSuccessfulProvisioningRecordId = Nothing,
      _ppaProvisioningArtifactId = Nothing,
      _ppaARN = Nothing,
      _ppaCreatedTime = Nothing,
      _ppaProvisioningArtifactName = Nothing,
      _ppaUserARN = Nothing,
      _ppaStatusMessage = Nothing,
      _ppaName = Nothing,
      _ppaLastRecordId = Nothing,
      _ppaUserARNSession = Nothing,
      _ppaId = Nothing,
      _ppaType = Nothing,
      _ppaPhysicalId = Nothing,
      _ppaLastProvisioningRecordId = Nothing,
      _ppaProductId = Nothing,
      _ppaTags = Nothing
    }

-- | A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
ppaIdempotencyToken :: Lens' ProvisionedProductAttribute (Maybe Text)
ppaIdempotencyToken = lens _ppaIdempotencyToken (\s a -> s {_ppaIdempotencyToken = a})

-- | The current status of the provisioned product.     * @AVAILABLE@ - Stable state, ready to perform any operation. The most recent operation succeeded and completed.     * @UNDER_CHANGE@ - Transitive state. Operations performed might not have valid results. Wait for an @AVAILABLE@ status before performing operations.     * @TAINTED@ - Stable state, ready to perform any operation. The stack has completed the requested operation but is not exactly what was requested. For example, a request to update to a new version failed and the stack rolled back to the current version.     * @ERROR@ - An unexpected error occurred. The provisioned product exists but the stack is not running. For example, CloudFormation received a parameter value that was not valid and could not launch the stack.     * @PLAN_IN_PROGRESS@ - Transitive state. The plan operations were performed to provision a new product, but resources have not yet been created. After reviewing the list of resources to be created, execute the plan. Wait for an @AVAILABLE@ status before performing operations.
ppaStatus :: Lens' ProvisionedProductAttribute (Maybe ProvisionedProductStatus)
ppaStatus = lens _ppaStatus (\s a -> s {_ppaStatus = a})

-- | The name of the product.
ppaProductName :: Lens' ProvisionedProductAttribute (Maybe Text)
ppaProductName = lens _ppaProductName (\s a -> s {_ppaProductName = a})

-- | The record identifier of the last successful request performed on this provisioned product of the following types:     * ProvisionedProduct      * UpdateProvisionedProduct      * ExecuteProvisionedProductPlan      * TerminateProvisionedProduct
ppaLastSuccessfulProvisioningRecordId :: Lens' ProvisionedProductAttribute (Maybe Text)
ppaLastSuccessfulProvisioningRecordId = lens _ppaLastSuccessfulProvisioningRecordId (\s a -> s {_ppaLastSuccessfulProvisioningRecordId = a})

-- | The identifier of the provisioning artifact.
ppaProvisioningArtifactId :: Lens' ProvisionedProductAttribute (Maybe Text)
ppaProvisioningArtifactId = lens _ppaProvisioningArtifactId (\s a -> s {_ppaProvisioningArtifactId = a})

-- | The ARN of the provisioned product.
ppaARN :: Lens' ProvisionedProductAttribute (Maybe Text)
ppaARN = lens _ppaARN (\s a -> s {_ppaARN = a})

-- | The UTC time stamp of the creation time.
ppaCreatedTime :: Lens' ProvisionedProductAttribute (Maybe UTCTime)
ppaCreatedTime = lens _ppaCreatedTime (\s a -> s {_ppaCreatedTime = a}) . mapping _Time

-- | The name of the provisioning artifact.
ppaProvisioningArtifactName :: Lens' ProvisionedProductAttribute (Maybe Text)
ppaProvisioningArtifactName = lens _ppaProvisioningArtifactName (\s a -> s {_ppaProvisioningArtifactName = a})

-- | The Amazon Resource Name (ARN) of the IAM user.
ppaUserARN :: Lens' ProvisionedProductAttribute (Maybe Text)
ppaUserARN = lens _ppaUserARN (\s a -> s {_ppaUserARN = a})

-- | The current status message of the provisioned product.
ppaStatusMessage :: Lens' ProvisionedProductAttribute (Maybe Text)
ppaStatusMessage = lens _ppaStatusMessage (\s a -> s {_ppaStatusMessage = a})

-- | The user-friendly name of the provisioned product.
ppaName :: Lens' ProvisionedProductAttribute (Maybe Text)
ppaName = lens _ppaName (\s a -> s {_ppaName = a})

-- | The record identifier of the last request performed on this provisioned product.
ppaLastRecordId :: Lens' ProvisionedProductAttribute (Maybe Text)
ppaLastRecordId = lens _ppaLastRecordId (\s a -> s {_ppaLastRecordId = a})

-- | The ARN of the IAM user in the session. This ARN might contain a session ID.
ppaUserARNSession :: Lens' ProvisionedProductAttribute (Maybe Text)
ppaUserARNSession = lens _ppaUserARNSession (\s a -> s {_ppaUserARNSession = a})

-- | The identifier of the provisioned product.
ppaId :: Lens' ProvisionedProductAttribute (Maybe Text)
ppaId = lens _ppaId (\s a -> s {_ppaId = a})

-- | The type of provisioned product. The supported values are @CFN_STACK@ and @CFN_STACKSET@ .
ppaType :: Lens' ProvisionedProductAttribute (Maybe Text)
ppaType = lens _ppaType (\s a -> s {_ppaType = a})

-- | The assigned identifier for the resource, such as an EC2 instance ID or an S3 bucket name.
ppaPhysicalId :: Lens' ProvisionedProductAttribute (Maybe Text)
ppaPhysicalId = lens _ppaPhysicalId (\s a -> s {_ppaPhysicalId = a})

-- | The record identifier of the last request performed on this provisioned product of the following types:     * ProvisionedProduct      * UpdateProvisionedProduct      * ExecuteProvisionedProductPlan      * TerminateProvisionedProduct
ppaLastProvisioningRecordId :: Lens' ProvisionedProductAttribute (Maybe Text)
ppaLastProvisioningRecordId = lens _ppaLastProvisioningRecordId (\s a -> s {_ppaLastProvisioningRecordId = a})

-- | The product identifier.
ppaProductId :: Lens' ProvisionedProductAttribute (Maybe Text)
ppaProductId = lens _ppaProductId (\s a -> s {_ppaProductId = a})

-- | One or more tags.
ppaTags :: Lens' ProvisionedProductAttribute [Tag]
ppaTags = lens _ppaTags (\s a -> s {_ppaTags = a}) . _Default . _Coerce

instance FromJSON ProvisionedProductAttribute where
  parseJSON =
    withObject
      "ProvisionedProductAttribute"
      ( \x ->
          ProvisionedProductAttribute'
            <$> (x .:? "IdempotencyToken")
            <*> (x .:? "Status")
            <*> (x .:? "ProductName")
            <*> (x .:? "LastSuccessfulProvisioningRecordId")
            <*> (x .:? "ProvisioningArtifactId")
            <*> (x .:? "Arn")
            <*> (x .:? "CreatedTime")
            <*> (x .:? "ProvisioningArtifactName")
            <*> (x .:? "UserArn")
            <*> (x .:? "StatusMessage")
            <*> (x .:? "Name")
            <*> (x .:? "LastRecordId")
            <*> (x .:? "UserArnSession")
            <*> (x .:? "Id")
            <*> (x .:? "Type")
            <*> (x .:? "PhysicalId")
            <*> (x .:? "LastProvisioningRecordId")
            <*> (x .:? "ProductId")
            <*> (x .:? "Tags" .!= mempty)
      )

instance Hashable ProvisionedProductAttribute

instance NFData ProvisionedProductAttribute
