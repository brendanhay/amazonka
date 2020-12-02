{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisionedProductDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisionedProductDetail where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.ServiceCatalog.Types.ProvisionedProductStatus

-- | Information about a provisioned product.
--
--
--
-- /See:/ 'provisionedProductDetail' smart constructor.
data ProvisionedProductDetail = ProvisionedProductDetail'
  { _ppdLaunchRoleARN ::
      !(Maybe Text),
    _ppdIdempotencyToken :: !(Maybe Text),
    _ppdStatus ::
      !(Maybe ProvisionedProductStatus),
    _ppdLastSuccessfulProvisioningRecordId ::
      !(Maybe Text),
    _ppdProvisioningArtifactId ::
      !(Maybe Text),
    _ppdARN :: !(Maybe Text),
    _ppdCreatedTime :: !(Maybe POSIX),
    _ppdStatusMessage :: !(Maybe Text),
    _ppdName :: !(Maybe Text),
    _ppdLastRecordId :: !(Maybe Text),
    _ppdId :: !(Maybe Text),
    _ppdType :: !(Maybe Text),
    _ppdLastProvisioningRecordId ::
      !(Maybe Text),
    _ppdProductId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProvisionedProductDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppdLaunchRoleARN' - The ARN of the launch role associated with the provisioned product.
--
-- * 'ppdIdempotencyToken' - A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
--
-- * 'ppdStatus' - The current status of the provisioned product.     * @AVAILABLE@ - Stable state, ready to perform any operation. The most recent operation succeeded and completed.     * @UNDER_CHANGE@ - Transitive state. Operations performed might not have valid results. Wait for an @AVAILABLE@ status before performing operations.     * @TAINTED@ - Stable state, ready to perform any operation. The stack has completed the requested operation but is not exactly what was requested. For example, a request to update to a new version failed and the stack rolled back to the current version.     * @ERROR@ - An unexpected error occurred. The provisioned product exists but the stack is not running. For example, CloudFormation received a parameter value that was not valid and could not launch the stack.     * @PLAN_IN_PROGRESS@ - Transitive state. The plan operations were performed to provision a new product, but resources have not yet been created. After reviewing the list of resources to be created, execute the plan. Wait for an @AVAILABLE@ status before performing operations.
--
-- * 'ppdLastSuccessfulProvisioningRecordId' - The record identifier of the last successful request performed on this provisioned product of the following types:     * ProvisionedProduct      * UpdateProvisionedProduct      * ExecuteProvisionedProductPlan      * TerminateProvisionedProduct
--
-- * 'ppdProvisioningArtifactId' - The identifier of the provisioning artifact. For example, @pa-4abcdjnxjj6ne@ .
--
-- * 'ppdARN' - The ARN of the provisioned product.
--
-- * 'ppdCreatedTime' - The UTC time stamp of the creation time.
--
-- * 'ppdStatusMessage' - The current status message of the provisioned product.
--
-- * 'ppdName' - The user-friendly name of the provisioned product.
--
-- * 'ppdLastRecordId' - The record identifier of the last request performed on this provisioned product.
--
-- * 'ppdId' - The identifier of the provisioned product.
--
-- * 'ppdType' - The type of provisioned product. The supported values are @CFN_STACK@ and @CFN_STACKSET@ .
--
-- * 'ppdLastProvisioningRecordId' - The record identifier of the last request performed on this provisioned product of the following types:     * ProvisionedProduct      * UpdateProvisionedProduct      * ExecuteProvisionedProductPlan      * TerminateProvisionedProduct
--
-- * 'ppdProductId' - The product identifier. For example, @prod-abcdzk7xy33qa@ .
provisionedProductDetail ::
  ProvisionedProductDetail
provisionedProductDetail =
  ProvisionedProductDetail'
    { _ppdLaunchRoleARN = Nothing,
      _ppdIdempotencyToken = Nothing,
      _ppdStatus = Nothing,
      _ppdLastSuccessfulProvisioningRecordId = Nothing,
      _ppdProvisioningArtifactId = Nothing,
      _ppdARN = Nothing,
      _ppdCreatedTime = Nothing,
      _ppdStatusMessage = Nothing,
      _ppdName = Nothing,
      _ppdLastRecordId = Nothing,
      _ppdId = Nothing,
      _ppdType = Nothing,
      _ppdLastProvisioningRecordId = Nothing,
      _ppdProductId = Nothing
    }

-- | The ARN of the launch role associated with the provisioned product.
ppdLaunchRoleARN :: Lens' ProvisionedProductDetail (Maybe Text)
ppdLaunchRoleARN = lens _ppdLaunchRoleARN (\s a -> s {_ppdLaunchRoleARN = a})

-- | A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
ppdIdempotencyToken :: Lens' ProvisionedProductDetail (Maybe Text)
ppdIdempotencyToken = lens _ppdIdempotencyToken (\s a -> s {_ppdIdempotencyToken = a})

-- | The current status of the provisioned product.     * @AVAILABLE@ - Stable state, ready to perform any operation. The most recent operation succeeded and completed.     * @UNDER_CHANGE@ - Transitive state. Operations performed might not have valid results. Wait for an @AVAILABLE@ status before performing operations.     * @TAINTED@ - Stable state, ready to perform any operation. The stack has completed the requested operation but is not exactly what was requested. For example, a request to update to a new version failed and the stack rolled back to the current version.     * @ERROR@ - An unexpected error occurred. The provisioned product exists but the stack is not running. For example, CloudFormation received a parameter value that was not valid and could not launch the stack.     * @PLAN_IN_PROGRESS@ - Transitive state. The plan operations were performed to provision a new product, but resources have not yet been created. After reviewing the list of resources to be created, execute the plan. Wait for an @AVAILABLE@ status before performing operations.
ppdStatus :: Lens' ProvisionedProductDetail (Maybe ProvisionedProductStatus)
ppdStatus = lens _ppdStatus (\s a -> s {_ppdStatus = a})

-- | The record identifier of the last successful request performed on this provisioned product of the following types:     * ProvisionedProduct      * UpdateProvisionedProduct      * ExecuteProvisionedProductPlan      * TerminateProvisionedProduct
ppdLastSuccessfulProvisioningRecordId :: Lens' ProvisionedProductDetail (Maybe Text)
ppdLastSuccessfulProvisioningRecordId = lens _ppdLastSuccessfulProvisioningRecordId (\s a -> s {_ppdLastSuccessfulProvisioningRecordId = a})

-- | The identifier of the provisioning artifact. For example, @pa-4abcdjnxjj6ne@ .
ppdProvisioningArtifactId :: Lens' ProvisionedProductDetail (Maybe Text)
ppdProvisioningArtifactId = lens _ppdProvisioningArtifactId (\s a -> s {_ppdProvisioningArtifactId = a})

-- | The ARN of the provisioned product.
ppdARN :: Lens' ProvisionedProductDetail (Maybe Text)
ppdARN = lens _ppdARN (\s a -> s {_ppdARN = a})

-- | The UTC time stamp of the creation time.
ppdCreatedTime :: Lens' ProvisionedProductDetail (Maybe UTCTime)
ppdCreatedTime = lens _ppdCreatedTime (\s a -> s {_ppdCreatedTime = a}) . mapping _Time

-- | The current status message of the provisioned product.
ppdStatusMessage :: Lens' ProvisionedProductDetail (Maybe Text)
ppdStatusMessage = lens _ppdStatusMessage (\s a -> s {_ppdStatusMessage = a})

-- | The user-friendly name of the provisioned product.
ppdName :: Lens' ProvisionedProductDetail (Maybe Text)
ppdName = lens _ppdName (\s a -> s {_ppdName = a})

-- | The record identifier of the last request performed on this provisioned product.
ppdLastRecordId :: Lens' ProvisionedProductDetail (Maybe Text)
ppdLastRecordId = lens _ppdLastRecordId (\s a -> s {_ppdLastRecordId = a})

-- | The identifier of the provisioned product.
ppdId :: Lens' ProvisionedProductDetail (Maybe Text)
ppdId = lens _ppdId (\s a -> s {_ppdId = a})

-- | The type of provisioned product. The supported values are @CFN_STACK@ and @CFN_STACKSET@ .
ppdType :: Lens' ProvisionedProductDetail (Maybe Text)
ppdType = lens _ppdType (\s a -> s {_ppdType = a})

-- | The record identifier of the last request performed on this provisioned product of the following types:     * ProvisionedProduct      * UpdateProvisionedProduct      * ExecuteProvisionedProductPlan      * TerminateProvisionedProduct
ppdLastProvisioningRecordId :: Lens' ProvisionedProductDetail (Maybe Text)
ppdLastProvisioningRecordId = lens _ppdLastProvisioningRecordId (\s a -> s {_ppdLastProvisioningRecordId = a})

-- | The product identifier. For example, @prod-abcdzk7xy33qa@ .
ppdProductId :: Lens' ProvisionedProductDetail (Maybe Text)
ppdProductId = lens _ppdProductId (\s a -> s {_ppdProductId = a})

instance FromJSON ProvisionedProductDetail where
  parseJSON =
    withObject
      "ProvisionedProductDetail"
      ( \x ->
          ProvisionedProductDetail'
            <$> (x .:? "LaunchRoleArn")
            <*> (x .:? "IdempotencyToken")
            <*> (x .:? "Status")
            <*> (x .:? "LastSuccessfulProvisioningRecordId")
            <*> (x .:? "ProvisioningArtifactId")
            <*> (x .:? "Arn")
            <*> (x .:? "CreatedTime")
            <*> (x .:? "StatusMessage")
            <*> (x .:? "Name")
            <*> (x .:? "LastRecordId")
            <*> (x .:? "Id")
            <*> (x .:? "Type")
            <*> (x .:? "LastProvisioningRecordId")
            <*> (x .:? "ProductId")
      )

instance Hashable ProvisionedProductDetail

instance NFData ProvisionedProductDetail
