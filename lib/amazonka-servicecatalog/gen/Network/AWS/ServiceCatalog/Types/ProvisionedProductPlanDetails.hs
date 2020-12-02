{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanDetails where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanStatus
import Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanType
import Network.AWS.ServiceCatalog.Types.Tag
import Network.AWS.ServiceCatalog.Types.UpdateProvisioningParameter

-- | Information about a plan.
--
--
--
-- /See:/ 'provisionedProductPlanDetails' smart constructor.
data ProvisionedProductPlanDetails = ProvisionedProductPlanDetails'
  { _pppdStatus ::
      !( Maybe
           ProvisionedProductPlanStatus
       ),
    _pppdProvisionProductId ::
      !(Maybe Text),
    _pppdProvisioningArtifactId ::
      !(Maybe Text),
    _pppdProvisionProductName ::
      !(Maybe Text),
    _pppdCreatedTime ::
      !(Maybe POSIX),
    _pppdNotificationARNs ::
      !(Maybe [Text]),
    _pppdPlanId :: !(Maybe Text),
    _pppdPlanName :: !(Maybe Text),
    _pppdStatusMessage ::
      !(Maybe Text),
    _pppdUpdatedTime ::
      !(Maybe POSIX),
    _pppdPathId :: !(Maybe Text),
    _pppdProvisioningParameters ::
      !( Maybe
           [UpdateProvisioningParameter]
       ),
    _pppdPlanType ::
      !( Maybe
           ProvisionedProductPlanType
       ),
    _pppdProductId :: !(Maybe Text),
    _pppdTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProvisionedProductPlanDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pppdStatus' - The status.
--
-- * 'pppdProvisionProductId' - The product identifier.
--
-- * 'pppdProvisioningArtifactId' - The identifier of the provisioning artifact.
--
-- * 'pppdProvisionProductName' - The user-friendly name of the provisioned product.
--
-- * 'pppdCreatedTime' - The UTC time stamp of the creation time.
--
-- * 'pppdNotificationARNs' - Passed to CloudFormation. The SNS topic ARNs to which to publish stack-related events.
--
-- * 'pppdPlanId' - The plan identifier.
--
-- * 'pppdPlanName' - The name of the plan.
--
-- * 'pppdStatusMessage' - The status message.
--
-- * 'pppdUpdatedTime' - The time when the plan was last updated.
--
-- * 'pppdPathId' - The path identifier of the product. This value is optional if the product has a default path, and required if the product has more than one path. To list the paths for a product, use 'ListLaunchPaths' .
--
-- * 'pppdProvisioningParameters' - Parameters specified by the administrator that are required for provisioning the product.
--
-- * 'pppdPlanType' - The plan type.
--
-- * 'pppdProductId' - The product identifier.
--
-- * 'pppdTags' - One or more tags.
provisionedProductPlanDetails ::
  ProvisionedProductPlanDetails
provisionedProductPlanDetails =
  ProvisionedProductPlanDetails'
    { _pppdStatus = Nothing,
      _pppdProvisionProductId = Nothing,
      _pppdProvisioningArtifactId = Nothing,
      _pppdProvisionProductName = Nothing,
      _pppdCreatedTime = Nothing,
      _pppdNotificationARNs = Nothing,
      _pppdPlanId = Nothing,
      _pppdPlanName = Nothing,
      _pppdStatusMessage = Nothing,
      _pppdUpdatedTime = Nothing,
      _pppdPathId = Nothing,
      _pppdProvisioningParameters = Nothing,
      _pppdPlanType = Nothing,
      _pppdProductId = Nothing,
      _pppdTags = Nothing
    }

-- | The status.
pppdStatus :: Lens' ProvisionedProductPlanDetails (Maybe ProvisionedProductPlanStatus)
pppdStatus = lens _pppdStatus (\s a -> s {_pppdStatus = a})

-- | The product identifier.
pppdProvisionProductId :: Lens' ProvisionedProductPlanDetails (Maybe Text)
pppdProvisionProductId = lens _pppdProvisionProductId (\s a -> s {_pppdProvisionProductId = a})

-- | The identifier of the provisioning artifact.
pppdProvisioningArtifactId :: Lens' ProvisionedProductPlanDetails (Maybe Text)
pppdProvisioningArtifactId = lens _pppdProvisioningArtifactId (\s a -> s {_pppdProvisioningArtifactId = a})

-- | The user-friendly name of the provisioned product.
pppdProvisionProductName :: Lens' ProvisionedProductPlanDetails (Maybe Text)
pppdProvisionProductName = lens _pppdProvisionProductName (\s a -> s {_pppdProvisionProductName = a})

-- | The UTC time stamp of the creation time.
pppdCreatedTime :: Lens' ProvisionedProductPlanDetails (Maybe UTCTime)
pppdCreatedTime = lens _pppdCreatedTime (\s a -> s {_pppdCreatedTime = a}) . mapping _Time

-- | Passed to CloudFormation. The SNS topic ARNs to which to publish stack-related events.
pppdNotificationARNs :: Lens' ProvisionedProductPlanDetails [Text]
pppdNotificationARNs = lens _pppdNotificationARNs (\s a -> s {_pppdNotificationARNs = a}) . _Default . _Coerce

-- | The plan identifier.
pppdPlanId :: Lens' ProvisionedProductPlanDetails (Maybe Text)
pppdPlanId = lens _pppdPlanId (\s a -> s {_pppdPlanId = a})

-- | The name of the plan.
pppdPlanName :: Lens' ProvisionedProductPlanDetails (Maybe Text)
pppdPlanName = lens _pppdPlanName (\s a -> s {_pppdPlanName = a})

-- | The status message.
pppdStatusMessage :: Lens' ProvisionedProductPlanDetails (Maybe Text)
pppdStatusMessage = lens _pppdStatusMessage (\s a -> s {_pppdStatusMessage = a})

-- | The time when the plan was last updated.
pppdUpdatedTime :: Lens' ProvisionedProductPlanDetails (Maybe UTCTime)
pppdUpdatedTime = lens _pppdUpdatedTime (\s a -> s {_pppdUpdatedTime = a}) . mapping _Time

-- | The path identifier of the product. This value is optional if the product has a default path, and required if the product has more than one path. To list the paths for a product, use 'ListLaunchPaths' .
pppdPathId :: Lens' ProvisionedProductPlanDetails (Maybe Text)
pppdPathId = lens _pppdPathId (\s a -> s {_pppdPathId = a})

-- | Parameters specified by the administrator that are required for provisioning the product.
pppdProvisioningParameters :: Lens' ProvisionedProductPlanDetails [UpdateProvisioningParameter]
pppdProvisioningParameters = lens _pppdProvisioningParameters (\s a -> s {_pppdProvisioningParameters = a}) . _Default . _Coerce

-- | The plan type.
pppdPlanType :: Lens' ProvisionedProductPlanDetails (Maybe ProvisionedProductPlanType)
pppdPlanType = lens _pppdPlanType (\s a -> s {_pppdPlanType = a})

-- | The product identifier.
pppdProductId :: Lens' ProvisionedProductPlanDetails (Maybe Text)
pppdProductId = lens _pppdProductId (\s a -> s {_pppdProductId = a})

-- | One or more tags.
pppdTags :: Lens' ProvisionedProductPlanDetails [Tag]
pppdTags = lens _pppdTags (\s a -> s {_pppdTags = a}) . _Default . _Coerce

instance FromJSON ProvisionedProductPlanDetails where
  parseJSON =
    withObject
      "ProvisionedProductPlanDetails"
      ( \x ->
          ProvisionedProductPlanDetails'
            <$> (x .:? "Status")
            <*> (x .:? "ProvisionProductId")
            <*> (x .:? "ProvisioningArtifactId")
            <*> (x .:? "ProvisionProductName")
            <*> (x .:? "CreatedTime")
            <*> (x .:? "NotificationArns" .!= mempty)
            <*> (x .:? "PlanId")
            <*> (x .:? "PlanName")
            <*> (x .:? "StatusMessage")
            <*> (x .:? "UpdatedTime")
            <*> (x .:? "PathId")
            <*> (x .:? "ProvisioningParameters" .!= mempty)
            <*> (x .:? "PlanType")
            <*> (x .:? "ProductId")
            <*> (x .:? "Tags" .!= mempty)
      )

instance Hashable ProvisionedProductPlanDetails

instance NFData ProvisionedProductPlanDetails
