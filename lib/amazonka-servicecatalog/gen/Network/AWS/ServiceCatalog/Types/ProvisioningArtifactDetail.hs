{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisioningArtifactDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisioningArtifactDetail where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactGuidance
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactType

-- | Information about a provisioning artifact (also known as a version) for a product.
--
--
--
-- /See:/ 'provisioningArtifactDetail' smart constructor.
data ProvisioningArtifactDetail = ProvisioningArtifactDetail'
  { _padCreatedTime ::
      !(Maybe POSIX),
    _padActive :: !(Maybe Bool),
    _padName :: !(Maybe Text),
    _padId :: !(Maybe Text),
    _padType ::
      !(Maybe ProvisioningArtifactType),
    _padGuidance ::
      !(Maybe ProvisioningArtifactGuidance),
    _padDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProvisioningArtifactDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'padCreatedTime' - The UTC time stamp of the creation time.
--
-- * 'padActive' - Indicates whether the product version is active.
--
-- * 'padName' - The name of the provisioning artifact.
--
-- * 'padId' - The identifier of the provisioning artifact.
--
-- * 'padType' - The type of provisioning artifact.     * @CLOUD_FORMATION_TEMPLATE@ - AWS CloudFormation template     * @MARKETPLACE_AMI@ - AWS Marketplace AMI     * @MARKETPLACE_CAR@ - AWS Marketplace Clusters and AWS Resources
--
-- * 'padGuidance' - Information set by the administrator to provide guidance to end users about which provisioning artifacts to use.
--
-- * 'padDescription' - The description of the provisioning artifact.
provisioningArtifactDetail ::
  ProvisioningArtifactDetail
provisioningArtifactDetail =
  ProvisioningArtifactDetail'
    { _padCreatedTime = Nothing,
      _padActive = Nothing,
      _padName = Nothing,
      _padId = Nothing,
      _padType = Nothing,
      _padGuidance = Nothing,
      _padDescription = Nothing
    }

-- | The UTC time stamp of the creation time.
padCreatedTime :: Lens' ProvisioningArtifactDetail (Maybe UTCTime)
padCreatedTime = lens _padCreatedTime (\s a -> s {_padCreatedTime = a}) . mapping _Time

-- | Indicates whether the product version is active.
padActive :: Lens' ProvisioningArtifactDetail (Maybe Bool)
padActive = lens _padActive (\s a -> s {_padActive = a})

-- | The name of the provisioning artifact.
padName :: Lens' ProvisioningArtifactDetail (Maybe Text)
padName = lens _padName (\s a -> s {_padName = a})

-- | The identifier of the provisioning artifact.
padId :: Lens' ProvisioningArtifactDetail (Maybe Text)
padId = lens _padId (\s a -> s {_padId = a})

-- | The type of provisioning artifact.     * @CLOUD_FORMATION_TEMPLATE@ - AWS CloudFormation template     * @MARKETPLACE_AMI@ - AWS Marketplace AMI     * @MARKETPLACE_CAR@ - AWS Marketplace Clusters and AWS Resources
padType :: Lens' ProvisioningArtifactDetail (Maybe ProvisioningArtifactType)
padType = lens _padType (\s a -> s {_padType = a})

-- | Information set by the administrator to provide guidance to end users about which provisioning artifacts to use.
padGuidance :: Lens' ProvisioningArtifactDetail (Maybe ProvisioningArtifactGuidance)
padGuidance = lens _padGuidance (\s a -> s {_padGuidance = a})

-- | The description of the provisioning artifact.
padDescription :: Lens' ProvisioningArtifactDetail (Maybe Text)
padDescription = lens _padDescription (\s a -> s {_padDescription = a})

instance FromJSON ProvisioningArtifactDetail where
  parseJSON =
    withObject
      "ProvisioningArtifactDetail"
      ( \x ->
          ProvisioningArtifactDetail'
            <$> (x .:? "CreatedTime")
            <*> (x .:? "Active")
            <*> (x .:? "Name")
            <*> (x .:? "Id")
            <*> (x .:? "Type")
            <*> (x .:? "Guidance")
            <*> (x .:? "Description")
      )

instance Hashable ProvisioningArtifactDetail

instance NFData ProvisioningArtifactDetail
