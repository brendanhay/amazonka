{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisioningArtifactSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisioningArtifactSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Summary information about a provisioning artifact (also known as a version) for a product.
--
--
--
-- /See:/ 'provisioningArtifactSummary' smart constructor.
data ProvisioningArtifactSummary = ProvisioningArtifactSummary'
  { _pasProvisioningArtifactMetadata ::
      !(Maybe (Map Text (Text))),
    _pasCreatedTime :: !(Maybe POSIX),
    _pasName :: !(Maybe Text),
    _pasId :: !(Maybe Text),
    _pasDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProvisioningArtifactSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pasProvisioningArtifactMetadata' - The metadata for the provisioning artifact. This is used with AWS Marketplace products.
--
-- * 'pasCreatedTime' - The UTC time stamp of the creation time.
--
-- * 'pasName' - The name of the provisioning artifact.
--
-- * 'pasId' - The identifier of the provisioning artifact.
--
-- * 'pasDescription' - The description of the provisioning artifact.
provisioningArtifactSummary ::
  ProvisioningArtifactSummary
provisioningArtifactSummary =
  ProvisioningArtifactSummary'
    { _pasProvisioningArtifactMetadata =
        Nothing,
      _pasCreatedTime = Nothing,
      _pasName = Nothing,
      _pasId = Nothing,
      _pasDescription = Nothing
    }

-- | The metadata for the provisioning artifact. This is used with AWS Marketplace products.
pasProvisioningArtifactMetadata :: Lens' ProvisioningArtifactSummary (HashMap Text (Text))
pasProvisioningArtifactMetadata = lens _pasProvisioningArtifactMetadata (\s a -> s {_pasProvisioningArtifactMetadata = a}) . _Default . _Map

-- | The UTC time stamp of the creation time.
pasCreatedTime :: Lens' ProvisioningArtifactSummary (Maybe UTCTime)
pasCreatedTime = lens _pasCreatedTime (\s a -> s {_pasCreatedTime = a}) . mapping _Time

-- | The name of the provisioning artifact.
pasName :: Lens' ProvisioningArtifactSummary (Maybe Text)
pasName = lens _pasName (\s a -> s {_pasName = a})

-- | The identifier of the provisioning artifact.
pasId :: Lens' ProvisioningArtifactSummary (Maybe Text)
pasId = lens _pasId (\s a -> s {_pasId = a})

-- | The description of the provisioning artifact.
pasDescription :: Lens' ProvisioningArtifactSummary (Maybe Text)
pasDescription = lens _pasDescription (\s a -> s {_pasDescription = a})

instance FromJSON ProvisioningArtifactSummary where
  parseJSON =
    withObject
      "ProvisioningArtifactSummary"
      ( \x ->
          ProvisioningArtifactSummary'
            <$> (x .:? "ProvisioningArtifactMetadata" .!= mempty)
            <*> (x .:? "CreatedTime")
            <*> (x .:? "Name")
            <*> (x .:? "Id")
            <*> (x .:? "Description")
      )

instance Hashable ProvisioningArtifactSummary

instance NFData ProvisioningArtifactSummary
