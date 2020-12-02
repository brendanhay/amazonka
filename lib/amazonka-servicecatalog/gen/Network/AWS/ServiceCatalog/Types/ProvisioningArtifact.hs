{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisioningArtifact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisioningArtifact where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactGuidance

-- | Information about a provisioning artifact. A provisioning artifact is also known as a product version.
--
--
--
-- /See:/ 'provisioningArtifact' smart constructor.
data ProvisioningArtifact = ProvisioningArtifact'
  { _paCreatedTime ::
      !(Maybe POSIX),
    _paName :: !(Maybe Text),
    _paId :: !(Maybe Text),
    _paGuidance ::
      !(Maybe ProvisioningArtifactGuidance),
    _paDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProvisioningArtifact' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'paCreatedTime' - The UTC time stamp of the creation time.
--
-- * 'paName' - The name of the provisioning artifact.
--
-- * 'paId' - The identifier of the provisioning artifact.
--
-- * 'paGuidance' - Information set by the administrator to provide guidance to end users about which provisioning artifacts to use.
--
-- * 'paDescription' - The description of the provisioning artifact.
provisioningArtifact ::
  ProvisioningArtifact
provisioningArtifact =
  ProvisioningArtifact'
    { _paCreatedTime = Nothing,
      _paName = Nothing,
      _paId = Nothing,
      _paGuidance = Nothing,
      _paDescription = Nothing
    }

-- | The UTC time stamp of the creation time.
paCreatedTime :: Lens' ProvisioningArtifact (Maybe UTCTime)
paCreatedTime = lens _paCreatedTime (\s a -> s {_paCreatedTime = a}) . mapping _Time

-- | The name of the provisioning artifact.
paName :: Lens' ProvisioningArtifact (Maybe Text)
paName = lens _paName (\s a -> s {_paName = a})

-- | The identifier of the provisioning artifact.
paId :: Lens' ProvisioningArtifact (Maybe Text)
paId = lens _paId (\s a -> s {_paId = a})

-- | Information set by the administrator to provide guidance to end users about which provisioning artifacts to use.
paGuidance :: Lens' ProvisioningArtifact (Maybe ProvisioningArtifactGuidance)
paGuidance = lens _paGuidance (\s a -> s {_paGuidance = a})

-- | The description of the provisioning artifact.
paDescription :: Lens' ProvisioningArtifact (Maybe Text)
paDescription = lens _paDescription (\s a -> s {_paDescription = a})

instance FromJSON ProvisioningArtifact where
  parseJSON =
    withObject
      "ProvisioningArtifact"
      ( \x ->
          ProvisioningArtifact'
            <$> (x .:? "CreatedTime")
            <*> (x .:? "Name")
            <*> (x .:? "Id")
            <*> (x .:? "Guidance")
            <*> (x .:? "Description")
      )

instance Hashable ProvisioningArtifact

instance NFData ProvisioningArtifact
