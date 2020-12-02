{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.BrokerInstanceOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.BrokerInstanceOption where

import Network.AWS.Lens
import Network.AWS.MQ.Types.AvailabilityZone
import Network.AWS.MQ.Types.BrokerStorageType
import Network.AWS.MQ.Types.DeploymentMode
import Network.AWS.MQ.Types.EngineType
import Network.AWS.Prelude

-- | Option for host instance type.
--
-- /See:/ 'brokerInstanceOption' smart constructor.
data BrokerInstanceOption = BrokerInstanceOption'
  { _bioSupportedEngineVersions ::
      !(Maybe [Text]),
    _bioAvailabilityZones ::
      !(Maybe [AvailabilityZone]),
    _bioSupportedDeploymentModes ::
      !(Maybe [DeploymentMode]),
    _bioEngineType :: !(Maybe EngineType),
    _bioHostInstanceType :: !(Maybe Text),
    _bioStorageType :: !(Maybe BrokerStorageType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BrokerInstanceOption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bioSupportedEngineVersions' - The list of supported engine versions.
--
-- * 'bioAvailabilityZones' - The list of available az.
--
-- * 'bioSupportedDeploymentModes' - The list of supported deployment modes.
--
-- * 'bioEngineType' - The type of broker engine.
--
-- * 'bioHostInstanceType' - The type of broker instance.
--
-- * 'bioStorageType' - The broker's storage type.
brokerInstanceOption ::
  BrokerInstanceOption
brokerInstanceOption =
  BrokerInstanceOption'
    { _bioSupportedEngineVersions = Nothing,
      _bioAvailabilityZones = Nothing,
      _bioSupportedDeploymentModes = Nothing,
      _bioEngineType = Nothing,
      _bioHostInstanceType = Nothing,
      _bioStorageType = Nothing
    }

-- | The list of supported engine versions.
bioSupportedEngineVersions :: Lens' BrokerInstanceOption [Text]
bioSupportedEngineVersions = lens _bioSupportedEngineVersions (\s a -> s {_bioSupportedEngineVersions = a}) . _Default . _Coerce

-- | The list of available az.
bioAvailabilityZones :: Lens' BrokerInstanceOption [AvailabilityZone]
bioAvailabilityZones = lens _bioAvailabilityZones (\s a -> s {_bioAvailabilityZones = a}) . _Default . _Coerce

-- | The list of supported deployment modes.
bioSupportedDeploymentModes :: Lens' BrokerInstanceOption [DeploymentMode]
bioSupportedDeploymentModes = lens _bioSupportedDeploymentModes (\s a -> s {_bioSupportedDeploymentModes = a}) . _Default . _Coerce

-- | The type of broker engine.
bioEngineType :: Lens' BrokerInstanceOption (Maybe EngineType)
bioEngineType = lens _bioEngineType (\s a -> s {_bioEngineType = a})

-- | The type of broker instance.
bioHostInstanceType :: Lens' BrokerInstanceOption (Maybe Text)
bioHostInstanceType = lens _bioHostInstanceType (\s a -> s {_bioHostInstanceType = a})

-- | The broker's storage type.
bioStorageType :: Lens' BrokerInstanceOption (Maybe BrokerStorageType)
bioStorageType = lens _bioStorageType (\s a -> s {_bioStorageType = a})

instance FromJSON BrokerInstanceOption where
  parseJSON =
    withObject
      "BrokerInstanceOption"
      ( \x ->
          BrokerInstanceOption'
            <$> (x .:? "supportedEngineVersions" .!= mempty)
            <*> (x .:? "availabilityZones" .!= mempty)
            <*> (x .:? "supportedDeploymentModes" .!= mempty)
            <*> (x .:? "engineType")
            <*> (x .:? "hostInstanceType")
            <*> (x .:? "storageType")
      )

instance Hashable BrokerInstanceOption

instance NFData BrokerInstanceOption
