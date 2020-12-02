{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.DomainConfigurationSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.DomainConfigurationSummary where

import Network.AWS.IoT.Types.ServiceType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The summary of a domain configuration. A domain configuration specifies custom IoT-specific information about a domain. A domain configuration can be associated with an AWS-managed domain (for example, dbc123defghijk.iot.us-west-2.amazonaws.com), a customer managed domain, or a default endpoint.
--
--
--     * Data
--
--     * Jobs
--
--     * CredentialProvider
--
--
--
--
-- /See:/ 'domainConfigurationSummary' smart constructor.
data DomainConfigurationSummary = DomainConfigurationSummary'
  { _dcsDomainConfigurationName ::
      !(Maybe Text),
    _dcsDomainConfigurationARN ::
      !(Maybe Text),
    _dcsServiceType ::
      !(Maybe ServiceType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DomainConfigurationSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsDomainConfigurationName' - The name of the domain configuration. This value must be unique to a region.
--
-- * 'dcsDomainConfigurationARN' - The ARN of the domain configuration.
--
-- * 'dcsServiceType' - The type of service delivered by the endpoint.
domainConfigurationSummary ::
  DomainConfigurationSummary
domainConfigurationSummary =
  DomainConfigurationSummary'
    { _dcsDomainConfigurationName =
        Nothing,
      _dcsDomainConfigurationARN = Nothing,
      _dcsServiceType = Nothing
    }

-- | The name of the domain configuration. This value must be unique to a region.
dcsDomainConfigurationName :: Lens' DomainConfigurationSummary (Maybe Text)
dcsDomainConfigurationName = lens _dcsDomainConfigurationName (\s a -> s {_dcsDomainConfigurationName = a})

-- | The ARN of the domain configuration.
dcsDomainConfigurationARN :: Lens' DomainConfigurationSummary (Maybe Text)
dcsDomainConfigurationARN = lens _dcsDomainConfigurationARN (\s a -> s {_dcsDomainConfigurationARN = a})

-- | The type of service delivered by the endpoint.
dcsServiceType :: Lens' DomainConfigurationSummary (Maybe ServiceType)
dcsServiceType = lens _dcsServiceType (\s a -> s {_dcsServiceType = a})

instance FromJSON DomainConfigurationSummary where
  parseJSON =
    withObject
      "DomainConfigurationSummary"
      ( \x ->
          DomainConfigurationSummary'
            <$> (x .:? "domainConfigurationName")
            <*> (x .:? "domainConfigurationArn")
            <*> (x .:? "serviceType")
      )

instance Hashable DomainConfigurationSummary

instance NFData DomainConfigurationSummary
