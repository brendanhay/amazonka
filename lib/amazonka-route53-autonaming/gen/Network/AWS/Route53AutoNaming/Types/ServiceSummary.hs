{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.ServiceSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.ServiceSummary where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53AutoNaming.Types.DNSConfig
import Network.AWS.Route53AutoNaming.Types.HealthCheckConfig
import Network.AWS.Route53AutoNaming.Types.HealthCheckCustomConfig

-- | A complex type that contains information about a specified service.
--
--
--
-- /See:/ 'serviceSummary' smart constructor.
data ServiceSummary = ServiceSummary'
  { _ssInstanceCount ::
      !(Maybe Int),
    _ssARN :: !(Maybe Text),
    _ssHealthCheckConfig :: !(Maybe HealthCheckConfig),
    _ssCreateDate :: !(Maybe POSIX),
    _ssHealthCheckCustomConfig ::
      !(Maybe HealthCheckCustomConfig),
    _ssName :: !(Maybe Text),
    _ssId :: !(Maybe Text),
    _ssDNSConfig :: !(Maybe DNSConfig),
    _ssDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ServiceSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssInstanceCount' - The number of instances that are currently associated with the service. Instances that were previously associated with the service but that have been deleted are not included in the count. The count might not reflect pending registrations and deregistrations.
--
-- * 'ssARN' - The Amazon Resource Name (ARN) that AWS Cloud Map assigns to the service when you create it.
--
-- * 'ssHealthCheckConfig' - Undocumented member.
--
-- * 'ssCreateDate' - The date and time that the service was created.
--
-- * 'ssHealthCheckCustomConfig' - Undocumented member.
--
-- * 'ssName' - The name of the service.
--
-- * 'ssId' - The ID that AWS Cloud Map assigned to the service when you created it.
--
-- * 'ssDNSConfig' - Undocumented member.
--
-- * 'ssDescription' - The description that you specify when you create the service.
serviceSummary ::
  ServiceSummary
serviceSummary =
  ServiceSummary'
    { _ssInstanceCount = Nothing,
      _ssARN = Nothing,
      _ssHealthCheckConfig = Nothing,
      _ssCreateDate = Nothing,
      _ssHealthCheckCustomConfig = Nothing,
      _ssName = Nothing,
      _ssId = Nothing,
      _ssDNSConfig = Nothing,
      _ssDescription = Nothing
    }

-- | The number of instances that are currently associated with the service. Instances that were previously associated with the service but that have been deleted are not included in the count. The count might not reflect pending registrations and deregistrations.
ssInstanceCount :: Lens' ServiceSummary (Maybe Int)
ssInstanceCount = lens _ssInstanceCount (\s a -> s {_ssInstanceCount = a})

-- | The Amazon Resource Name (ARN) that AWS Cloud Map assigns to the service when you create it.
ssARN :: Lens' ServiceSummary (Maybe Text)
ssARN = lens _ssARN (\s a -> s {_ssARN = a})

-- | Undocumented member.
ssHealthCheckConfig :: Lens' ServiceSummary (Maybe HealthCheckConfig)
ssHealthCheckConfig = lens _ssHealthCheckConfig (\s a -> s {_ssHealthCheckConfig = a})

-- | The date and time that the service was created.
ssCreateDate :: Lens' ServiceSummary (Maybe UTCTime)
ssCreateDate = lens _ssCreateDate (\s a -> s {_ssCreateDate = a}) . mapping _Time

-- | Undocumented member.
ssHealthCheckCustomConfig :: Lens' ServiceSummary (Maybe HealthCheckCustomConfig)
ssHealthCheckCustomConfig = lens _ssHealthCheckCustomConfig (\s a -> s {_ssHealthCheckCustomConfig = a})

-- | The name of the service.
ssName :: Lens' ServiceSummary (Maybe Text)
ssName = lens _ssName (\s a -> s {_ssName = a})

-- | The ID that AWS Cloud Map assigned to the service when you created it.
ssId :: Lens' ServiceSummary (Maybe Text)
ssId = lens _ssId (\s a -> s {_ssId = a})

-- | Undocumented member.
ssDNSConfig :: Lens' ServiceSummary (Maybe DNSConfig)
ssDNSConfig = lens _ssDNSConfig (\s a -> s {_ssDNSConfig = a})

-- | The description that you specify when you create the service.
ssDescription :: Lens' ServiceSummary (Maybe Text)
ssDescription = lens _ssDescription (\s a -> s {_ssDescription = a})

instance FromJSON ServiceSummary where
  parseJSON =
    withObject
      "ServiceSummary"
      ( \x ->
          ServiceSummary'
            <$> (x .:? "InstanceCount")
            <*> (x .:? "Arn")
            <*> (x .:? "HealthCheckConfig")
            <*> (x .:? "CreateDate")
            <*> (x .:? "HealthCheckCustomConfig")
            <*> (x .:? "Name")
            <*> (x .:? "Id")
            <*> (x .:? "DnsConfig")
            <*> (x .:? "Description")
      )

instance Hashable ServiceSummary

instance NFData ServiceSummary
