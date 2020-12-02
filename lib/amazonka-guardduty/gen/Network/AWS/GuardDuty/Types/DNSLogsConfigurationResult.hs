{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.DNSLogsConfigurationResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.DNSLogsConfigurationResult where

import Network.AWS.GuardDuty.Types.DataSourceStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information on the status of DNS logs as a data source.
--
--
--
-- /See:/ 'dnsLogsConfigurationResult' smart constructor.
newtype DNSLogsConfigurationResult = DNSLogsConfigurationResult'
  { _dlcrStatus ::
      DataSourceStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DNSLogsConfigurationResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlcrStatus' - Denotes whether DNS logs is enabled as a data source.
dnsLogsConfigurationResult ::
  -- | 'dlcrStatus'
  DataSourceStatus ->
  DNSLogsConfigurationResult
dnsLogsConfigurationResult pStatus_ =
  DNSLogsConfigurationResult' {_dlcrStatus = pStatus_}

-- | Denotes whether DNS logs is enabled as a data source.
dlcrStatus :: Lens' DNSLogsConfigurationResult DataSourceStatus
dlcrStatus = lens _dlcrStatus (\s a -> s {_dlcrStatus = a})

instance FromJSON DNSLogsConfigurationResult where
  parseJSON =
    withObject
      "DNSLogsConfigurationResult"
      (\x -> DNSLogsConfigurationResult' <$> (x .: "status"))

instance Hashable DNSLogsConfigurationResult

instance NFData DNSLogsConfigurationResult
