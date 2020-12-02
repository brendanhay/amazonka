{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.FlowLogsConfigurationResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.FlowLogsConfigurationResult where

import Network.AWS.GuardDuty.Types.DataSourceStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information on the status of VPC flow logs as a data source.
--
--
--
-- /See:/ 'flowLogsConfigurationResult' smart constructor.
newtype FlowLogsConfigurationResult = FlowLogsConfigurationResult'
  { _flcrStatus ::
      DataSourceStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FlowLogsConfigurationResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'flcrStatus' - Denotes whether VPC flow logs is enabled as a data source.
flowLogsConfigurationResult ::
  -- | 'flcrStatus'
  DataSourceStatus ->
  FlowLogsConfigurationResult
flowLogsConfigurationResult pStatus_ =
  FlowLogsConfigurationResult' {_flcrStatus = pStatus_}

-- | Denotes whether VPC flow logs is enabled as a data source.
flcrStatus :: Lens' FlowLogsConfigurationResult DataSourceStatus
flcrStatus = lens _flcrStatus (\s a -> s {_flcrStatus = a})

instance FromJSON FlowLogsConfigurationResult where
  parseJSON =
    withObject
      "FlowLogsConfigurationResult"
      (\x -> FlowLogsConfigurationResult' <$> (x .: "status"))

instance Hashable FlowLogsConfigurationResult

instance NFData FlowLogsConfigurationResult
