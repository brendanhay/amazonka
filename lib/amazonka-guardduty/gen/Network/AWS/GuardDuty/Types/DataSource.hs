{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.DataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.DataSource
  ( DataSource
      ( DataSource',
        DataSourceFlowLogs,
        DataSourceCloudTrail,
        DataSourceDnsLogs,
        DataSourceS3Logs,
        fromDataSource
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype DataSource = DataSource' {fromDataSource :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern DataSourceFlowLogs :: DataSource
pattern DataSourceFlowLogs = DataSource' "FLOW_LOGS"

pattern DataSourceCloudTrail :: DataSource
pattern DataSourceCloudTrail = DataSource' "CLOUD_TRAIL"

pattern DataSourceDnsLogs :: DataSource
pattern DataSourceDnsLogs = DataSource' "DNS_LOGS"

pattern DataSourceS3Logs :: DataSource
pattern DataSourceS3Logs = DataSource' "S3_LOGS"

{-# COMPLETE
  DataSourceFlowLogs,
  DataSourceCloudTrail,
  DataSourceDnsLogs,
  DataSourceS3Logs,
  DataSource'
  #-}
