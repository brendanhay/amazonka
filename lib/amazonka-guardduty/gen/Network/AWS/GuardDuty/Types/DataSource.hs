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
        CloudTrail,
        DNSLogs,
        FlowLogs,
        S3Logs
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DataSource = DataSource' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern CloudTrail :: DataSource
pattern CloudTrail = DataSource' "CLOUD_TRAIL"

pattern DNSLogs :: DataSource
pattern DNSLogs = DataSource' "DNS_LOGS"

pattern FlowLogs :: DataSource
pattern FlowLogs = DataSource' "FLOW_LOGS"

pattern S3Logs :: DataSource
pattern S3Logs = DataSource' "S3_LOGS"

{-# COMPLETE
  CloudTrail,
  DNSLogs,
  FlowLogs,
  S3Logs,
  DataSource'
  #-}
