{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.CloudWatchLogsTimeZone
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.CloudWatchLogsTimeZone
  ( CloudWatchLogsTimeZone
      ( CloudWatchLogsTimeZone',
        CloudWatchLogsTimeZoneLocal,
        CloudWatchLogsTimeZoneUtc,
        fromCloudWatchLogsTimeZone
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | The preferred time zone for logs streamed to CloudWatch Logs. Valid values are @LOCAL@ and @UTC@ , for Coordinated Universal Time.
newtype CloudWatchLogsTimeZone = CloudWatchLogsTimeZone'
  { fromCloudWatchLogsTimeZone ::
      Core.Text
  }
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

pattern CloudWatchLogsTimeZoneLocal :: CloudWatchLogsTimeZone
pattern CloudWatchLogsTimeZoneLocal = CloudWatchLogsTimeZone' "LOCAL"

pattern CloudWatchLogsTimeZoneUtc :: CloudWatchLogsTimeZone
pattern CloudWatchLogsTimeZoneUtc = CloudWatchLogsTimeZone' "UTC"

{-# COMPLETE
  CloudWatchLogsTimeZoneLocal,
  CloudWatchLogsTimeZoneUtc,
  CloudWatchLogsTimeZone'
  #-}
