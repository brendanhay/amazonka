{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.LogDriver
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.LogDriver
  ( LogDriver
      ( LogDriver',
        LogDriverJsonFile,
        LogDriverSyslog,
        LogDriverJournald,
        LogDriverGelf,
        LogDriverFluentd,
        LogDriverAwslogs,
        LogDriverSplunk,
        fromLogDriver
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype LogDriver = LogDriver' {fromLogDriver :: Core.Text}
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

pattern LogDriverJsonFile :: LogDriver
pattern LogDriverJsonFile = LogDriver' "json-file"

pattern LogDriverSyslog :: LogDriver
pattern LogDriverSyslog = LogDriver' "syslog"

pattern LogDriverJournald :: LogDriver
pattern LogDriverJournald = LogDriver' "journald"

pattern LogDriverGelf :: LogDriver
pattern LogDriverGelf = LogDriver' "gelf"

pattern LogDriverFluentd :: LogDriver
pattern LogDriverFluentd = LogDriver' "fluentd"

pattern LogDriverAwslogs :: LogDriver
pattern LogDriverAwslogs = LogDriver' "awslogs"

pattern LogDriverSplunk :: LogDriver
pattern LogDriverSplunk = LogDriver' "splunk"

{-# COMPLETE
  LogDriverJsonFile,
  LogDriverSyslog,
  LogDriverJournald,
  LogDriverGelf,
  LogDriverFluentd,
  LogDriverAwslogs,
  LogDriverSplunk,
  LogDriver'
  #-}
