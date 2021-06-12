{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.LogDriver
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.LogDriver
  ( LogDriver
      ( ..,
        LogDriver_Awslogs,
        LogDriver_Fluentd,
        LogDriver_Gelf,
        LogDriver_Journald,
        LogDriver_Json_file,
        LogDriver_Splunk,
        LogDriver_Syslog
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype LogDriver = LogDriver'
  { fromLogDriver ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern LogDriver_Awslogs :: LogDriver
pattern LogDriver_Awslogs = LogDriver' "awslogs"

pattern LogDriver_Fluentd :: LogDriver
pattern LogDriver_Fluentd = LogDriver' "fluentd"

pattern LogDriver_Gelf :: LogDriver
pattern LogDriver_Gelf = LogDriver' "gelf"

pattern LogDriver_Journald :: LogDriver
pattern LogDriver_Journald = LogDriver' "journald"

pattern LogDriver_Json_file :: LogDriver
pattern LogDriver_Json_file = LogDriver' "json-file"

pattern LogDriver_Splunk :: LogDriver
pattern LogDriver_Splunk = LogDriver' "splunk"

pattern LogDriver_Syslog :: LogDriver
pattern LogDriver_Syslog = LogDriver' "syslog"

{-# COMPLETE
  LogDriver_Awslogs,
  LogDriver_Fluentd,
  LogDriver_Gelf,
  LogDriver_Journald,
  LogDriver_Json_file,
  LogDriver_Splunk,
  LogDriver_Syslog,
  LogDriver'
  #-}
