{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype LogDriver = LogDriver'
  { fromLogDriver ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
