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
-- Module      : Amazonka.ECS.Types.LogDriver
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.LogDriver
  ( LogDriver
      ( ..,
        LogDriver_Awsfirelens,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LogDriver = LogDriver'
  { fromLogDriver ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern LogDriver_Awsfirelens :: LogDriver
pattern LogDriver_Awsfirelens = LogDriver' "awsfirelens"

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
  LogDriver_Awsfirelens,
  LogDriver_Awslogs,
  LogDriver_Fluentd,
  LogDriver_Gelf,
  LogDriver_Journald,
  LogDriver_Json_file,
  LogDriver_Splunk,
  LogDriver_Syslog,
  LogDriver'
  #-}
