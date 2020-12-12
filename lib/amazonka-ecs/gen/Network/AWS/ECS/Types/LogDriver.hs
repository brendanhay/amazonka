{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.LogDriver
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.LogDriver
  ( LogDriver
      ( LogDriver',
        LDAWSfirelens,
        LDAWSlogs,
        LDFluentd,
        LDGelf,
        LDJSONFile,
        LDJournald,
        LDSplunk,
        LDSyslog
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype LogDriver = LogDriver' Lude.Text
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

pattern LDAWSfirelens :: LogDriver
pattern LDAWSfirelens = LogDriver' "awsfirelens"

pattern LDAWSlogs :: LogDriver
pattern LDAWSlogs = LogDriver' "awslogs"

pattern LDFluentd :: LogDriver
pattern LDFluentd = LogDriver' "fluentd"

pattern LDGelf :: LogDriver
pattern LDGelf = LogDriver' "gelf"

pattern LDJSONFile :: LogDriver
pattern LDJSONFile = LogDriver' "json-file"

pattern LDJournald :: LogDriver
pattern LDJournald = LogDriver' "journald"

pattern LDSplunk :: LogDriver
pattern LDSplunk = LogDriver' "splunk"

pattern LDSyslog :: LogDriver
pattern LDSyslog = LogDriver' "syslog"

{-# COMPLETE
  LDAWSfirelens,
  LDAWSlogs,
  LDFluentd,
  LDGelf,
  LDJSONFile,
  LDJournald,
  LDSplunk,
  LDSyslog,
  LogDriver'
  #-}
