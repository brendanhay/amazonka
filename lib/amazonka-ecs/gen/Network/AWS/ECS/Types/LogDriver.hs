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
        LDJSONFile,
        LDSyslog,
        LDJournald,
        LDGelf,
        LDFluentd,
        LDAWSlogs,
        LDSplunk,
        LDAWSfirelens
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

pattern LDJSONFile :: LogDriver
pattern LDJSONFile = LogDriver' "json-file"

pattern LDSyslog :: LogDriver
pattern LDSyslog = LogDriver' "syslog"

pattern LDJournald :: LogDriver
pattern LDJournald = LogDriver' "journald"

pattern LDGelf :: LogDriver
pattern LDGelf = LogDriver' "gelf"

pattern LDFluentd :: LogDriver
pattern LDFluentd = LogDriver' "fluentd"

pattern LDAWSlogs :: LogDriver
pattern LDAWSlogs = LogDriver' "awslogs"

pattern LDSplunk :: LogDriver
pattern LDSplunk = LogDriver' "splunk"

pattern LDAWSfirelens :: LogDriver
pattern LDAWSfirelens = LogDriver' "awsfirelens"

{-# COMPLETE
  LDJSONFile,
  LDSyslog,
  LDJournald,
  LDGelf,
  LDFluentd,
  LDAWSlogs,
  LDSplunk,
  LDAWSfirelens,
  LogDriver'
  #-}
