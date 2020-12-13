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
        JSONFile,
        Syslog,
        Journald,
        Gelf,
        Fluentd,
        AWSlogs,
        Splunk
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

pattern JSONFile :: LogDriver
pattern JSONFile = LogDriver' "json-file"

pattern Syslog :: LogDriver
pattern Syslog = LogDriver' "syslog"

pattern Journald :: LogDriver
pattern Journald = LogDriver' "journald"

pattern Gelf :: LogDriver
pattern Gelf = LogDriver' "gelf"

pattern Fluentd :: LogDriver
pattern Fluentd = LogDriver' "fluentd"

pattern AWSlogs :: LogDriver
pattern AWSlogs = LogDriver' "awslogs"

pattern Splunk :: LogDriver
pattern Splunk = LogDriver' "splunk"

{-# COMPLETE
  JSONFile,
  Syslog,
  Journald,
  Gelf,
  Fluentd,
  AWSlogs,
  Splunk,
  LogDriver'
  #-}
