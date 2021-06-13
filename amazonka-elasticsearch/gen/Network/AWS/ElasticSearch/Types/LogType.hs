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
-- Module      : Network.AWS.ElasticSearch.Types.LogType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.LogType
  ( LogType
      ( ..,
        LogType_AUDIT_LOGS,
        LogType_ES_APPLICATION_LOGS,
        LogType_INDEX_SLOW_LOGS,
        LogType_SEARCH_SLOW_LOGS
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Type of Log File, it can be one of the following:
--
-- -   INDEX_SLOW_LOGS: Index slow logs contain insert requests that took
--     more time than configured index query log threshold to execute.
-- -   SEARCH_SLOW_LOGS: Search slow logs contain search queries that took
--     more time than configured search query log threshold to execute.
-- -   ES_APPLICATION_LOGS: Elasticsearch application logs contain
--     information about errors and warnings raised during the operation of
--     the service and can be useful for troubleshooting.
-- -   AUDIT_LOGS: Audit logs contain records of user requests for access
--     from the domain.
newtype LogType = LogType' {fromLogType :: Core.Text}
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

pattern LogType_AUDIT_LOGS :: LogType
pattern LogType_AUDIT_LOGS = LogType' "AUDIT_LOGS"

pattern LogType_ES_APPLICATION_LOGS :: LogType
pattern LogType_ES_APPLICATION_LOGS = LogType' "ES_APPLICATION_LOGS"

pattern LogType_INDEX_SLOW_LOGS :: LogType
pattern LogType_INDEX_SLOW_LOGS = LogType' "INDEX_SLOW_LOGS"

pattern LogType_SEARCH_SLOW_LOGS :: LogType
pattern LogType_SEARCH_SLOW_LOGS = LogType' "SEARCH_SLOW_LOGS"

{-# COMPLETE
  LogType_AUDIT_LOGS,
  LogType_ES_APPLICATION_LOGS,
  LogType_INDEX_SLOW_LOGS,
  LogType_SEARCH_SLOW_LOGS,
  LogType'
  #-}
