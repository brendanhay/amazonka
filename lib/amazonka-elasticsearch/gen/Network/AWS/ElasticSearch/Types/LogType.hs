{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.LogType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.LogType
  ( LogType
    ( LogType'
    , LogTypeIndexSlowLogs
    , LogTypeSearchSlowLogs
    , LogTypeEsApplicationLogs
    , LogTypeAuditLogs
    , fromLogType
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Type of Log File, it can be one of the following: 
--
--     * INDEX_SLOW_LOGS: Index slow logs contain insert requests that took more time than configured index query log threshold to execute.
--
--     * SEARCH_SLOW_LOGS: Search slow logs contain search queries that took more time than configured search query log threshold to execute.
--
--     * ES_APPLICATION_LOGS: Elasticsearch application logs contain information about errors and warnings raised during the operation of the service and can be useful for troubleshooting.
--
--     * AUDIT_LOGS: Audit logs contain records of user requests for access from the domain.
--
--
newtype LogType = LogType'{fromLogType :: Core.Text}
                    deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                    Core.Generic)
                    deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                      Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON, Core.FromJSON,
                                      Core.ToXML, Core.FromXML, Core.ToText, Core.FromText,
                                      Core.ToByteString, Core.ToQuery, Core.ToHeader)

pattern LogTypeIndexSlowLogs :: LogType
pattern LogTypeIndexSlowLogs = LogType' "INDEX_SLOW_LOGS"

pattern LogTypeSearchSlowLogs :: LogType
pattern LogTypeSearchSlowLogs = LogType' "SEARCH_SLOW_LOGS"

pattern LogTypeEsApplicationLogs :: LogType
pattern LogTypeEsApplicationLogs = LogType' "ES_APPLICATION_LOGS"

pattern LogTypeAuditLogs :: LogType
pattern LogTypeAuditLogs = LogType' "AUDIT_LOGS"

{-# COMPLETE 
  LogTypeIndexSlowLogs,

  LogTypeSearchSlowLogs,

  LogTypeEsApplicationLogs,

  LogTypeAuditLogs,
  LogType'
  #-}
