{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.LogType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.LogType
  ( LogType
      ( LogType',
        IndexSlowLogs,
        SearchSlowLogs,
        EsApplicationLogs,
        AuditLogs
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Type of Log File, it can be one of the following:
--
--     * INDEX_SLOW_LOGS: Index slow logs contain insert requests that took more time than configured index query log threshold to execute.
--
--     * SEARCH_SLOW_LOGS: Search slow logs contain search queries that took more time than configured search query log threshold to execute.
--
--     * ES_APPLICATION_LOGS: Elasticsearch application logs contain information about errors and warnings raised during the operation of the service and can be useful for troubleshooting.
--
--     * AUDIT_LOGS: Audit logs contain records of user requests for access from the domain.
newtype LogType = LogType' Lude.Text
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

pattern IndexSlowLogs :: LogType
pattern IndexSlowLogs = LogType' "INDEX_SLOW_LOGS"

pattern SearchSlowLogs :: LogType
pattern SearchSlowLogs = LogType' "SEARCH_SLOW_LOGS"

pattern EsApplicationLogs :: LogType
pattern EsApplicationLogs = LogType' "ES_APPLICATION_LOGS"

pattern AuditLogs :: LogType
pattern AuditLogs = LogType' "AUDIT_LOGS"

{-# COMPLETE
  IndexSlowLogs,
  SearchSlowLogs,
  EsApplicationLogs,
  AuditLogs,
  LogType'
  #-}
