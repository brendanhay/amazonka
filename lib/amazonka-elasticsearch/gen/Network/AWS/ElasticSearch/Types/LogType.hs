{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.LogType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.LogType where

import Network.AWS.Prelude

-- | Type of Log File, it can be one of the following:     * INDEX_SLOW_LOGS: Index slow logs contain insert requests that took more time than configured index query log threshold to execute.    * SEARCH_SLOW_LOGS: Search slow logs contain search queries that took more time than configured search query log threshold to execute.    * ES_APPLICATION_LOGS: Elasticsearch application logs contain information about errors and warnings raised during the operation of the service and can be useful for troubleshooting.    * AUDIT_LOGS: Audit logs contain records of user requests for access from the domain.
data LogType
  = AuditLogs
  | EsApplicationLogs
  | IndexSlowLogs
  | SearchSlowLogs
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText LogType where
  parser =
    takeLowerText >>= \case
      "audit_logs" -> pure AuditLogs
      "es_application_logs" -> pure EsApplicationLogs
      "index_slow_logs" -> pure IndexSlowLogs
      "search_slow_logs" -> pure SearchSlowLogs
      e ->
        fromTextError $
          "Failure parsing LogType from value: '" <> e
            <> "'. Accepted values: audit_logs, es_application_logs, index_slow_logs, search_slow_logs"

instance ToText LogType where
  toText = \case
    AuditLogs -> "AUDIT_LOGS"
    EsApplicationLogs -> "ES_APPLICATION_LOGS"
    IndexSlowLogs -> "INDEX_SLOW_LOGS"
    SearchSlowLogs -> "SEARCH_SLOW_LOGS"

instance Hashable LogType

instance NFData LogType

instance ToByteString LogType

instance ToQuery LogType

instance ToHeader LogType

instance ToJSON LogType where
  toJSON = toJSONText

instance FromJSON LogType where
  parseJSON = parseJSONText "LogType"
