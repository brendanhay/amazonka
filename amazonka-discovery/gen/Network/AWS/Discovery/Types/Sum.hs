{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Discovery.Types.Sum where

import Network.AWS.Prelude

data AgentStatus
  = Blacklisted
  | Healthy
  | Running
  | Shutdown
  | Unhealthy
  | Unknown
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AgentStatus where
    parser = takeLowerText >>= \case
        "blacklisted" -> pure Blacklisted
        "healthy" -> pure Healthy
        "running" -> pure Running
        "shutdown" -> pure Shutdown
        "unhealthy" -> pure Unhealthy
        "unknown" -> pure Unknown
        e -> fromTextError $ "Failure parsing AgentStatus from value: '" <> e
           <> "'. Accepted values: blacklisted, healthy, running, shutdown, unhealthy, unknown"

instance ToText AgentStatus where
    toText = \case
        Blacklisted -> "BLACKLISTED"
        Healthy -> "HEALTHY"
        Running -> "RUNNING"
        Shutdown -> "SHUTDOWN"
        Unhealthy -> "UNHEALTHY"
        Unknown -> "UNKNOWN"

instance Hashable     AgentStatus
instance NFData       AgentStatus
instance ToByteString AgentStatus
instance ToQuery      AgentStatus
instance ToHeader     AgentStatus

instance FromJSON AgentStatus where
    parseJSON = parseJSONText "AgentStatus"

data BatchDeleteImportDataErrorCode
  = InternalServerError
  | NotFound
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BatchDeleteImportDataErrorCode where
    parser = takeLowerText >>= \case
        "internal_server_error" -> pure InternalServerError
        "not_found" -> pure NotFound
        e -> fromTextError $ "Failure parsing BatchDeleteImportDataErrorCode from value: '" <> e
           <> "'. Accepted values: internal_server_error, not_found"

instance ToText BatchDeleteImportDataErrorCode where
    toText = \case
        InternalServerError -> "INTERNAL_SERVER_ERROR"
        NotFound -> "NOT_FOUND"

instance Hashable     BatchDeleteImportDataErrorCode
instance NFData       BatchDeleteImportDataErrorCode
instance ToByteString BatchDeleteImportDataErrorCode
instance ToQuery      BatchDeleteImportDataErrorCode
instance ToHeader     BatchDeleteImportDataErrorCode

instance FromJSON BatchDeleteImportDataErrorCode where
    parseJSON = parseJSONText "BatchDeleteImportDataErrorCode"

data ConfigurationItemType
  = Application
  | Connection
  | Process
  | Server
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ConfigurationItemType where
    parser = takeLowerText >>= \case
        "application" -> pure Application
        "connection" -> pure Connection
        "process" -> pure Process
        "server" -> pure Server
        e -> fromTextError $ "Failure parsing ConfigurationItemType from value: '" <> e
           <> "'. Accepted values: application, connection, process, server"

instance ToText ConfigurationItemType where
    toText = \case
        Application -> "APPLICATION"
        Connection -> "CONNECTION"
        Process -> "PROCESS"
        Server -> "SERVER"

instance Hashable     ConfigurationItemType
instance NFData       ConfigurationItemType
instance ToByteString ConfigurationItemType
instance ToQuery      ConfigurationItemType
instance ToHeader     ConfigurationItemType

instance ToJSON ConfigurationItemType where
    toJSON = toJSONText

instance FromJSON ConfigurationItemType where
    parseJSON = parseJSONText "ConfigurationItemType"

data ContinuousExportStatus
  = Active
  | Error'
  | Inactive
  | StartFailed
  | StartInProgress
  | StopFailed
  | StopInProgress
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ContinuousExportStatus where
    parser = takeLowerText >>= \case
        "active" -> pure Active
        "error" -> pure Error'
        "inactive" -> pure Inactive
        "start_failed" -> pure StartFailed
        "start_in_progress" -> pure StartInProgress
        "stop_failed" -> pure StopFailed
        "stop_in_progress" -> pure StopInProgress
        e -> fromTextError $ "Failure parsing ContinuousExportStatus from value: '" <> e
           <> "'. Accepted values: active, error, inactive, start_failed, start_in_progress, stop_failed, stop_in_progress"

instance ToText ContinuousExportStatus where
    toText = \case
        Active -> "ACTIVE"
        Error' -> "ERROR"
        Inactive -> "INACTIVE"
        StartFailed -> "START_FAILED"
        StartInProgress -> "START_IN_PROGRESS"
        StopFailed -> "STOP_FAILED"
        StopInProgress -> "STOP_IN_PROGRESS"

instance Hashable     ContinuousExportStatus
instance NFData       ContinuousExportStatus
instance ToByteString ContinuousExportStatus
instance ToQuery      ContinuousExportStatus
instance ToHeader     ContinuousExportStatus

instance FromJSON ContinuousExportStatus where
    parseJSON = parseJSONText "ContinuousExportStatus"

data DataSource =
  Agent
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DataSource where
    parser = takeLowerText >>= \case
        "agent" -> pure Agent
        e -> fromTextError $ "Failure parsing DataSource from value: '" <> e
           <> "'. Accepted values: agent"

instance ToText DataSource where
    toText = \case
        Agent -> "AGENT"

instance Hashable     DataSource
instance NFData       DataSource
instance ToByteString DataSource
instance ToQuery      DataSource
instance ToHeader     DataSource

instance FromJSON DataSource where
    parseJSON = parseJSONText "DataSource"

data ExportDataFormat
  = CSV
  | Graphml
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ExportDataFormat where
    parser = takeLowerText >>= \case
        "csv" -> pure CSV
        "graphml" -> pure Graphml
        e -> fromTextError $ "Failure parsing ExportDataFormat from value: '" <> e
           <> "'. Accepted values: csv, graphml"

instance ToText ExportDataFormat where
    toText = \case
        CSV -> "CSV"
        Graphml -> "GRAPHML"

instance Hashable     ExportDataFormat
instance NFData       ExportDataFormat
instance ToByteString ExportDataFormat
instance ToQuery      ExportDataFormat
instance ToHeader     ExportDataFormat

instance ToJSON ExportDataFormat where
    toJSON = toJSONText

data ExportStatus
  = Failed
  | InProgress
  | Succeeded
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ExportStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure Failed
        "in_progress" -> pure InProgress
        "succeeded" -> pure Succeeded
        e -> fromTextError $ "Failure parsing ExportStatus from value: '" <> e
           <> "'. Accepted values: failed, in_progress, succeeded"

instance ToText ExportStatus where
    toText = \case
        Failed -> "FAILED"
        InProgress -> "IN_PROGRESS"
        Succeeded -> "SUCCEEDED"

instance Hashable     ExportStatus
instance NFData       ExportStatus
instance ToByteString ExportStatus
instance ToQuery      ExportStatus
instance ToHeader     ExportStatus

instance FromJSON ExportStatus where
    parseJSON = parseJSONText "ExportStatus"

data ImportStatus
  = DeleteComplete
  | DeleteFailed
  | DeleteFailedLimitExceeded
  | DeleteInProgress
  | ImportComplete
  | ImportFailed
  | ImportFailedRecordLimitExceeded
  | ImportFailedServerLimitExceeded
  | ImportInProgress
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ImportStatus where
    parser = takeLowerText >>= \case
        "delete_complete" -> pure DeleteComplete
        "delete_failed" -> pure DeleteFailed
        "delete_failed_limit_exceeded" -> pure DeleteFailedLimitExceeded
        "delete_in_progress" -> pure DeleteInProgress
        "import_complete" -> pure ImportComplete
        "import_failed" -> pure ImportFailed
        "import_failed_record_limit_exceeded" -> pure ImportFailedRecordLimitExceeded
        "import_failed_server_limit_exceeded" -> pure ImportFailedServerLimitExceeded
        "import_in_progress" -> pure ImportInProgress
        e -> fromTextError $ "Failure parsing ImportStatus from value: '" <> e
           <> "'. Accepted values: delete_complete, delete_failed, delete_failed_limit_exceeded, delete_in_progress, import_complete, import_failed, import_failed_record_limit_exceeded, import_failed_server_limit_exceeded, import_in_progress"

instance ToText ImportStatus where
    toText = \case
        DeleteComplete -> "DELETE_COMPLETE"
        DeleteFailed -> "DELETE_FAILED"
        DeleteFailedLimitExceeded -> "DELETE_FAILED_LIMIT_EXCEEDED"
        DeleteInProgress -> "DELETE_IN_PROGRESS"
        ImportComplete -> "IMPORT_COMPLETE"
        ImportFailed -> "IMPORT_FAILED"
        ImportFailedRecordLimitExceeded -> "IMPORT_FAILED_RECORD_LIMIT_EXCEEDED"
        ImportFailedServerLimitExceeded -> "IMPORT_FAILED_SERVER_LIMIT_EXCEEDED"
        ImportInProgress -> "IMPORT_IN_PROGRESS"

instance Hashable     ImportStatus
instance NFData       ImportStatus
instance ToByteString ImportStatus
instance ToQuery      ImportStatus
instance ToHeader     ImportStatus

instance FromJSON ImportStatus where
    parseJSON = parseJSONText "ImportStatus"

data ImportTaskFilterName
  = ImportTaskId
  | Name
  | Status
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ImportTaskFilterName where
    parser = takeLowerText >>= \case
        "import_task_id" -> pure ImportTaskId
        "name" -> pure Name
        "status" -> pure Status
        e -> fromTextError $ "Failure parsing ImportTaskFilterName from value: '" <> e
           <> "'. Accepted values: import_task_id, name, status"

instance ToText ImportTaskFilterName where
    toText = \case
        ImportTaskId -> "IMPORT_TASK_ID"
        Name -> "NAME"
        Status -> "STATUS"

instance Hashable     ImportTaskFilterName
instance NFData       ImportTaskFilterName
instance ToByteString ImportTaskFilterName
instance ToQuery      ImportTaskFilterName
instance ToHeader     ImportTaskFilterName

instance ToJSON ImportTaskFilterName where
    toJSON = toJSONText

data OrderString
  = Asc
  | Desc
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OrderString where
    parser = takeLowerText >>= \case
        "asc" -> pure Asc
        "desc" -> pure Desc
        e -> fromTextError $ "Failure parsing OrderString from value: '" <> e
           <> "'. Accepted values: asc, desc"

instance ToText OrderString where
    toText = \case
        Asc -> "ASC"
        Desc -> "DESC"

instance Hashable     OrderString
instance NFData       OrderString
instance ToByteString OrderString
instance ToQuery      OrderString
instance ToHeader     OrderString

instance ToJSON OrderString where
    toJSON = toJSONText
