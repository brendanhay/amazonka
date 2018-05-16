{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.Sum where

import Network.AWS.Prelude

data ConnectionPropertyKey
  = ConfigFiles
  | Host
  | InstanceId
  | JdbcConnectionURL
  | JdbcDriverClassName
  | JdbcDriverJARURI
  | JdbcEngine
  | JdbcEngineVersion
  | Password
  | Port
  | Username
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ConnectionPropertyKey where
    parser = takeLowerText >>= \case
        "config_files" -> pure ConfigFiles
        "host" -> pure Host
        "instance_id" -> pure InstanceId
        "jdbc_connection_url" -> pure JdbcConnectionURL
        "jdbc_driver_class_name" -> pure JdbcDriverClassName
        "jdbc_driver_jar_uri" -> pure JdbcDriverJARURI
        "jdbc_engine" -> pure JdbcEngine
        "jdbc_engine_version" -> pure JdbcEngineVersion
        "password" -> pure Password
        "port" -> pure Port
        "username" -> pure Username
        e -> fromTextError $ "Failure parsing ConnectionPropertyKey from value: '" <> e
           <> "'. Accepted values: config_files, host, instance_id, jdbc_connection_url, jdbc_driver_class_name, jdbc_driver_jar_uri, jdbc_engine, jdbc_engine_version, password, port, username"

instance ToText ConnectionPropertyKey where
    toText = \case
        ConfigFiles -> "CONFIG_FILES"
        Host -> "HOST"
        InstanceId -> "INSTANCE_ID"
        JdbcConnectionURL -> "JDBC_CONNECTION_URL"
        JdbcDriverClassName -> "JDBC_DRIVER_CLASS_NAME"
        JdbcDriverJARURI -> "JDBC_DRIVER_JAR_URI"
        JdbcEngine -> "JDBC_ENGINE"
        JdbcEngineVersion -> "JDBC_ENGINE_VERSION"
        Password -> "PASSWORD"
        Port -> "PORT"
        Username -> "USERNAME"

instance Hashable     ConnectionPropertyKey
instance NFData       ConnectionPropertyKey
instance ToByteString ConnectionPropertyKey
instance ToQuery      ConnectionPropertyKey
instance ToHeader     ConnectionPropertyKey

instance ToJSON ConnectionPropertyKey where
    toJSON = toJSONText

instance FromJSON ConnectionPropertyKey where
    parseJSON = parseJSONText "ConnectionPropertyKey"

data ConnectionType
  = Jdbc
  | Sftp
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ConnectionType where
    parser = takeLowerText >>= \case
        "jdbc" -> pure Jdbc
        "sftp" -> pure Sftp
        e -> fromTextError $ "Failure parsing ConnectionType from value: '" <> e
           <> "'. Accepted values: jdbc, sftp"

instance ToText ConnectionType where
    toText = \case
        Jdbc -> "JDBC"
        Sftp -> "SFTP"

instance Hashable     ConnectionType
instance NFData       ConnectionType
instance ToByteString ConnectionType
instance ToQuery      ConnectionType
instance ToHeader     ConnectionType

instance ToJSON ConnectionType where
    toJSON = toJSONText

instance FromJSON ConnectionType where
    parseJSON = parseJSONText "ConnectionType"

data CrawlerState
  = CSReady
  | CSRunning
  | CSStopping
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CrawlerState where
    parser = takeLowerText >>= \case
        "ready" -> pure CSReady
        "running" -> pure CSRunning
        "stopping" -> pure CSStopping
        e -> fromTextError $ "Failure parsing CrawlerState from value: '" <> e
           <> "'. Accepted values: ready, running, stopping"

instance ToText CrawlerState where
    toText = \case
        CSReady -> "READY"
        CSRunning -> "RUNNING"
        CSStopping -> "STOPPING"

instance Hashable     CrawlerState
instance NFData       CrawlerState
instance ToByteString CrawlerState
instance ToQuery      CrawlerState
instance ToHeader     CrawlerState

instance FromJSON CrawlerState where
    parseJSON = parseJSONText "CrawlerState"

data DeleteBehavior
  = DeleteFromDatabase
  | DeprecateInDatabase
  | Log
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DeleteBehavior where
    parser = takeLowerText >>= \case
        "delete_from_database" -> pure DeleteFromDatabase
        "deprecate_in_database" -> pure DeprecateInDatabase
        "log" -> pure Log
        e -> fromTextError $ "Failure parsing DeleteBehavior from value: '" <> e
           <> "'. Accepted values: delete_from_database, deprecate_in_database, log"

instance ToText DeleteBehavior where
    toText = \case
        DeleteFromDatabase -> "DELETE_FROM_DATABASE"
        DeprecateInDatabase -> "DEPRECATE_IN_DATABASE"
        Log -> "LOG"

instance Hashable     DeleteBehavior
instance NFData       DeleteBehavior
instance ToByteString DeleteBehavior
instance ToQuery      DeleteBehavior
instance ToHeader     DeleteBehavior

instance ToJSON DeleteBehavior where
    toJSON = toJSONText

instance FromJSON DeleteBehavior where
    parseJSON = parseJSONText "DeleteBehavior"

data JobRunState
  = Failed
  | Running
  | Starting
  | Stopped
  | Stopping
  | Succeeded
  | Timeout
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText JobRunState where
    parser = takeLowerText >>= \case
        "failed" -> pure Failed
        "running" -> pure Running
        "starting" -> pure Starting
        "stopped" -> pure Stopped
        "stopping" -> pure Stopping
        "succeeded" -> pure Succeeded
        "timeout" -> pure Timeout
        e -> fromTextError $ "Failure parsing JobRunState from value: '" <> e
           <> "'. Accepted values: failed, running, starting, stopped, stopping, succeeded, timeout"

instance ToText JobRunState where
    toText = \case
        Failed -> "FAILED"
        Running -> "RUNNING"
        Starting -> "STARTING"
        Stopped -> "STOPPED"
        Stopping -> "STOPPING"
        Succeeded -> "SUCCEEDED"
        Timeout -> "TIMEOUT"

instance Hashable     JobRunState
instance NFData       JobRunState
instance ToByteString JobRunState
instance ToQuery      JobRunState
instance ToHeader     JobRunState

instance ToJSON JobRunState where
    toJSON = toJSONText

instance FromJSON JobRunState where
    parseJSON = parseJSONText "JobRunState"

data Language
  = Python
  | Scala
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Language where
    parser = takeLowerText >>= \case
        "python" -> pure Python
        "scala" -> pure Scala
        e -> fromTextError $ "Failure parsing Language from value: '" <> e
           <> "'. Accepted values: python, scala"

instance ToText Language where
    toText = \case
        Python -> "PYTHON"
        Scala -> "SCALA"

instance Hashable     Language
instance NFData       Language
instance ToByteString Language
instance ToQuery      Language
instance ToHeader     Language

instance ToJSON Language where
    toJSON = toJSONText

data LastCrawlStatus
  = LCSCancelled
  | LCSFailed
  | LCSSucceeded
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LastCrawlStatus where
    parser = takeLowerText >>= \case
        "cancelled" -> pure LCSCancelled
        "failed" -> pure LCSFailed
        "succeeded" -> pure LCSSucceeded
        e -> fromTextError $ "Failure parsing LastCrawlStatus from value: '" <> e
           <> "'. Accepted values: cancelled, failed, succeeded"

instance ToText LastCrawlStatus where
    toText = \case
        LCSCancelled -> "CANCELLED"
        LCSFailed -> "FAILED"
        LCSSucceeded -> "SUCCEEDED"

instance Hashable     LastCrawlStatus
instance NFData       LastCrawlStatus
instance ToByteString LastCrawlStatus
instance ToQuery      LastCrawlStatus
instance ToHeader     LastCrawlStatus

instance FromJSON LastCrawlStatus where
    parseJSON = parseJSONText "LastCrawlStatus"

data Logical
  = And
  | Any
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Logical where
    parser = takeLowerText >>= \case
        "and" -> pure And
        "any" -> pure Any
        e -> fromTextError $ "Failure parsing Logical from value: '" <> e
           <> "'. Accepted values: and, any"

instance ToText Logical where
    toText = \case
        And -> "AND"
        Any -> "ANY"

instance Hashable     Logical
instance NFData       Logical
instance ToByteString Logical
instance ToQuery      Logical
instance ToHeader     Logical

instance ToJSON Logical where
    toJSON = toJSONText

instance FromJSON Logical where
    parseJSON = parseJSONText "Logical"

data LogicalOperator =
  Equals
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LogicalOperator where
    parser = takeLowerText >>= \case
        "equals" -> pure Equals
        e -> fromTextError $ "Failure parsing LogicalOperator from value: '" <> e
           <> "'. Accepted values: equals"

instance ToText LogicalOperator where
    toText = \case
        Equals -> "EQUALS"

instance Hashable     LogicalOperator
instance NFData       LogicalOperator
instance ToByteString LogicalOperator
instance ToQuery      LogicalOperator
instance ToHeader     LogicalOperator

instance ToJSON LogicalOperator where
    toJSON = toJSONText

instance FromJSON LogicalOperator where
    parseJSON = parseJSONText "LogicalOperator"

data PrincipalType
  = Group
  | Role
  | User
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PrincipalType where
    parser = takeLowerText >>= \case
        "group" -> pure Group
        "role" -> pure Role
        "user" -> pure User
        e -> fromTextError $ "Failure parsing PrincipalType from value: '" <> e
           <> "'. Accepted values: group, role, user"

instance ToText PrincipalType where
    toText = \case
        Group -> "GROUP"
        Role -> "ROLE"
        User -> "USER"

instance Hashable     PrincipalType
instance NFData       PrincipalType
instance ToByteString PrincipalType
instance ToQuery      PrincipalType
instance ToHeader     PrincipalType

instance ToJSON PrincipalType where
    toJSON = toJSONText

instance FromJSON PrincipalType where
    parseJSON = parseJSONText "PrincipalType"

data ResourceType
  = Archive
  | File
  | JAR
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ResourceType where
    parser = takeLowerText >>= \case
        "archive" -> pure Archive
        "file" -> pure File
        "jar" -> pure JAR
        e -> fromTextError $ "Failure parsing ResourceType from value: '" <> e
           <> "'. Accepted values: archive, file, jar"

instance ToText ResourceType where
    toText = \case
        Archive -> "ARCHIVE"
        File -> "FILE"
        JAR -> "JAR"

instance Hashable     ResourceType
instance NFData       ResourceType
instance ToByteString ResourceType
instance ToQuery      ResourceType
instance ToHeader     ResourceType

instance ToJSON ResourceType where
    toJSON = toJSONText

instance FromJSON ResourceType where
    parseJSON = parseJSONText "ResourceType"

data ScheduleState
  = NotScheduled
  | Scheduled
  | Transitioning
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ScheduleState where
    parser = takeLowerText >>= \case
        "not_scheduled" -> pure NotScheduled
        "scheduled" -> pure Scheduled
        "transitioning" -> pure Transitioning
        e -> fromTextError $ "Failure parsing ScheduleState from value: '" <> e
           <> "'. Accepted values: not_scheduled, scheduled, transitioning"

instance ToText ScheduleState where
    toText = \case
        NotScheduled -> "NOT_SCHEDULED"
        Scheduled -> "SCHEDULED"
        Transitioning -> "TRANSITIONING"

instance Hashable     ScheduleState
instance NFData       ScheduleState
instance ToByteString ScheduleState
instance ToQuery      ScheduleState
instance ToHeader     ScheduleState

instance FromJSON ScheduleState where
    parseJSON = parseJSONText "ScheduleState"

data TriggerState
  = Activated
  | Activating
  | Created
  | Creating
  | Deactivated
  | Deactivating
  | Deleting
  | Updating
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TriggerState where
    parser = takeLowerText >>= \case
        "activated" -> pure Activated
        "activating" -> pure Activating
        "created" -> pure Created
        "creating" -> pure Creating
        "deactivated" -> pure Deactivated
        "deactivating" -> pure Deactivating
        "deleting" -> pure Deleting
        "updating" -> pure Updating
        e -> fromTextError $ "Failure parsing TriggerState from value: '" <> e
           <> "'. Accepted values: activated, activating, created, creating, deactivated, deactivating, deleting, updating"

instance ToText TriggerState where
    toText = \case
        Activated -> "ACTIVATED"
        Activating -> "ACTIVATING"
        Created -> "CREATED"
        Creating -> "CREATING"
        Deactivated -> "DEACTIVATED"
        Deactivating -> "DEACTIVATING"
        Deleting -> "DELETING"
        Updating -> "UPDATING"

instance Hashable     TriggerState
instance NFData       TriggerState
instance ToByteString TriggerState
instance ToQuery      TriggerState
instance ToHeader     TriggerState

instance FromJSON TriggerState where
    parseJSON = parseJSONText "TriggerState"

data TriggerType
  = TTConditional
  | TTOnDemand
  | TTScheduled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TriggerType where
    parser = takeLowerText >>= \case
        "conditional" -> pure TTConditional
        "on_demand" -> pure TTOnDemand
        "scheduled" -> pure TTScheduled
        e -> fromTextError $ "Failure parsing TriggerType from value: '" <> e
           <> "'. Accepted values: conditional, on_demand, scheduled"

instance ToText TriggerType where
    toText = \case
        TTConditional -> "CONDITIONAL"
        TTOnDemand -> "ON_DEMAND"
        TTScheduled -> "SCHEDULED"

instance Hashable     TriggerType
instance NFData       TriggerType
instance ToByteString TriggerType
instance ToQuery      TriggerType
instance ToHeader     TriggerType

instance ToJSON TriggerType where
    toJSON = toJSONText

instance FromJSON TriggerType where
    parseJSON = parseJSONText "TriggerType"

data UpdateBehavior
  = UBLog
  | UBUpdateInDatabase
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText UpdateBehavior where
    parser = takeLowerText >>= \case
        "log" -> pure UBLog
        "update_in_database" -> pure UBUpdateInDatabase
        e -> fromTextError $ "Failure parsing UpdateBehavior from value: '" <> e
           <> "'. Accepted values: log, update_in_database"

instance ToText UpdateBehavior where
    toText = \case
        UBLog -> "LOG"
        UBUpdateInDatabase -> "UPDATE_IN_DATABASE"

instance Hashable     UpdateBehavior
instance NFData       UpdateBehavior
instance ToByteString UpdateBehavior
instance ToQuery      UpdateBehavior
instance ToHeader     UpdateBehavior

instance ToJSON UpdateBehavior where
    toJSON = toJSONText

instance FromJSON UpdateBehavior where
    parseJSON = parseJSONText "UpdateBehavior"
