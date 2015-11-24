{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.Sum
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.Sum where

import           Network.AWS.Prelude

data AssociationFilterKey
    = AFKInstanceId
    | AFKName
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText AssociationFilterKey where
    parser = takeLowerText >>= \case
        "instanceid" -> pure AFKInstanceId
        "name" -> pure AFKName
        e -> fromTextError $ "Failure parsing AssociationFilterKey from value: '" <> e
           <> "'. Accepted values: InstanceId, Name"

instance ToText AssociationFilterKey where
    toText = \case
        AFKInstanceId -> "InstanceId"
        AFKName -> "Name"

instance Hashable     AssociationFilterKey
instance ToByteString AssociationFilterKey
instance ToQuery      AssociationFilterKey
instance ToHeader     AssociationFilterKey

instance ToJSON AssociationFilterKey where
    toJSON = toJSONText

data AssociationStatusName
    = ASNFailed
    | ASNPending
    | ASNSuccess
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText AssociationStatusName where
    parser = takeLowerText >>= \case
        "failed" -> pure ASNFailed
        "pending" -> pure ASNPending
        "success" -> pure ASNSuccess
        e -> fromTextError $ "Failure parsing AssociationStatusName from value: '" <> e
           <> "'. Accepted values: Failed, Pending, Success"

instance ToText AssociationStatusName where
    toText = \case
        ASNFailed -> "Failed"
        ASNPending -> "Pending"
        ASNSuccess -> "Success"

instance Hashable     AssociationStatusName
instance ToByteString AssociationStatusName
instance ToQuery      AssociationStatusName
instance ToHeader     AssociationStatusName

instance ToJSON AssociationStatusName where
    toJSON = toJSONText

instance FromJSON AssociationStatusName where
    parseJSON = parseJSONText "AssociationStatusName"

data CommandFilterKey
    = CommandInvokedAfter
    | CommandInvokedBefore
    | CommandStatus
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText CommandFilterKey where
    parser = takeLowerText >>= \case
        "invokedafter" -> pure CommandInvokedAfter
        "invokedbefore" -> pure CommandInvokedBefore
        "status" -> pure CommandStatus
        e -> fromTextError $ "Failure parsing CommandFilterKey from value: '" <> e
           <> "'. Accepted values: InvokedAfter, InvokedBefore, Status"

instance ToText CommandFilterKey where
    toText = \case
        CommandInvokedAfter -> "InvokedAfter"
        CommandInvokedBefore -> "InvokedBefore"
        CommandStatus -> "Status"

instance Hashable     CommandFilterKey
instance ToByteString CommandFilterKey
instance ToQuery      CommandFilterKey
instance ToHeader     CommandFilterKey

instance ToJSON CommandFilterKey where
    toJSON = toJSONText

data CommandInvocationStatus
    = CISCancelled
    | CISCancelling
    | CISFailed
    | CISInProgress
    | CISPending
    | CISSuccess
    | CISTimedOut
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText CommandInvocationStatus where
    parser = takeLowerText >>= \case
        "cancelled" -> pure CISCancelled
        "cancelling" -> pure CISCancelling
        "failed" -> pure CISFailed
        "inprogress" -> pure CISInProgress
        "pending" -> pure CISPending
        "success" -> pure CISSuccess
        "timedout" -> pure CISTimedOut
        e -> fromTextError $ "Failure parsing CommandInvocationStatus from value: '" <> e
           <> "'. Accepted values: Cancelled, Cancelling, Failed, InProgress, Pending, Success, TimedOut"

instance ToText CommandInvocationStatus where
    toText = \case
        CISCancelled -> "Cancelled"
        CISCancelling -> "Cancelling"
        CISFailed -> "Failed"
        CISInProgress -> "InProgress"
        CISPending -> "Pending"
        CISSuccess -> "Success"
        CISTimedOut -> "TimedOut"

instance Hashable     CommandInvocationStatus
instance ToByteString CommandInvocationStatus
instance ToQuery      CommandInvocationStatus
instance ToHeader     CommandInvocationStatus

instance FromJSON CommandInvocationStatus where
    parseJSON = parseJSONText "CommandInvocationStatus"

data CommandPluginStatus
    = CPSCancelled
    | CPSFailed
    | CPSInProgress
    | CPSPending
    | CPSSuccess
    | CPSTimedOut
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText CommandPluginStatus where
    parser = takeLowerText >>= \case
        "cancelled" -> pure CPSCancelled
        "failed" -> pure CPSFailed
        "inprogress" -> pure CPSInProgress
        "pending" -> pure CPSPending
        "success" -> pure CPSSuccess
        "timedout" -> pure CPSTimedOut
        e -> fromTextError $ "Failure parsing CommandPluginStatus from value: '" <> e
           <> "'. Accepted values: Cancelled, Failed, InProgress, Pending, Success, TimedOut"

instance ToText CommandPluginStatus where
    toText = \case
        CPSCancelled -> "Cancelled"
        CPSFailed -> "Failed"
        CPSInProgress -> "InProgress"
        CPSPending -> "Pending"
        CPSSuccess -> "Success"
        CPSTimedOut -> "TimedOut"

instance Hashable     CommandPluginStatus
instance ToByteString CommandPluginStatus
instance ToQuery      CommandPluginStatus
instance ToHeader     CommandPluginStatus

instance FromJSON CommandPluginStatus where
    parseJSON = parseJSONText "CommandPluginStatus"

data CommandStatus
    = Cancelled
    | Cancelling
    | Failed
    | InProgress
    | Pending
    | Success
    | TimedOut
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText CommandStatus where
    parser = takeLowerText >>= \case
        "cancelled" -> pure Cancelled
        "cancelling" -> pure Cancelling
        "failed" -> pure Failed
        "inprogress" -> pure InProgress
        "pending" -> pure Pending
        "success" -> pure Success
        "timedout" -> pure TimedOut
        e -> fromTextError $ "Failure parsing CommandStatus from value: '" <> e
           <> "'. Accepted values: Cancelled, Cancelling, Failed, InProgress, Pending, Success, TimedOut"

instance ToText CommandStatus where
    toText = \case
        Cancelled -> "Cancelled"
        Cancelling -> "Cancelling"
        Failed -> "Failed"
        InProgress -> "InProgress"
        Pending -> "Pending"
        Success -> "Success"
        TimedOut -> "TimedOut"

instance Hashable     CommandStatus
instance ToByteString CommandStatus
instance ToQuery      CommandStatus
instance ToHeader     CommandStatus

instance FromJSON CommandStatus where
    parseJSON = parseJSONText "CommandStatus"

data DocumentFilterKey
    = Name
    | Owner
    | PlatformTypes
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText DocumentFilterKey where
    parser = takeLowerText >>= \case
        "name" -> pure Name
        "owner" -> pure Owner
        "platformtypes" -> pure PlatformTypes
        e -> fromTextError $ "Failure parsing DocumentFilterKey from value: '" <> e
           <> "'. Accepted values: Name, Owner, PlatformTypes"

instance ToText DocumentFilterKey where
    toText = \case
        Name -> "Name"
        Owner -> "Owner"
        PlatformTypes -> "PlatformTypes"

instance Hashable     DocumentFilterKey
instance ToByteString DocumentFilterKey
instance ToQuery      DocumentFilterKey
instance ToHeader     DocumentFilterKey

instance ToJSON DocumentFilterKey where
    toJSON = toJSONText

data DocumentParameterType
    = String
    | StringList
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText DocumentParameterType where
    parser = takeLowerText >>= \case
        "string" -> pure String
        "stringlist" -> pure StringList
        e -> fromTextError $ "Failure parsing DocumentParameterType from value: '" <> e
           <> "'. Accepted values: String, StringList"

instance ToText DocumentParameterType where
    toText = \case
        String -> "String"
        StringList -> "StringList"

instance Hashable     DocumentParameterType
instance ToByteString DocumentParameterType
instance ToQuery      DocumentParameterType
instance ToHeader     DocumentParameterType

instance FromJSON DocumentParameterType where
    parseJSON = parseJSONText "DocumentParameterType"

data DocumentStatus
    = Active
    | Creating
    | Deleting
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText DocumentStatus where
    parser = takeLowerText >>= \case
        "active" -> pure Active
        "creating" -> pure Creating
        "deleting" -> pure Deleting
        e -> fromTextError $ "Failure parsing DocumentStatus from value: '" <> e
           <> "'. Accepted values: Active, Creating, Deleting"

instance ToText DocumentStatus where
    toText = \case
        Active -> "Active"
        Creating -> "Creating"
        Deleting -> "Deleting"

instance Hashable     DocumentStatus
instance ToByteString DocumentStatus
instance ToQuery      DocumentStatus
instance ToHeader     DocumentStatus

instance FromJSON DocumentStatus where
    parseJSON = parseJSONText "DocumentStatus"

data Fault
    = Client
    | Server
    | Unknown
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText Fault where
    parser = takeLowerText >>= \case
        "client" -> pure Client
        "server" -> pure Server
        "unknown" -> pure Unknown
        e -> fromTextError $ "Failure parsing Fault from value: '" <> e
           <> "'. Accepted values: Client, Server, Unknown"

instance ToText Fault where
    toText = \case
        Client -> "Client"
        Server -> "Server"
        Unknown -> "Unknown"

instance Hashable     Fault
instance ToByteString Fault
instance ToQuery      Fault
instance ToHeader     Fault

instance FromJSON Fault where
    parseJSON = parseJSONText "Fault"

data InstanceInformationFilterKey
    = IIFKAgentVersion
    | IIFKInstanceIds
    | IIFKPingStatus
    | IIFKPlatformTypes
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText InstanceInformationFilterKey where
    parser = takeLowerText >>= \case
        "agentversion" -> pure IIFKAgentVersion
        "instanceids" -> pure IIFKInstanceIds
        "pingstatus" -> pure IIFKPingStatus
        "platformtypes" -> pure IIFKPlatformTypes
        e -> fromTextError $ "Failure parsing InstanceInformationFilterKey from value: '" <> e
           <> "'. Accepted values: AgentVersion, InstanceIds, PingStatus, PlatformTypes"

instance ToText InstanceInformationFilterKey where
    toText = \case
        IIFKAgentVersion -> "AgentVersion"
        IIFKInstanceIds -> "InstanceIds"
        IIFKPingStatus -> "PingStatus"
        IIFKPlatformTypes -> "PlatformTypes"

instance Hashable     InstanceInformationFilterKey
instance ToByteString InstanceInformationFilterKey
instance ToQuery      InstanceInformationFilterKey
instance ToHeader     InstanceInformationFilterKey

instance ToJSON InstanceInformationFilterKey where
    toJSON = toJSONText

data PingStatus
    = ConnectionLost
    | Inactive
    | Online
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText PingStatus where
    parser = takeLowerText >>= \case
        "connectionlost" -> pure ConnectionLost
        "inactive" -> pure Inactive
        "online" -> pure Online
        e -> fromTextError $ "Failure parsing PingStatus from value: '" <> e
           <> "'. Accepted values: ConnectionLost, Inactive, Online"

instance ToText PingStatus where
    toText = \case
        ConnectionLost -> "ConnectionLost"
        Inactive -> "Inactive"
        Online -> "Online"

instance Hashable     PingStatus
instance ToByteString PingStatus
instance ToQuery      PingStatus
instance ToHeader     PingStatus

instance FromJSON PingStatus where
    parseJSON = parseJSONText "PingStatus"

data PlatformType
    = Linux
    | Windows
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText PlatformType where
    parser = takeLowerText >>= \case
        "linux" -> pure Linux
        "windows" -> pure Windows
        e -> fromTextError $ "Failure parsing PlatformType from value: '" <> e
           <> "'. Accepted values: Linux, Windows"

instance ToText PlatformType where
    toText = \case
        Linux -> "Linux"
        Windows -> "Windows"

instance Hashable     PlatformType
instance ToByteString PlatformType
instance ToQuery      PlatformType
instance ToHeader     PlatformType

instance FromJSON PlatformType where
    parseJSON = parseJSONText "PlatformType"
