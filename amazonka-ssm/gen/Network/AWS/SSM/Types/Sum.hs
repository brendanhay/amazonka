{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.Sum
-- Copyright   : (c) 2013-2016 Brendan Hay
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
           <> "'. Accepted values: instanceid, name"

instance ToText AssociationFilterKey where
    toText = \case
        AFKInstanceId -> "InstanceId"
        AFKName -> "Name"

instance Hashable     AssociationFilterKey
instance NFData       AssociationFilterKey
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
           <> "'. Accepted values: failed, pending, success"

instance ToText AssociationStatusName where
    toText = \case
        ASNFailed -> "Failed"
        ASNPending -> "Pending"
        ASNSuccess -> "Success"

instance Hashable     AssociationStatusName
instance NFData       AssociationStatusName
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
           <> "'. Accepted values: invokedafter, invokedbefore, status"

instance ToText CommandFilterKey where
    toText = \case
        CommandInvokedAfter -> "InvokedAfter"
        CommandInvokedBefore -> "InvokedBefore"
        CommandStatus -> "Status"

instance Hashable     CommandFilterKey
instance NFData       CommandFilterKey
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
           <> "'. Accepted values: cancelled, cancelling, failed, inprogress, pending, success, timedout"

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
instance NFData       CommandInvocationStatus
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
           <> "'. Accepted values: cancelled, failed, inprogress, pending, success, timedout"

instance ToText CommandPluginStatus where
    toText = \case
        CPSCancelled -> "Cancelled"
        CPSFailed -> "Failed"
        CPSInProgress -> "InProgress"
        CPSPending -> "Pending"
        CPSSuccess -> "Success"
        CPSTimedOut -> "TimedOut"

instance Hashable     CommandPluginStatus
instance NFData       CommandPluginStatus
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
           <> "'. Accepted values: cancelled, cancelling, failed, inprogress, pending, success, timedout"

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
instance NFData       CommandStatus
instance ToByteString CommandStatus
instance ToQuery      CommandStatus
instance ToHeader     CommandStatus

instance FromJSON CommandStatus where
    parseJSON = parseJSONText "CommandStatus"

data DescribeActivationsFilterKeys
    = ActivationIds
    | DefaultInstanceName
    | IAMRole
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText DescribeActivationsFilterKeys where
    parser = takeLowerText >>= \case
        "activationids" -> pure ActivationIds
        "defaultinstancename" -> pure DefaultInstanceName
        "iamrole" -> pure IAMRole
        e -> fromTextError $ "Failure parsing DescribeActivationsFilterKeys from value: '" <> e
           <> "'. Accepted values: activationids, defaultinstancename, iamrole"

instance ToText DescribeActivationsFilterKeys where
    toText = \case
        ActivationIds -> "ActivationIds"
        DefaultInstanceName -> "DefaultInstanceName"
        IAMRole -> "IamRole"

instance Hashable     DescribeActivationsFilterKeys
instance NFData       DescribeActivationsFilterKeys
instance ToByteString DescribeActivationsFilterKeys
instance ToQuery      DescribeActivationsFilterKeys
instance ToHeader     DescribeActivationsFilterKeys

instance ToJSON DescribeActivationsFilterKeys where
    toJSON = toJSONText

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
           <> "'. Accepted values: name, owner, platformtypes"

instance ToText DocumentFilterKey where
    toText = \case
        Name -> "Name"
        Owner -> "Owner"
        PlatformTypes -> "PlatformTypes"

instance Hashable     DocumentFilterKey
instance NFData       DocumentFilterKey
instance ToByteString DocumentFilterKey
instance ToQuery      DocumentFilterKey
instance ToHeader     DocumentFilterKey

instance ToJSON DocumentFilterKey where
    toJSON = toJSONText

data DocumentHashType
    = SHA1
    | SHA256
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText DocumentHashType where
    parser = takeLowerText >>= \case
        "sha1" -> pure SHA1
        "sha256" -> pure SHA256
        e -> fromTextError $ "Failure parsing DocumentHashType from value: '" <> e
           <> "'. Accepted values: sha1, sha256"

instance ToText DocumentHashType where
    toText = \case
        SHA1 -> "Sha1"
        SHA256 -> "Sha256"

instance Hashable     DocumentHashType
instance NFData       DocumentHashType
instance ToByteString DocumentHashType
instance ToQuery      DocumentHashType
instance ToHeader     DocumentHashType

instance ToJSON DocumentHashType where
    toJSON = toJSONText

instance FromJSON DocumentHashType where
    parseJSON = parseJSONText "DocumentHashType"

data DocumentParameterType
    = String
    | StringList
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText DocumentParameterType where
    parser = takeLowerText >>= \case
        "string" -> pure String
        "stringlist" -> pure StringList
        e -> fromTextError $ "Failure parsing DocumentParameterType from value: '" <> e
           <> "'. Accepted values: string, stringlist"

instance ToText DocumentParameterType where
    toText = \case
        String -> "String"
        StringList -> "StringList"

instance Hashable     DocumentParameterType
instance NFData       DocumentParameterType
instance ToByteString DocumentParameterType
instance ToQuery      DocumentParameterType
instance ToHeader     DocumentParameterType

instance FromJSON DocumentParameterType where
    parseJSON = parseJSONText "DocumentParameterType"

data DocumentPermissionType =
    Share
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText DocumentPermissionType where
    parser = takeLowerText >>= \case
        "share" -> pure Share
        e -> fromTextError $ "Failure parsing DocumentPermissionType from value: '" <> e
           <> "'. Accepted values: share"

instance ToText DocumentPermissionType where
    toText = \case
        Share -> "Share"

instance Hashable     DocumentPermissionType
instance NFData       DocumentPermissionType
instance ToByteString DocumentPermissionType
instance ToQuery      DocumentPermissionType
instance ToHeader     DocumentPermissionType

instance ToJSON DocumentPermissionType where
    toJSON = toJSONText

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
           <> "'. Accepted values: active, creating, deleting"

instance ToText DocumentStatus where
    toText = \case
        Active -> "Active"
        Creating -> "Creating"
        Deleting -> "Deleting"

instance Hashable     DocumentStatus
instance NFData       DocumentStatus
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
           <> "'. Accepted values: client, server, unknown"

instance ToText Fault where
    toText = \case
        Client -> "Client"
        Server -> "Server"
        Unknown -> "Unknown"

instance Hashable     Fault
instance NFData       Fault
instance ToByteString Fault
instance ToQuery      Fault
instance ToHeader     Fault

instance FromJSON Fault where
    parseJSON = parseJSONText "Fault"

data InstanceInformationFilterKey
    = IIFKActivationIds
    | IIFKAgentVersion
    | IIFKIAMRole
    | IIFKInstanceIds
    | IIFKPingStatus
    | IIFKPlatformTypes
    | IIFKResourceType
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText InstanceInformationFilterKey where
    parser = takeLowerText >>= \case
        "activationids" -> pure IIFKActivationIds
        "agentversion" -> pure IIFKAgentVersion
        "iamrole" -> pure IIFKIAMRole
        "instanceids" -> pure IIFKInstanceIds
        "pingstatus" -> pure IIFKPingStatus
        "platformtypes" -> pure IIFKPlatformTypes
        "resourcetype" -> pure IIFKResourceType
        e -> fromTextError $ "Failure parsing InstanceInformationFilterKey from value: '" <> e
           <> "'. Accepted values: activationids, agentversion, iamrole, instanceids, pingstatus, platformtypes, resourcetype"

instance ToText InstanceInformationFilterKey where
    toText = \case
        IIFKActivationIds -> "ActivationIds"
        IIFKAgentVersion -> "AgentVersion"
        IIFKIAMRole -> "IamRole"
        IIFKInstanceIds -> "InstanceIds"
        IIFKPingStatus -> "PingStatus"
        IIFKPlatformTypes -> "PlatformTypes"
        IIFKResourceType -> "ResourceType"

instance Hashable     InstanceInformationFilterKey
instance NFData       InstanceInformationFilterKey
instance ToByteString InstanceInformationFilterKey
instance ToQuery      InstanceInformationFilterKey
instance ToHeader     InstanceInformationFilterKey

instance ToJSON InstanceInformationFilterKey where
    toJSON = toJSONText

data NotificationEvent
    = NEAll
    | NECancelled
    | NEFailed
    | NEInProgress
    | NESuccess
    | NETimedOut
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText NotificationEvent where
    parser = takeLowerText >>= \case
        "all" -> pure NEAll
        "cancelled" -> pure NECancelled
        "failed" -> pure NEFailed
        "inprogress" -> pure NEInProgress
        "success" -> pure NESuccess
        "timedout" -> pure NETimedOut
        e -> fromTextError $ "Failure parsing NotificationEvent from value: '" <> e
           <> "'. Accepted values: all, cancelled, failed, inprogress, success, timedout"

instance ToText NotificationEvent where
    toText = \case
        NEAll -> "All"
        NECancelled -> "Cancelled"
        NEFailed -> "Failed"
        NEInProgress -> "InProgress"
        NESuccess -> "Success"
        NETimedOut -> "TimedOut"

instance Hashable     NotificationEvent
instance NFData       NotificationEvent
instance ToByteString NotificationEvent
instance ToQuery      NotificationEvent
instance ToHeader     NotificationEvent

instance ToJSON NotificationEvent where
    toJSON = toJSONText

instance FromJSON NotificationEvent where
    parseJSON = parseJSONText "NotificationEvent"

data NotificationType
    = Command
    | Invocation
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText NotificationType where
    parser = takeLowerText >>= \case
        "command" -> pure Command
        "invocation" -> pure Invocation
        e -> fromTextError $ "Failure parsing NotificationType from value: '" <> e
           <> "'. Accepted values: command, invocation"

instance ToText NotificationType where
    toText = \case
        Command -> "Command"
        Invocation -> "Invocation"

instance Hashable     NotificationType
instance NFData       NotificationType
instance ToByteString NotificationType
instance ToQuery      NotificationType
instance ToHeader     NotificationType

instance ToJSON NotificationType where
    toJSON = toJSONText

instance FromJSON NotificationType where
    parseJSON = parseJSONText "NotificationType"

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
           <> "'. Accepted values: connectionlost, inactive, online"

instance ToText PingStatus where
    toText = \case
        ConnectionLost -> "ConnectionLost"
        Inactive -> "Inactive"
        Online -> "Online"

instance Hashable     PingStatus
instance NFData       PingStatus
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
           <> "'. Accepted values: linux, windows"

instance ToText PlatformType where
    toText = \case
        Linux -> "Linux"
        Windows -> "Windows"

instance Hashable     PlatformType
instance NFData       PlatformType
instance ToByteString PlatformType
instance ToQuery      PlatformType
instance ToHeader     PlatformType

instance FromJSON PlatformType where
    parseJSON = parseJSONText "PlatformType"

data ResourceType
    = Document
    | EC2Instance
    | ManagedInstance
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText ResourceType where
    parser = takeLowerText >>= \case
        "document" -> pure Document
        "ec2instance" -> pure EC2Instance
        "managedinstance" -> pure ManagedInstance
        e -> fromTextError $ "Failure parsing ResourceType from value: '" <> e
           <> "'. Accepted values: document, ec2instance, managedinstance"

instance ToText ResourceType where
    toText = \case
        Document -> "Document"
        EC2Instance -> "EC2Instance"
        ManagedInstance -> "ManagedInstance"

instance Hashable     ResourceType
instance NFData       ResourceType
instance ToByteString ResourceType
instance ToQuery      ResourceType
instance ToHeader     ResourceType

instance FromJSON ResourceType where
    parseJSON = parseJSONText "ResourceType"

data ResourceTypeForTagging =
    RTFTManagedInstance
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText ResourceTypeForTagging where
    parser = takeLowerText >>= \case
        "managedinstance" -> pure RTFTManagedInstance
        e -> fromTextError $ "Failure parsing ResourceTypeForTagging from value: '" <> e
           <> "'. Accepted values: managedinstance"

instance ToText ResourceTypeForTagging where
    toText = \case
        RTFTManagedInstance -> "ManagedInstance"

instance Hashable     ResourceTypeForTagging
instance NFData       ResourceTypeForTagging
instance ToByteString ResourceTypeForTagging
instance ToQuery      ResourceTypeForTagging
instance ToHeader     ResourceTypeForTagging

instance ToJSON ResourceTypeForTagging where
    toJSON = toJSONText
