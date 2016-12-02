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
    = AFKAssociationId
    | AFKAssociationStatusName
    | AFKInstanceId
    | AFKLastExecutedAfter
    | AFKLastExecutedBefore
    | AFKName
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText AssociationFilterKey where
    parser = takeLowerText >>= \case
        "associationid" -> pure AFKAssociationId
        "associationstatusname" -> pure AFKAssociationStatusName
        "instanceid" -> pure AFKInstanceId
        "lastexecutedafter" -> pure AFKLastExecutedAfter
        "lastexecutedbefore" -> pure AFKLastExecutedBefore
        "name" -> pure AFKName
        e -> fromTextError $ "Failure parsing AssociationFilterKey from value: '" <> e
           <> "'. Accepted values: associationid, associationstatusname, instanceid, lastexecutedafter, lastexecutedbefore, name"

instance ToText AssociationFilterKey where
    toText = \case
        AFKAssociationId -> "AssociationId"
        AFKAssociationStatusName -> "AssociationStatusName"
        AFKInstanceId -> "InstanceId"
        AFKLastExecutedAfter -> "LastExecutedAfter"
        AFKLastExecutedBefore -> "LastExecutedBefore"
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

data AutomationExecutionFilterKey
    = DocumentNamePrefix
    | ExecutionStatus
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText AutomationExecutionFilterKey where
    parser = takeLowerText >>= \case
        "documentnameprefix" -> pure DocumentNamePrefix
        "executionstatus" -> pure ExecutionStatus
        e -> fromTextError $ "Failure parsing AutomationExecutionFilterKey from value: '" <> e
           <> "'. Accepted values: documentnameprefix, executionstatus"

instance ToText AutomationExecutionFilterKey where
    toText = \case
        DocumentNamePrefix -> "DocumentNamePrefix"
        ExecutionStatus -> "ExecutionStatus"

instance Hashable     AutomationExecutionFilterKey
instance NFData       AutomationExecutionFilterKey
instance ToByteString AutomationExecutionFilterKey
instance ToQuery      AutomationExecutionFilterKey
instance ToHeader     AutomationExecutionFilterKey

instance ToJSON AutomationExecutionFilterKey where
    toJSON = toJSONText

data AutomationExecutionStatus
    = AESCancelled
    | AESFailed
    | AESInProgress
    | AESPending
    | AESSuccess
    | AESTimedOut
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText AutomationExecutionStatus where
    parser = takeLowerText >>= \case
        "cancelled" -> pure AESCancelled
        "failed" -> pure AESFailed
        "inprogress" -> pure AESInProgress
        "pending" -> pure AESPending
        "success" -> pure AESSuccess
        "timedout" -> pure AESTimedOut
        e -> fromTextError $ "Failure parsing AutomationExecutionStatus from value: '" <> e
           <> "'. Accepted values: cancelled, failed, inprogress, pending, success, timedout"

instance ToText AutomationExecutionStatus where
    toText = \case
        AESCancelled -> "Cancelled"
        AESFailed -> "Failed"
        AESInProgress -> "InProgress"
        AESPending -> "Pending"
        AESSuccess -> "Success"
        AESTimedOut -> "TimedOut"

instance Hashable     AutomationExecutionStatus
instance NFData       AutomationExecutionStatus
instance ToByteString AutomationExecutionStatus
instance ToQuery      AutomationExecutionStatus
instance ToHeader     AutomationExecutionStatus

instance FromJSON AutomationExecutionStatus where
    parseJSON = parseJSONText "AutomationExecutionStatus"

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
    | CISDelayed
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
        "delayed" -> pure CISDelayed
        "failed" -> pure CISFailed
        "inprogress" -> pure CISInProgress
        "pending" -> pure CISPending
        "success" -> pure CISSuccess
        "timedout" -> pure CISTimedOut
        e -> fromTextError $ "Failure parsing CommandInvocationStatus from value: '" <> e
           <> "'. Accepted values: cancelled, cancelling, delayed, failed, inprogress, pending, success, timedout"

instance ToText CommandInvocationStatus where
    toText = \case
        CISCancelled -> "Cancelled"
        CISCancelling -> "Cancelling"
        CISDelayed -> "Delayed"
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
    = DocumentType
    | Name
    | Owner
    | PlatformTypes
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText DocumentFilterKey where
    parser = takeLowerText >>= \case
        "documenttype" -> pure DocumentType
        "name" -> pure Name
        "owner" -> pure Owner
        "platformtypes" -> pure PlatformTypes
        e -> fromTextError $ "Failure parsing DocumentFilterKey from value: '" <> e
           <> "'. Accepted values: documenttype, name, owner, platformtypes"

instance ToText DocumentFilterKey where
    toText = \case
        DocumentType -> "DocumentType"
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
    = HashSHA1
    | HashSHA256
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText DocumentHashType where
    parser = takeLowerText >>= \case
        "sha1" -> pure HashSHA1
        "sha256" -> pure HashSHA256
        e -> fromTextError $ "Failure parsing DocumentHashType from value: '" <> e
           <> "'. Accepted values: sha1, sha256"

instance ToText DocumentHashType where
    toText = \case
        HashSHA1 -> "Sha1"
        HashSHA256 -> "Sha256"

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
    = DPTString
    | DPTStringList
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText DocumentParameterType where
    parser = takeLowerText >>= \case
        "string" -> pure DPTString
        "stringlist" -> pure DPTStringList
        e -> fromTextError $ "Failure parsing DocumentParameterType from value: '" <> e
           <> "'. Accepted values: string, stringlist"

instance ToText DocumentParameterType where
    toText = \case
        DPTString -> "String"
        DPTStringList -> "StringList"

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
    | Updating
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText DocumentStatus where
    parser = takeLowerText >>= \case
        "active" -> pure Active
        "creating" -> pure Creating
        "deleting" -> pure Deleting
        "updating" -> pure Updating
        e -> fromTextError $ "Failure parsing DocumentStatus from value: '" <> e
           <> "'. Accepted values: active, creating, deleting, updating"

instance ToText DocumentStatus where
    toText = \case
        Active -> "Active"
        Creating -> "Creating"
        Deleting -> "Deleting"
        Updating -> "Updating"

instance Hashable     DocumentStatus
instance NFData       DocumentStatus
instance ToByteString DocumentStatus
instance ToQuery      DocumentStatus
instance ToHeader     DocumentStatus

instance FromJSON DocumentStatus where
    parseJSON = parseJSONText "DocumentStatus"

data DocumentType
    = DTAutomation
    | DTCommand
    | DTPolicy
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText DocumentType where
    parser = takeLowerText >>= \case
        "automation" -> pure DTAutomation
        "command" -> pure DTCommand
        "policy" -> pure DTPolicy
        e -> fromTextError $ "Failure parsing DocumentType from value: '" <> e
           <> "'. Accepted values: automation, command, policy"

instance ToText DocumentType where
    toText = \case
        DTAutomation -> "Automation"
        DTCommand -> "Command"
        DTPolicy -> "Policy"

instance Hashable     DocumentType
instance NFData       DocumentType
instance ToByteString DocumentType
instance ToQuery      DocumentType
instance ToHeader     DocumentType

instance ToJSON DocumentType where
    toJSON = toJSONText

instance FromJSON DocumentType where
    parseJSON = parseJSONText "DocumentType"

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
    | IIFKAssociationStatus
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
        "associationstatus" -> pure IIFKAssociationStatus
        "iamrole" -> pure IIFKIAMRole
        "instanceids" -> pure IIFKInstanceIds
        "pingstatus" -> pure IIFKPingStatus
        "platformtypes" -> pure IIFKPlatformTypes
        "resourcetype" -> pure IIFKResourceType
        e -> fromTextError $ "Failure parsing InstanceInformationFilterKey from value: '" <> e
           <> "'. Accepted values: activationids, agentversion, associationstatus, iamrole, instanceids, pingstatus, platformtypes, resourcetype"

instance ToText InstanceInformationFilterKey where
    toText = \case
        IIFKActivationIds -> "ActivationIds"
        IIFKAgentVersion -> "AgentVersion"
        IIFKAssociationStatus -> "AssociationStatus"
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

data InventoryAttributeDataType
    = IADTNumber
    | IADTString
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText InventoryAttributeDataType where
    parser = takeLowerText >>= \case
        "number" -> pure IADTNumber
        "string" -> pure IADTString
        e -> fromTextError $ "Failure parsing InventoryAttributeDataType from value: '" <> e
           <> "'. Accepted values: number, string"

instance ToText InventoryAttributeDataType where
    toText = \case
        IADTNumber -> "number"
        IADTString -> "string"

instance Hashable     InventoryAttributeDataType
instance NFData       InventoryAttributeDataType
instance ToByteString InventoryAttributeDataType
instance ToQuery      InventoryAttributeDataType
instance ToHeader     InventoryAttributeDataType

instance FromJSON InventoryAttributeDataType where
    parseJSON = parseJSONText "InventoryAttributeDataType"

data InventoryQueryOperatorType
    = BeginWith
    | Equal
    | GreaterThan
    | LessThan
    | NotEqual
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText InventoryQueryOperatorType where
    parser = takeLowerText >>= \case
        "beginwith" -> pure BeginWith
        "equal" -> pure Equal
        "greaterthan" -> pure GreaterThan
        "lessthan" -> pure LessThan
        "notequal" -> pure NotEqual
        e -> fromTextError $ "Failure parsing InventoryQueryOperatorType from value: '" <> e
           <> "'. Accepted values: beginwith, equal, greaterthan, lessthan, notequal"

instance ToText InventoryQueryOperatorType where
    toText = \case
        BeginWith -> "BeginWith"
        Equal -> "Equal"
        GreaterThan -> "GreaterThan"
        LessThan -> "LessThan"
        NotEqual -> "NotEqual"

instance Hashable     InventoryQueryOperatorType
instance NFData       InventoryQueryOperatorType
instance ToByteString InventoryQueryOperatorType
instance ToQuery      InventoryQueryOperatorType
instance ToHeader     InventoryQueryOperatorType

instance ToJSON InventoryQueryOperatorType where
    toJSON = toJSONText

data MaintenanceWindowExecutionStatus
    = MWESCancelled
    | MWESCancelling
    | MWESFailed
    | MWESInProgress
    | MWESPending
    | MWESSkippedOverlapping
    | MWESSuccess
    | MWESTimedOut
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText MaintenanceWindowExecutionStatus where
    parser = takeLowerText >>= \case
        "cancelled" -> pure MWESCancelled
        "cancelling" -> pure MWESCancelling
        "failed" -> pure MWESFailed
        "in_progress" -> pure MWESInProgress
        "pending" -> pure MWESPending
        "skipped_overlapping" -> pure MWESSkippedOverlapping
        "success" -> pure MWESSuccess
        "timed_out" -> pure MWESTimedOut
        e -> fromTextError $ "Failure parsing MaintenanceWindowExecutionStatus from value: '" <> e
           <> "'. Accepted values: cancelled, cancelling, failed, in_progress, pending, skipped_overlapping, success, timed_out"

instance ToText MaintenanceWindowExecutionStatus where
    toText = \case
        MWESCancelled -> "CANCELLED"
        MWESCancelling -> "CANCELLING"
        MWESFailed -> "FAILED"
        MWESInProgress -> "IN_PROGRESS"
        MWESPending -> "PENDING"
        MWESSkippedOverlapping -> "SKIPPED_OVERLAPPING"
        MWESSuccess -> "SUCCESS"
        MWESTimedOut -> "TIMED_OUT"

instance Hashable     MaintenanceWindowExecutionStatus
instance NFData       MaintenanceWindowExecutionStatus
instance ToByteString MaintenanceWindowExecutionStatus
instance ToQuery      MaintenanceWindowExecutionStatus
instance ToHeader     MaintenanceWindowExecutionStatus

instance FromJSON MaintenanceWindowExecutionStatus where
    parseJSON = parseJSONText "MaintenanceWindowExecutionStatus"

data MaintenanceWindowResourceType =
    Instance
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText MaintenanceWindowResourceType where
    parser = takeLowerText >>= \case
        "instance" -> pure Instance
        e -> fromTextError $ "Failure parsing MaintenanceWindowResourceType from value: '" <> e
           <> "'. Accepted values: instance"

instance ToText MaintenanceWindowResourceType where
    toText = \case
        Instance -> "INSTANCE"

instance Hashable     MaintenanceWindowResourceType
instance NFData       MaintenanceWindowResourceType
instance ToByteString MaintenanceWindowResourceType
instance ToQuery      MaintenanceWindowResourceType
instance ToHeader     MaintenanceWindowResourceType

instance ToJSON MaintenanceWindowResourceType where
    toJSON = toJSONText

instance FromJSON MaintenanceWindowResourceType where
    parseJSON = parseJSONText "MaintenanceWindowResourceType"

data MaintenanceWindowTaskType =
    RunCommand
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText MaintenanceWindowTaskType where
    parser = takeLowerText >>= \case
        "run_command" -> pure RunCommand
        e -> fromTextError $ "Failure parsing MaintenanceWindowTaskType from value: '" <> e
           <> "'. Accepted values: run_command"

instance ToText MaintenanceWindowTaskType where
    toText = \case
        RunCommand -> "RUN_COMMAND"

instance Hashable     MaintenanceWindowTaskType
instance NFData       MaintenanceWindowTaskType
instance ToByteString MaintenanceWindowTaskType
instance ToQuery      MaintenanceWindowTaskType
instance ToHeader     MaintenanceWindowTaskType

instance ToJSON MaintenanceWindowTaskType where
    toJSON = toJSONText

instance FromJSON MaintenanceWindowTaskType where
    parseJSON = parseJSONText "MaintenanceWindowTaskType"

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

data ParameterType
    = SecureString
    | String
    | StringList
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText ParameterType where
    parser = takeLowerText >>= \case
        "securestring" -> pure SecureString
        "string" -> pure String
        "stringlist" -> pure StringList
        e -> fromTextError $ "Failure parsing ParameterType from value: '" <> e
           <> "'. Accepted values: securestring, string, stringlist"

instance ToText ParameterType where
    toText = \case
        SecureString -> "SecureString"
        String -> "String"
        StringList -> "StringList"

instance Hashable     ParameterType
instance NFData       ParameterType
instance ToByteString ParameterType
instance ToQuery      ParameterType
instance ToHeader     ParameterType

instance ToJSON ParameterType where
    toJSON = toJSONText

instance FromJSON ParameterType where
    parseJSON = parseJSONText "ParameterType"

data ParametersFilterKey
    = PFKKeyId
    | PFKName
    | PFKType
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText ParametersFilterKey where
    parser = takeLowerText >>= \case
        "keyid" -> pure PFKKeyId
        "name" -> pure PFKName
        "type" -> pure PFKType
        e -> fromTextError $ "Failure parsing ParametersFilterKey from value: '" <> e
           <> "'. Accepted values: keyid, name, type"

instance ToText ParametersFilterKey where
    toText = \case
        PFKKeyId -> "KeyId"
        PFKName -> "Name"
        PFKType -> "Type"

instance Hashable     ParametersFilterKey
instance NFData       ParametersFilterKey
instance ToByteString ParametersFilterKey
instance ToQuery      ParametersFilterKey
instance ToHeader     ParametersFilterKey

instance ToJSON ParametersFilterKey where
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

data ResourceTypeForTagging
    = RTFTMaintenanceWindow
    | RTFTManagedInstance
    | RTFTParameter
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText ResourceTypeForTagging where
    parser = takeLowerText >>= \case
        "maintenancewindow" -> pure RTFTMaintenanceWindow
        "managedinstance" -> pure RTFTManagedInstance
        "parameter" -> pure RTFTParameter
        e -> fromTextError $ "Failure parsing ResourceTypeForTagging from value: '" <> e
           <> "'. Accepted values: maintenancewindow, managedinstance, parameter"

instance ToText ResourceTypeForTagging where
    toText = \case
        RTFTMaintenanceWindow -> "MaintenanceWindow"
        RTFTManagedInstance -> "ManagedInstance"
        RTFTParameter -> "Parameter"

instance Hashable     ResourceTypeForTagging
instance NFData       ResourceTypeForTagging
instance ToByteString ResourceTypeForTagging
instance ToQuery      ResourceTypeForTagging
instance ToHeader     ResourceTypeForTagging

instance ToJSON ResourceTypeForTagging where
    toJSON = toJSONText
