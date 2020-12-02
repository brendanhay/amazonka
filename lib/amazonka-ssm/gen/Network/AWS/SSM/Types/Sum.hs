{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.Sum where

import Network.AWS.Prelude

data AssociationFilterKey
  = AFKAssociationId
  | AFKAssociationName
  | AFKAssociationStatusName
  | AFKInstanceId
  | AFKLastExecutedAfter
  | AFKLastExecutedBefore
  | AFKName
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AssociationFilterKey where
    parser = takeLowerText >>= \case
        "associationid" -> pure AFKAssociationId
        "associationname" -> pure AFKAssociationName
        "associationstatusname" -> pure AFKAssociationStatusName
        "instanceid" -> pure AFKInstanceId
        "lastexecutedafter" -> pure AFKLastExecutedAfter
        "lastexecutedbefore" -> pure AFKLastExecutedBefore
        "name" -> pure AFKName
        e -> fromTextError $ "Failure parsing AssociationFilterKey from value: '" <> e
           <> "'. Accepted values: associationid, associationname, associationstatusname, instanceid, lastexecutedafter, lastexecutedbefore, name"

instance ToText AssociationFilterKey where
    toText = \case
        AFKAssociationId -> "AssociationId"
        AFKAssociationName -> "AssociationName"
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
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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
  = AEFKCurrentAction
  | AEFKDocumentNamePrefix
  | AEFKExecutionId
  | AEFKExecutionStatus
  | AEFKParentExecutionId
  | AEFKStartTimeAfter
  | AEFKStartTimeBefore
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AutomationExecutionFilterKey where
    parser = takeLowerText >>= \case
        "currentaction" -> pure AEFKCurrentAction
        "documentnameprefix" -> pure AEFKDocumentNamePrefix
        "executionid" -> pure AEFKExecutionId
        "executionstatus" -> pure AEFKExecutionStatus
        "parentexecutionid" -> pure AEFKParentExecutionId
        "starttimeafter" -> pure AEFKStartTimeAfter
        "starttimebefore" -> pure AEFKStartTimeBefore
        e -> fromTextError $ "Failure parsing AutomationExecutionFilterKey from value: '" <> e
           <> "'. Accepted values: currentaction, documentnameprefix, executionid, executionstatus, parentexecutionid, starttimeafter, starttimebefore"

instance ToText AutomationExecutionFilterKey where
    toText = \case
        AEFKCurrentAction -> "CurrentAction"
        AEFKDocumentNamePrefix -> "DocumentNamePrefix"
        AEFKExecutionId -> "ExecutionId"
        AEFKExecutionStatus -> "ExecutionStatus"
        AEFKParentExecutionId -> "ParentExecutionId"
        AEFKStartTimeAfter -> "StartTimeAfter"
        AEFKStartTimeBefore -> "StartTimeBefore"

instance Hashable     AutomationExecutionFilterKey
instance NFData       AutomationExecutionFilterKey
instance ToByteString AutomationExecutionFilterKey
instance ToQuery      AutomationExecutionFilterKey
instance ToHeader     AutomationExecutionFilterKey

instance ToJSON AutomationExecutionFilterKey where
    toJSON = toJSONText

data AutomationExecutionStatus
  = AESCancelled
  | AESCancelling
  | AESFailed
  | AESInProgress
  | AESPending
  | AESSuccess
  | AESTimedOut
  | AESWaiting
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AutomationExecutionStatus where
    parser = takeLowerText >>= \case
        "cancelled" -> pure AESCancelled
        "cancelling" -> pure AESCancelling
        "failed" -> pure AESFailed
        "inprogress" -> pure AESInProgress
        "pending" -> pure AESPending
        "success" -> pure AESSuccess
        "timedout" -> pure AESTimedOut
        "waiting" -> pure AESWaiting
        e -> fromTextError $ "Failure parsing AutomationExecutionStatus from value: '" <> e
           <> "'. Accepted values: cancelled, cancelling, failed, inprogress, pending, success, timedout, waiting"

instance ToText AutomationExecutionStatus where
    toText = \case
        AESCancelled -> "Cancelled"
        AESCancelling -> "Cancelling"
        AESFailed -> "Failed"
        AESInProgress -> "InProgress"
        AESPending -> "Pending"
        AESSuccess -> "Success"
        AESTimedOut -> "TimedOut"
        AESWaiting -> "Waiting"

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
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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
  = CSCancelled
  | CSCancelling
  | CSFailed
  | CSInProgress
  | CSPending
  | CSSuccess
  | CSTimedOut
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CommandStatus where
    parser = takeLowerText >>= \case
        "cancelled" -> pure CSCancelled
        "cancelling" -> pure CSCancelling
        "failed" -> pure CSFailed
        "inprogress" -> pure CSInProgress
        "pending" -> pure CSPending
        "success" -> pure CSSuccess
        "timedout" -> pure CSTimedOut
        e -> fromTextError $ "Failure parsing CommandStatus from value: '" <> e
           <> "'. Accepted values: cancelled, cancelling, failed, inprogress, pending, success, timedout"

instance ToText CommandStatus where
    toText = \case
        CSCancelled -> "Cancelled"
        CSCancelling -> "Cancelling"
        CSFailed -> "Failed"
        CSInProgress -> "InProgress"
        CSPending -> "Pending"
        CSSuccess -> "Success"
        CSTimedOut -> "TimedOut"

instance Hashable     CommandStatus
instance NFData       CommandStatus
instance ToByteString CommandStatus
instance ToQuery      CommandStatus
instance ToHeader     CommandStatus

instance FromJSON CommandStatus where
    parseJSON = parseJSONText "CommandStatus"

data ComplianceQueryOperatorType
  = CQOTBeginWith
  | CQOTEqual
  | CQOTGreaterThan
  | CQOTLessThan
  | CQOTNotEqual
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ComplianceQueryOperatorType where
    parser = takeLowerText >>= \case
        "begin_with" -> pure CQOTBeginWith
        "equal" -> pure CQOTEqual
        "greater_than" -> pure CQOTGreaterThan
        "less_than" -> pure CQOTLessThan
        "not_equal" -> pure CQOTNotEqual
        e -> fromTextError $ "Failure parsing ComplianceQueryOperatorType from value: '" <> e
           <> "'. Accepted values: begin_with, equal, greater_than, less_than, not_equal"

instance ToText ComplianceQueryOperatorType where
    toText = \case
        CQOTBeginWith -> "BEGIN_WITH"
        CQOTEqual -> "EQUAL"
        CQOTGreaterThan -> "GREATER_THAN"
        CQOTLessThan -> "LESS_THAN"
        CQOTNotEqual -> "NOT_EQUAL"

instance Hashable     ComplianceQueryOperatorType
instance NFData       ComplianceQueryOperatorType
instance ToByteString ComplianceQueryOperatorType
instance ToQuery      ComplianceQueryOperatorType
instance ToHeader     ComplianceQueryOperatorType

instance ToJSON ComplianceQueryOperatorType where
    toJSON = toJSONText

data ComplianceSeverity
  = CSCritical
  | CSHigh
  | CSInformational
  | CSLow
  | CSMedium
  | CSUnspecified
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ComplianceSeverity where
    parser = takeLowerText >>= \case
        "critical" -> pure CSCritical
        "high" -> pure CSHigh
        "informational" -> pure CSInformational
        "low" -> pure CSLow
        "medium" -> pure CSMedium
        "unspecified" -> pure CSUnspecified
        e -> fromTextError $ "Failure parsing ComplianceSeverity from value: '" <> e
           <> "'. Accepted values: critical, high, informational, low, medium, unspecified"

instance ToText ComplianceSeverity where
    toText = \case
        CSCritical -> "CRITICAL"
        CSHigh -> "HIGH"
        CSInformational -> "INFORMATIONAL"
        CSLow -> "LOW"
        CSMedium -> "MEDIUM"
        CSUnspecified -> "UNSPECIFIED"

instance Hashable     ComplianceSeverity
instance NFData       ComplianceSeverity
instance ToByteString ComplianceSeverity
instance ToQuery      ComplianceSeverity
instance ToHeader     ComplianceSeverity

instance ToJSON ComplianceSeverity where
    toJSON = toJSONText

instance FromJSON ComplianceSeverity where
    parseJSON = parseJSONText "ComplianceSeverity"

data ComplianceStatus
  = Compliant
  | NonCompliant
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ComplianceStatus where
    parser = takeLowerText >>= \case
        "compliant" -> pure Compliant
        "non_compliant" -> pure NonCompliant
        e -> fromTextError $ "Failure parsing ComplianceStatus from value: '" <> e
           <> "'. Accepted values: compliant, non_compliant"

instance ToText ComplianceStatus where
    toText = \case
        Compliant -> "COMPLIANT"
        NonCompliant -> "NON_COMPLIANT"

instance Hashable     ComplianceStatus
instance NFData       ComplianceStatus
instance ToByteString ComplianceStatus
instance ToQuery      ComplianceStatus
instance ToHeader     ComplianceStatus

instance ToJSON ComplianceStatus where
    toJSON = toJSONText

instance FromJSON ComplianceStatus where
    parseJSON = parseJSONText "ComplianceStatus"

data DescribeActivationsFilterKeys
  = ActivationIds
  | DefaultInstanceName
  | IAMRole
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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

data DocumentFormat
  = JSON
  | Yaml
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DocumentFormat where
    parser = takeLowerText >>= \case
        "json" -> pure JSON
        "yaml" -> pure Yaml
        e -> fromTextError $ "Failure parsing DocumentFormat from value: '" <> e
           <> "'. Accepted values: json, yaml"

instance ToText DocumentFormat where
    toText = \case
        JSON -> "JSON"
        Yaml -> "YAML"

instance Hashable     DocumentFormat
instance NFData       DocumentFormat
instance ToByteString DocumentFormat
instance ToQuery      DocumentFormat
instance ToHeader     DocumentFormat

instance ToJSON DocumentFormat where
    toJSON = toJSONText

instance FromJSON DocumentFormat where
    parseJSON = parseJSONText "DocumentFormat"

data DocumentHashType
  = HashSHA1
  | HashSHA256
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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

data ExecutionMode
  = Auto
  | Interactive
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ExecutionMode where
    parser = takeLowerText >>= \case
        "auto" -> pure Auto
        "interactive" -> pure Interactive
        e -> fromTextError $ "Failure parsing ExecutionMode from value: '" <> e
           <> "'. Accepted values: auto, interactive"

instance ToText ExecutionMode where
    toText = \case
        Auto -> "Auto"
        Interactive -> "Interactive"

instance Hashable     ExecutionMode
instance NFData       ExecutionMode
instance ToByteString ExecutionMode
instance ToQuery      ExecutionMode
instance ToHeader     ExecutionMode

instance ToJSON ExecutionMode where
    toJSON = toJSONText

instance FromJSON ExecutionMode where
    parseJSON = parseJSONText "ExecutionMode"

data Fault
  = Client
  | Server
  | Unknown
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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

data InstancePatchStateOperatorType
  = IPSOTEqual
  | IPSOTGreaterThan
  | IPSOTLessThan
  | IPSOTNotEqual
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InstancePatchStateOperatorType where
    parser = takeLowerText >>= \case
        "equal" -> pure IPSOTEqual
        "greaterthan" -> pure IPSOTGreaterThan
        "lessthan" -> pure IPSOTLessThan
        "notequal" -> pure IPSOTNotEqual
        e -> fromTextError $ "Failure parsing InstancePatchStateOperatorType from value: '" <> e
           <> "'. Accepted values: equal, greaterthan, lessthan, notequal"

instance ToText InstancePatchStateOperatorType where
    toText = \case
        IPSOTEqual -> "Equal"
        IPSOTGreaterThan -> "GreaterThan"
        IPSOTLessThan -> "LessThan"
        IPSOTNotEqual -> "NotEqual"

instance Hashable     InstancePatchStateOperatorType
instance NFData       InstancePatchStateOperatorType
instance ToByteString InstancePatchStateOperatorType
instance ToQuery      InstancePatchStateOperatorType
instance ToHeader     InstancePatchStateOperatorType

instance ToJSON InstancePatchStateOperatorType where
    toJSON = toJSONText

data InventoryAttributeDataType
  = IADTNumber
  | IADTString
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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

data InventoryDeletionStatus
  = Complete
  | InProgress
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InventoryDeletionStatus where
    parser = takeLowerText >>= \case
        "complete" -> pure Complete
        "inprogress" -> pure InProgress
        e -> fromTextError $ "Failure parsing InventoryDeletionStatus from value: '" <> e
           <> "'. Accepted values: complete, inprogress"

instance ToText InventoryDeletionStatus where
    toText = \case
        Complete -> "Complete"
        InProgress -> "InProgress"

instance Hashable     InventoryDeletionStatus
instance NFData       InventoryDeletionStatus
instance ToByteString InventoryDeletionStatus
instance ToQuery      InventoryDeletionStatus
instance ToHeader     InventoryDeletionStatus

instance FromJSON InventoryDeletionStatus where
    parseJSON = parseJSONText "InventoryDeletionStatus"

data InventoryQueryOperatorType
  = BeginWith
  | Equal
  | GreaterThan
  | LessThan
  | NotEqual
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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

data InventorySchemaDeleteOption
  = DeleteSchema
  | DisableSchema
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InventorySchemaDeleteOption where
    parser = takeLowerText >>= \case
        "deleteschema" -> pure DeleteSchema
        "disableschema" -> pure DisableSchema
        e -> fromTextError $ "Failure parsing InventorySchemaDeleteOption from value: '" <> e
           <> "'. Accepted values: deleteschema, disableschema"

instance ToText InventorySchemaDeleteOption where
    toText = \case
        DeleteSchema -> "DeleteSchema"
        DisableSchema -> "DisableSchema"

instance Hashable     InventorySchemaDeleteOption
instance NFData       InventorySchemaDeleteOption
instance ToByteString InventorySchemaDeleteOption
instance ToQuery      InventorySchemaDeleteOption
instance ToHeader     InventorySchemaDeleteOption

instance ToJSON InventorySchemaDeleteOption where
    toJSON = toJSONText

data LastResourceDataSyncStatus
  = LRDSSFailed
  | LRDSSInProgress
  | LRDSSSuccessful
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LastResourceDataSyncStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure LRDSSFailed
        "inprogress" -> pure LRDSSInProgress
        "successful" -> pure LRDSSSuccessful
        e -> fromTextError $ "Failure parsing LastResourceDataSyncStatus from value: '" <> e
           <> "'. Accepted values: failed, inprogress, successful"

instance ToText LastResourceDataSyncStatus where
    toText = \case
        LRDSSFailed -> "Failed"
        LRDSSInProgress -> "InProgress"
        LRDSSSuccessful -> "Successful"

instance Hashable     LastResourceDataSyncStatus
instance NFData       LastResourceDataSyncStatus
instance ToByteString LastResourceDataSyncStatus
instance ToQuery      LastResourceDataSyncStatus
instance ToHeader     LastResourceDataSyncStatus

instance FromJSON LastResourceDataSyncStatus where
    parseJSON = parseJSONText "LastResourceDataSyncStatus"

data MaintenanceWindowExecutionStatus
  = MWESCancelled
  | MWESCancelling
  | MWESFailed
  | MWESInProgress
  | MWESPending
  | MWESSkippedOverlapping
  | MWESSuccess
  | MWESTimedOut
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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

data MaintenanceWindowTaskType
  = Automation
  | Lambda
  | RunCommand
  | StepFunctions
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MaintenanceWindowTaskType where
    parser = takeLowerText >>= \case
        "automation" -> pure Automation
        "lambda" -> pure Lambda
        "run_command" -> pure RunCommand
        "step_functions" -> pure StepFunctions
        e -> fromTextError $ "Failure parsing MaintenanceWindowTaskType from value: '" <> e
           <> "'. Accepted values: automation, lambda, run_command, step_functions"

instance ToText MaintenanceWindowTaskType where
    toText = \case
        Automation -> "AUTOMATION"
        Lambda -> "LAMBDA"
        RunCommand -> "RUN_COMMAND"
        StepFunctions -> "STEP_FUNCTIONS"

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
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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

data OperatingSystem
  = AmazonLinux
  | Centos
  | RedhatEnterpriseLinux
  | Suse
  | Ubuntu
  | Windows
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OperatingSystem where
    parser = takeLowerText >>= \case
        "amazon_linux" -> pure AmazonLinux
        "centos" -> pure Centos
        "redhat_enterprise_linux" -> pure RedhatEnterpriseLinux
        "suse" -> pure Suse
        "ubuntu" -> pure Ubuntu
        "windows" -> pure Windows
        e -> fromTextError $ "Failure parsing OperatingSystem from value: '" <> e
           <> "'. Accepted values: amazon_linux, centos, redhat_enterprise_linux, suse, ubuntu, windows"

instance ToText OperatingSystem where
    toText = \case
        AmazonLinux -> "AMAZON_LINUX"
        Centos -> "CENTOS"
        RedhatEnterpriseLinux -> "REDHAT_ENTERPRISE_LINUX"
        Suse -> "SUSE"
        Ubuntu -> "UBUNTU"
        Windows -> "WINDOWS"

instance Hashable     OperatingSystem
instance NFData       OperatingSystem
instance ToByteString OperatingSystem
instance ToQuery      OperatingSystem
instance ToHeader     OperatingSystem

instance ToJSON OperatingSystem where
    toJSON = toJSONText

instance FromJSON OperatingSystem where
    parseJSON = parseJSONText "OperatingSystem"

data ParameterType
  = SecureString
  | String
  | StringList
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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

data PatchComplianceDataState
  = Failed
  | Installed
  | InstalledOther
  | Missing
  | NotApplicable
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PatchComplianceDataState where
    parser = takeLowerText >>= \case
        "failed" -> pure Failed
        "installed" -> pure Installed
        "installed_other" -> pure InstalledOther
        "missing" -> pure Missing
        "not_applicable" -> pure NotApplicable
        e -> fromTextError $ "Failure parsing PatchComplianceDataState from value: '" <> e
           <> "'. Accepted values: failed, installed, installed_other, missing, not_applicable"

instance ToText PatchComplianceDataState where
    toText = \case
        Failed -> "FAILED"
        Installed -> "INSTALLED"
        InstalledOther -> "INSTALLED_OTHER"
        Missing -> "MISSING"
        NotApplicable -> "NOT_APPLICABLE"

instance Hashable     PatchComplianceDataState
instance NFData       PatchComplianceDataState
instance ToByteString PatchComplianceDataState
instance ToQuery      PatchComplianceDataState
instance ToHeader     PatchComplianceDataState

instance FromJSON PatchComplianceDataState where
    parseJSON = parseJSONText "PatchComplianceDataState"

data PatchComplianceLevel
  = Critical
  | High
  | Informational
  | Low
  | Medium
  | Unspecified
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PatchComplianceLevel where
    parser = takeLowerText >>= \case
        "critical" -> pure Critical
        "high" -> pure High
        "informational" -> pure Informational
        "low" -> pure Low
        "medium" -> pure Medium
        "unspecified" -> pure Unspecified
        e -> fromTextError $ "Failure parsing PatchComplianceLevel from value: '" <> e
           <> "'. Accepted values: critical, high, informational, low, medium, unspecified"

instance ToText PatchComplianceLevel where
    toText = \case
        Critical -> "CRITICAL"
        High -> "HIGH"
        Informational -> "INFORMATIONAL"
        Low -> "LOW"
        Medium -> "MEDIUM"
        Unspecified -> "UNSPECIFIED"

instance Hashable     PatchComplianceLevel
instance NFData       PatchComplianceLevel
instance ToByteString PatchComplianceLevel
instance ToQuery      PatchComplianceLevel
instance ToHeader     PatchComplianceLevel

instance ToJSON PatchComplianceLevel where
    toJSON = toJSONText

instance FromJSON PatchComplianceLevel where
    parseJSON = parseJSONText "PatchComplianceLevel"

data PatchDeploymentStatus
  = Approved
  | ExplicitApproved
  | ExplicitRejected
  | PendingApproval
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PatchDeploymentStatus where
    parser = takeLowerText >>= \case
        "approved" -> pure Approved
        "explicit_approved" -> pure ExplicitApproved
        "explicit_rejected" -> pure ExplicitRejected
        "pending_approval" -> pure PendingApproval
        e -> fromTextError $ "Failure parsing PatchDeploymentStatus from value: '" <> e
           <> "'. Accepted values: approved, explicit_approved, explicit_rejected, pending_approval"

instance ToText PatchDeploymentStatus where
    toText = \case
        Approved -> "APPROVED"
        ExplicitApproved -> "EXPLICIT_APPROVED"
        ExplicitRejected -> "EXPLICIT_REJECTED"
        PendingApproval -> "PENDING_APPROVAL"

instance Hashable     PatchDeploymentStatus
instance NFData       PatchDeploymentStatus
instance ToByteString PatchDeploymentStatus
instance ToQuery      PatchDeploymentStatus
instance ToHeader     PatchDeploymentStatus

instance FromJSON PatchDeploymentStatus where
    parseJSON = parseJSONText "PatchDeploymentStatus"

data PatchFilterKey
  = Classification
  | MsrcSeverity
  | PatchId
  | Priority
  | Product
  | Section
  | Severity
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PatchFilterKey where
    parser = takeLowerText >>= \case
        "classification" -> pure Classification
        "msrc_severity" -> pure MsrcSeverity
        "patch_id" -> pure PatchId
        "priority" -> pure Priority
        "product" -> pure Product
        "section" -> pure Section
        "severity" -> pure Severity
        e -> fromTextError $ "Failure parsing PatchFilterKey from value: '" <> e
           <> "'. Accepted values: classification, msrc_severity, patch_id, priority, product, section, severity"

instance ToText PatchFilterKey where
    toText = \case
        Classification -> "CLASSIFICATION"
        MsrcSeverity -> "MSRC_SEVERITY"
        PatchId -> "PATCH_ID"
        Priority -> "PRIORITY"
        Product -> "PRODUCT"
        Section -> "SECTION"
        Severity -> "SEVERITY"

instance Hashable     PatchFilterKey
instance NFData       PatchFilterKey
instance ToByteString PatchFilterKey
instance ToQuery      PatchFilterKey
instance ToHeader     PatchFilterKey

instance ToJSON PatchFilterKey where
    toJSON = toJSONText

instance FromJSON PatchFilterKey where
    parseJSON = parseJSONText "PatchFilterKey"

data PatchOperationType
  = Install
  | Scan
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PatchOperationType where
    parser = takeLowerText >>= \case
        "install" -> pure Install
        "scan" -> pure Scan
        e -> fromTextError $ "Failure parsing PatchOperationType from value: '" <> e
           <> "'. Accepted values: install, scan"

instance ToText PatchOperationType where
    toText = \case
        Install -> "Install"
        Scan -> "Scan"

instance Hashable     PatchOperationType
instance NFData       PatchOperationType
instance ToByteString PatchOperationType
instance ToQuery      PatchOperationType
instance ToHeader     PatchOperationType

instance FromJSON PatchOperationType where
    parseJSON = parseJSONText "PatchOperationType"

data PingStatus
  = ConnectionLost
  | Inactive
  | Online
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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
  = PTLinux
  | PTWindows
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PlatformType where
    parser = takeLowerText >>= \case
        "linux" -> pure PTLinux
        "windows" -> pure PTWindows
        e -> fromTextError $ "Failure parsing PlatformType from value: '" <> e
           <> "'. Accepted values: linux, windows"

instance ToText PlatformType where
    toText = \case
        PTLinux -> "Linux"
        PTWindows -> "Windows"

instance Hashable     PlatformType
instance NFData       PlatformType
instance ToByteString PlatformType
instance ToQuery      PlatformType
instance ToHeader     PlatformType

instance FromJSON PlatformType where
    parseJSON = parseJSONText "PlatformType"

data ResourceDataSyncS3Format =
  JSONSerDe
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ResourceDataSyncS3Format where
    parser = takeLowerText >>= \case
        "jsonserde" -> pure JSONSerDe
        e -> fromTextError $ "Failure parsing ResourceDataSyncS3Format from value: '" <> e
           <> "'. Accepted values: jsonserde"

instance ToText ResourceDataSyncS3Format where
    toText = \case
        JSONSerDe -> "JsonSerDe"

instance Hashable     ResourceDataSyncS3Format
instance NFData       ResourceDataSyncS3Format
instance ToByteString ResourceDataSyncS3Format
instance ToQuery      ResourceDataSyncS3Format
instance ToHeader     ResourceDataSyncS3Format

instance ToJSON ResourceDataSyncS3Format where
    toJSON = toJSONText

instance FromJSON ResourceDataSyncS3Format where
    parseJSON = parseJSONText "ResourceDataSyncS3Format"

data ResourceType
  = Document
  | EC2Instance
  | ManagedInstance
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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
  = RTFTDocument
  | RTFTMaintenanceWindow
  | RTFTManagedInstance
  | RTFTParameter
  | RTFTPatchBaseline
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ResourceTypeForTagging where
    parser = takeLowerText >>= \case
        "document" -> pure RTFTDocument
        "maintenancewindow" -> pure RTFTMaintenanceWindow
        "managedinstance" -> pure RTFTManagedInstance
        "parameter" -> pure RTFTParameter
        "patchbaseline" -> pure RTFTPatchBaseline
        e -> fromTextError $ "Failure parsing ResourceTypeForTagging from value: '" <> e
           <> "'. Accepted values: document, maintenancewindow, managedinstance, parameter, patchbaseline"

instance ToText ResourceTypeForTagging where
    toText = \case
        RTFTDocument -> "Document"
        RTFTMaintenanceWindow -> "MaintenanceWindow"
        RTFTManagedInstance -> "ManagedInstance"
        RTFTParameter -> "Parameter"
        RTFTPatchBaseline -> "PatchBaseline"

instance Hashable     ResourceTypeForTagging
instance NFData       ResourceTypeForTagging
instance ToByteString ResourceTypeForTagging
instance ToQuery      ResourceTypeForTagging
instance ToHeader     ResourceTypeForTagging

instance ToJSON ResourceTypeForTagging where
    toJSON = toJSONText

data SignalType
  = Approve
  | Reject
  | Resume
  | StartStep
  | StopStep
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SignalType where
    parser = takeLowerText >>= \case
        "approve" -> pure Approve
        "reject" -> pure Reject
        "resume" -> pure Resume
        "startstep" -> pure StartStep
        "stopstep" -> pure StopStep
        e -> fromTextError $ "Failure parsing SignalType from value: '" <> e
           <> "'. Accepted values: approve, reject, resume, startstep, stopstep"

instance ToText SignalType where
    toText = \case
        Approve -> "Approve"
        Reject -> "Reject"
        Resume -> "Resume"
        StartStep -> "StartStep"
        StopStep -> "StopStep"

instance Hashable     SignalType
instance NFData       SignalType
instance ToByteString SignalType
instance ToQuery      SignalType
instance ToHeader     SignalType

instance ToJSON SignalType where
    toJSON = toJSONText

data StepExecutionFilterKey
  = Action
  | StartTimeAfter
  | StartTimeBefore
  | StepExecutionId
  | StepExecutionStatus
  | StepName
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StepExecutionFilterKey where
    parser = takeLowerText >>= \case
        "action" -> pure Action
        "starttimeafter" -> pure StartTimeAfter
        "starttimebefore" -> pure StartTimeBefore
        "stepexecutionid" -> pure StepExecutionId
        "stepexecutionstatus" -> pure StepExecutionStatus
        "stepname" -> pure StepName
        e -> fromTextError $ "Failure parsing StepExecutionFilterKey from value: '" <> e
           <> "'. Accepted values: action, starttimeafter, starttimebefore, stepexecutionid, stepexecutionstatus, stepname"

instance ToText StepExecutionFilterKey where
    toText = \case
        Action -> "Action"
        StartTimeAfter -> "StartTimeAfter"
        StartTimeBefore -> "StartTimeBefore"
        StepExecutionId -> "StepExecutionId"
        StepExecutionStatus -> "StepExecutionStatus"
        StepName -> "StepName"

instance Hashable     StepExecutionFilterKey
instance NFData       StepExecutionFilterKey
instance ToByteString StepExecutionFilterKey
instance ToQuery      StepExecutionFilterKey
instance ToHeader     StepExecutionFilterKey

instance ToJSON StepExecutionFilterKey where
    toJSON = toJSONText

data StopType
  = STCancel
  | STComplete
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StopType where
    parser = takeLowerText >>= \case
        "cancel" -> pure STCancel
        "complete" -> pure STComplete
        e -> fromTextError $ "Failure parsing StopType from value: '" <> e
           <> "'. Accepted values: cancel, complete"

instance ToText StopType where
    toText = \case
        STCancel -> "Cancel"
        STComplete -> "Complete"

instance Hashable     StopType
instance NFData       StopType
instance ToByteString StopType
instance ToQuery      StopType
instance ToHeader     StopType

instance ToJSON StopType where
    toJSON = toJSONText
