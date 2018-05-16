{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.Sum where

import Network.AWS.Prelude

data AccountGateStatus
  = AGSFailed
  | AGSSkipped
  | AGSSucceeded
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AccountGateStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure AGSFailed
        "skipped" -> pure AGSSkipped
        "succeeded" -> pure AGSSucceeded
        e -> fromTextError $ "Failure parsing AccountGateStatus from value: '" <> e
           <> "'. Accepted values: failed, skipped, succeeded"

instance ToText AccountGateStatus where
    toText = \case
        AGSFailed -> "FAILED"
        AGSSkipped -> "SKIPPED"
        AGSSucceeded -> "SUCCEEDED"

instance Hashable     AccountGateStatus
instance NFData       AccountGateStatus
instance ToByteString AccountGateStatus
instance ToQuery      AccountGateStatus
instance ToHeader     AccountGateStatus

instance FromXML AccountGateStatus where
    parseXML = parseXMLText "AccountGateStatus"

data Capability
  = CapabilityIAM
  | CapabilityNamedIAM
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Capability where
    parser = takeLowerText >>= \case
        "capability_iam" -> pure CapabilityIAM
        "capability_named_iam" -> pure CapabilityNamedIAM
        e -> fromTextError $ "Failure parsing Capability from value: '" <> e
           <> "'. Accepted values: capability_iam, capability_named_iam"

instance ToText Capability where
    toText = \case
        CapabilityIAM -> "CAPABILITY_IAM"
        CapabilityNamedIAM -> "CAPABILITY_NAMED_IAM"

instance Hashable     Capability
instance NFData       Capability
instance ToByteString Capability
instance ToQuery      Capability
instance ToHeader     Capability

instance FromXML Capability where
    parseXML = parseXMLText "Capability"

data ChangeAction
  = Add
  | Modify
  | Remove
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ChangeAction where
    parser = takeLowerText >>= \case
        "add" -> pure Add
        "modify" -> pure Modify
        "remove" -> pure Remove
        e -> fromTextError $ "Failure parsing ChangeAction from value: '" <> e
           <> "'. Accepted values: add, modify, remove"

instance ToText ChangeAction where
    toText = \case
        Add -> "Add"
        Modify -> "Modify"
        Remove -> "Remove"

instance Hashable     ChangeAction
instance NFData       ChangeAction
instance ToByteString ChangeAction
instance ToQuery      ChangeAction
instance ToHeader     ChangeAction

instance FromXML ChangeAction where
    parseXML = parseXMLText "ChangeAction"

data ChangeSetStatus
  = CSSCreateComplete
  | CSSCreateInProgress
  | CSSCreatePending
  | CSSDeleteComplete
  | CSSFailed
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ChangeSetStatus where
    parser = takeLowerText >>= \case
        "create_complete" -> pure CSSCreateComplete
        "create_in_progress" -> pure CSSCreateInProgress
        "create_pending" -> pure CSSCreatePending
        "delete_complete" -> pure CSSDeleteComplete
        "failed" -> pure CSSFailed
        e -> fromTextError $ "Failure parsing ChangeSetStatus from value: '" <> e
           <> "'. Accepted values: create_complete, create_in_progress, create_pending, delete_complete, failed"

instance ToText ChangeSetStatus where
    toText = \case
        CSSCreateComplete -> "CREATE_COMPLETE"
        CSSCreateInProgress -> "CREATE_IN_PROGRESS"
        CSSCreatePending -> "CREATE_PENDING"
        CSSDeleteComplete -> "DELETE_COMPLETE"
        CSSFailed -> "FAILED"

instance Hashable     ChangeSetStatus
instance NFData       ChangeSetStatus
instance ToByteString ChangeSetStatus
instance ToQuery      ChangeSetStatus
instance ToHeader     ChangeSetStatus

instance FromXML ChangeSetStatus where
    parseXML = parseXMLText "ChangeSetStatus"

data ChangeSetType
  = Create
  | Update
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ChangeSetType where
    parser = takeLowerText >>= \case
        "create" -> pure Create
        "update" -> pure Update
        e -> fromTextError $ "Failure parsing ChangeSetType from value: '" <> e
           <> "'. Accepted values: create, update"

instance ToText ChangeSetType where
    toText = \case
        Create -> "CREATE"
        Update -> "UPDATE"

instance Hashable     ChangeSetType
instance NFData       ChangeSetType
instance ToByteString ChangeSetType
instance ToQuery      ChangeSetType
instance ToHeader     ChangeSetType

data ChangeSource
  = Automatic
  | DirectModification
  | ParameterReference
  | ResourceAttribute
  | ResourceReference
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ChangeSource where
    parser = takeLowerText >>= \case
        "automatic" -> pure Automatic
        "directmodification" -> pure DirectModification
        "parameterreference" -> pure ParameterReference
        "resourceattribute" -> pure ResourceAttribute
        "resourcereference" -> pure ResourceReference
        e -> fromTextError $ "Failure parsing ChangeSource from value: '" <> e
           <> "'. Accepted values: automatic, directmodification, parameterreference, resourceattribute, resourcereference"

instance ToText ChangeSource where
    toText = \case
        Automatic -> "Automatic"
        DirectModification -> "DirectModification"
        ParameterReference -> "ParameterReference"
        ResourceAttribute -> "ResourceAttribute"
        ResourceReference -> "ResourceReference"

instance Hashable     ChangeSource
instance NFData       ChangeSource
instance ToByteString ChangeSource
instance ToQuery      ChangeSource
instance ToHeader     ChangeSource

instance FromXML ChangeSource where
    parseXML = parseXMLText "ChangeSource"

data ChangeType =
  Resource
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ChangeType where
    parser = takeLowerText >>= \case
        "resource" -> pure Resource
        e -> fromTextError $ "Failure parsing ChangeType from value: '" <> e
           <> "'. Accepted values: resource"

instance ToText ChangeType where
    toText = \case
        Resource -> "Resource"

instance Hashable     ChangeType
instance NFData       ChangeType
instance ToByteString ChangeType
instance ToQuery      ChangeType
instance ToHeader     ChangeType

instance FromXML ChangeType where
    parseXML = parseXMLText "ChangeType"

data EvaluationType
  = Dynamic
  | Static
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EvaluationType where
    parser = takeLowerText >>= \case
        "dynamic" -> pure Dynamic
        "static" -> pure Static
        e -> fromTextError $ "Failure parsing EvaluationType from value: '" <> e
           <> "'. Accepted values: dynamic, static"

instance ToText EvaluationType where
    toText = \case
        Dynamic -> "Dynamic"
        Static -> "Static"

instance Hashable     EvaluationType
instance NFData       EvaluationType
instance ToByteString EvaluationType
instance ToQuery      EvaluationType
instance ToHeader     EvaluationType

instance FromXML EvaluationType where
    parseXML = parseXMLText "EvaluationType"

data ExecutionStatus
  = Available
  | ExecuteComplete
  | ExecuteFailed
  | ExecuteInProgress
  | Obsolete
  | Unavailable
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ExecutionStatus where
    parser = takeLowerText >>= \case
        "available" -> pure Available
        "execute_complete" -> pure ExecuteComplete
        "execute_failed" -> pure ExecuteFailed
        "execute_in_progress" -> pure ExecuteInProgress
        "obsolete" -> pure Obsolete
        "unavailable" -> pure Unavailable
        e -> fromTextError $ "Failure parsing ExecutionStatus from value: '" <> e
           <> "'. Accepted values: available, execute_complete, execute_failed, execute_in_progress, obsolete, unavailable"

instance ToText ExecutionStatus where
    toText = \case
        Available -> "AVAILABLE"
        ExecuteComplete -> "EXECUTE_COMPLETE"
        ExecuteFailed -> "EXECUTE_FAILED"
        ExecuteInProgress -> "EXECUTE_IN_PROGRESS"
        Obsolete -> "OBSOLETE"
        Unavailable -> "UNAVAILABLE"

instance Hashable     ExecutionStatus
instance NFData       ExecutionStatus
instance ToByteString ExecutionStatus
instance ToQuery      ExecutionStatus
instance ToHeader     ExecutionStatus

instance FromXML ExecutionStatus where
    parseXML = parseXMLText "ExecutionStatus"

data OnFailure
  = Delete
  | DoNothing
  | Rollback
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OnFailure where
    parser = takeLowerText >>= \case
        "delete" -> pure Delete
        "do_nothing" -> pure DoNothing
        "rollback" -> pure Rollback
        e -> fromTextError $ "Failure parsing OnFailure from value: '" <> e
           <> "'. Accepted values: delete, do_nothing, rollback"

instance ToText OnFailure where
    toText = \case
        Delete -> "DELETE"
        DoNothing -> "DO_NOTHING"
        Rollback -> "ROLLBACK"

instance Hashable     OnFailure
instance NFData       OnFailure
instance ToByteString OnFailure
instance ToQuery      OnFailure
instance ToHeader     OnFailure

data Replacement
  = Conditional
  | False'
  | True'
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Replacement where
    parser = takeLowerText >>= \case
        "conditional" -> pure Conditional
        "false" -> pure False'
        "true" -> pure True'
        e -> fromTextError $ "Failure parsing Replacement from value: '" <> e
           <> "'. Accepted values: conditional, false, true"

instance ToText Replacement where
    toText = \case
        Conditional -> "Conditional"
        False' -> "False"
        True' -> "True"

instance Hashable     Replacement
instance NFData       Replacement
instance ToByteString Replacement
instance ToQuery      Replacement
instance ToHeader     Replacement

instance FromXML Replacement where
    parseXML = parseXMLText "Replacement"

data RequiresRecreation
  = Always
  | Conditionally
  | Never
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RequiresRecreation where
    parser = takeLowerText >>= \case
        "always" -> pure Always
        "conditionally" -> pure Conditionally
        "never" -> pure Never
        e -> fromTextError $ "Failure parsing RequiresRecreation from value: '" <> e
           <> "'. Accepted values: always, conditionally, never"

instance ToText RequiresRecreation where
    toText = \case
        Always -> "Always"
        Conditionally -> "Conditionally"
        Never -> "Never"

instance Hashable     RequiresRecreation
instance NFData       RequiresRecreation
instance ToByteString RequiresRecreation
instance ToQuery      RequiresRecreation
instance ToHeader     RequiresRecreation

instance FromXML RequiresRecreation where
    parseXML = parseXMLText "RequiresRecreation"

data ResourceAttribute
  = CreationPolicy
  | DeletionPolicy
  | Metadata
  | Properties
  | Tags
  | UpdatePolicy
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ResourceAttribute where
    parser = takeLowerText >>= \case
        "creationpolicy" -> pure CreationPolicy
        "deletionpolicy" -> pure DeletionPolicy
        "metadata" -> pure Metadata
        "properties" -> pure Properties
        "tags" -> pure Tags
        "updatepolicy" -> pure UpdatePolicy
        e -> fromTextError $ "Failure parsing ResourceAttribute from value: '" <> e
           <> "'. Accepted values: creationpolicy, deletionpolicy, metadata, properties, tags, updatepolicy"

instance ToText ResourceAttribute where
    toText = \case
        CreationPolicy -> "CreationPolicy"
        DeletionPolicy -> "DeletionPolicy"
        Metadata -> "Metadata"
        Properties -> "Properties"
        Tags -> "Tags"
        UpdatePolicy -> "UpdatePolicy"

instance Hashable     ResourceAttribute
instance NFData       ResourceAttribute
instance ToByteString ResourceAttribute
instance ToQuery      ResourceAttribute
instance ToHeader     ResourceAttribute

instance FromXML ResourceAttribute where
    parseXML = parseXMLText "ResourceAttribute"

data ResourceSignalStatus
  = Failure
  | Success
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ResourceSignalStatus where
    parser = takeLowerText >>= \case
        "failure" -> pure Failure
        "success" -> pure Success
        e -> fromTextError $ "Failure parsing ResourceSignalStatus from value: '" <> e
           <> "'. Accepted values: failure, success"

instance ToText ResourceSignalStatus where
    toText = \case
        Failure -> "FAILURE"
        Success -> "SUCCESS"

instance Hashable     ResourceSignalStatus
instance NFData       ResourceSignalStatus
instance ToByteString ResourceSignalStatus
instance ToQuery      ResourceSignalStatus
instance ToHeader     ResourceSignalStatus

data ResourceStatus
  = CreateComplete
  | CreateFailed
  | CreateInProgress
  | DeleteComplete
  | DeleteFailed
  | DeleteInProgress
  | DeleteSkipped
  | UpdateComplete
  | UpdateFailed
  | UpdateInProgress
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ResourceStatus where
    parser = takeLowerText >>= \case
        "create_complete" -> pure CreateComplete
        "create_failed" -> pure CreateFailed
        "create_in_progress" -> pure CreateInProgress
        "delete_complete" -> pure DeleteComplete
        "delete_failed" -> pure DeleteFailed
        "delete_in_progress" -> pure DeleteInProgress
        "delete_skipped" -> pure DeleteSkipped
        "update_complete" -> pure UpdateComplete
        "update_failed" -> pure UpdateFailed
        "update_in_progress" -> pure UpdateInProgress
        e -> fromTextError $ "Failure parsing ResourceStatus from value: '" <> e
           <> "'. Accepted values: create_complete, create_failed, create_in_progress, delete_complete, delete_failed, delete_in_progress, delete_skipped, update_complete, update_failed, update_in_progress"

instance ToText ResourceStatus where
    toText = \case
        CreateComplete -> "CREATE_COMPLETE"
        CreateFailed -> "CREATE_FAILED"
        CreateInProgress -> "CREATE_IN_PROGRESS"
        DeleteComplete -> "DELETE_COMPLETE"
        DeleteFailed -> "DELETE_FAILED"
        DeleteInProgress -> "DELETE_IN_PROGRESS"
        DeleteSkipped -> "DELETE_SKIPPED"
        UpdateComplete -> "UPDATE_COMPLETE"
        UpdateFailed -> "UPDATE_FAILED"
        UpdateInProgress -> "UPDATE_IN_PROGRESS"

instance Hashable     ResourceStatus
instance NFData       ResourceStatus
instance ToByteString ResourceStatus
instance ToQuery      ResourceStatus
instance ToHeader     ResourceStatus

instance FromXML ResourceStatus where
    parseXML = parseXMLText "ResourceStatus"

data StackInstanceStatus
  = Current
  | Inoperable
  | Outdated
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StackInstanceStatus where
    parser = takeLowerText >>= \case
        "current" -> pure Current
        "inoperable" -> pure Inoperable
        "outdated" -> pure Outdated
        e -> fromTextError $ "Failure parsing StackInstanceStatus from value: '" <> e
           <> "'. Accepted values: current, inoperable, outdated"

instance ToText StackInstanceStatus where
    toText = \case
        Current -> "CURRENT"
        Inoperable -> "INOPERABLE"
        Outdated -> "OUTDATED"

instance Hashable     StackInstanceStatus
instance NFData       StackInstanceStatus
instance ToByteString StackInstanceStatus
instance ToQuery      StackInstanceStatus
instance ToHeader     StackInstanceStatus

instance FromXML StackInstanceStatus where
    parseXML = parseXMLText "StackInstanceStatus"

data StackSetOperationAction
  = SSOACreate
  | SSOADelete
  | SSOAUpdate
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StackSetOperationAction where
    parser = takeLowerText >>= \case
        "create" -> pure SSOACreate
        "delete" -> pure SSOADelete
        "update" -> pure SSOAUpdate
        e -> fromTextError $ "Failure parsing StackSetOperationAction from value: '" <> e
           <> "'. Accepted values: create, delete, update"

instance ToText StackSetOperationAction where
    toText = \case
        SSOACreate -> "CREATE"
        SSOADelete -> "DELETE"
        SSOAUpdate -> "UPDATE"

instance Hashable     StackSetOperationAction
instance NFData       StackSetOperationAction
instance ToByteString StackSetOperationAction
instance ToQuery      StackSetOperationAction
instance ToHeader     StackSetOperationAction

instance FromXML StackSetOperationAction where
    parseXML = parseXMLText "StackSetOperationAction"

data StackSetOperationResultStatus
  = Cancelled
  | Failed
  | Pending
  | Running
  | Succeeded
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StackSetOperationResultStatus where
    parser = takeLowerText >>= \case
        "cancelled" -> pure Cancelled
        "failed" -> pure Failed
        "pending" -> pure Pending
        "running" -> pure Running
        "succeeded" -> pure Succeeded
        e -> fromTextError $ "Failure parsing StackSetOperationResultStatus from value: '" <> e
           <> "'. Accepted values: cancelled, failed, pending, running, succeeded"

instance ToText StackSetOperationResultStatus where
    toText = \case
        Cancelled -> "CANCELLED"
        Failed -> "FAILED"
        Pending -> "PENDING"
        Running -> "RUNNING"
        Succeeded -> "SUCCEEDED"

instance Hashable     StackSetOperationResultStatus
instance NFData       StackSetOperationResultStatus
instance ToByteString StackSetOperationResultStatus
instance ToQuery      StackSetOperationResultStatus
instance ToHeader     StackSetOperationResultStatus

instance FromXML StackSetOperationResultStatus where
    parseXML = parseXMLText "StackSetOperationResultStatus"

data StackSetOperationStatus
  = SSOSFailed
  | SSOSRunning
  | SSOSStopped
  | SSOSStopping
  | SSOSSucceeded
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StackSetOperationStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure SSOSFailed
        "running" -> pure SSOSRunning
        "stopped" -> pure SSOSStopped
        "stopping" -> pure SSOSStopping
        "succeeded" -> pure SSOSSucceeded
        e -> fromTextError $ "Failure parsing StackSetOperationStatus from value: '" <> e
           <> "'. Accepted values: failed, running, stopped, stopping, succeeded"

instance ToText StackSetOperationStatus where
    toText = \case
        SSOSFailed -> "FAILED"
        SSOSRunning -> "RUNNING"
        SSOSStopped -> "STOPPED"
        SSOSStopping -> "STOPPING"
        SSOSSucceeded -> "SUCCEEDED"

instance Hashable     StackSetOperationStatus
instance NFData       StackSetOperationStatus
instance ToByteString StackSetOperationStatus
instance ToQuery      StackSetOperationStatus
instance ToHeader     StackSetOperationStatus

instance FromXML StackSetOperationStatus where
    parseXML = parseXMLText "StackSetOperationStatus"

data StackSetStatus
  = Active
  | Deleted
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StackSetStatus where
    parser = takeLowerText >>= \case
        "active" -> pure Active
        "deleted" -> pure Deleted
        e -> fromTextError $ "Failure parsing StackSetStatus from value: '" <> e
           <> "'. Accepted values: active, deleted"

instance ToText StackSetStatus where
    toText = \case
        Active -> "ACTIVE"
        Deleted -> "DELETED"

instance Hashable     StackSetStatus
instance NFData       StackSetStatus
instance ToByteString StackSetStatus
instance ToQuery      StackSetStatus
instance ToHeader     StackSetStatus

instance FromXML StackSetStatus where
    parseXML = parseXMLText "StackSetStatus"

data StackStatus
  = SSCreateComplete
  | SSCreateFailed
  | SSCreateInProgress
  | SSDeleteComplete
  | SSDeleteFailed
  | SSDeleteInProgress
  | SSReviewInProgress
  | SSRollbackComplete
  | SSRollbackFailed
  | SSRollbackInProgress
  | SSUpdateComplete
  | SSUpdateCompleteCleanupInProgress
  | SSUpdateInProgress
  | SSUpdateRollbackComplete
  | SSUpdateRollbackCompleteCleanupInProgress
  | SSUpdateRollbackFailed
  | SSUpdateRollbackInProgress
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StackStatus where
    parser = takeLowerText >>= \case
        "create_complete" -> pure SSCreateComplete
        "create_failed" -> pure SSCreateFailed
        "create_in_progress" -> pure SSCreateInProgress
        "delete_complete" -> pure SSDeleteComplete
        "delete_failed" -> pure SSDeleteFailed
        "delete_in_progress" -> pure SSDeleteInProgress
        "review_in_progress" -> pure SSReviewInProgress
        "rollback_complete" -> pure SSRollbackComplete
        "rollback_failed" -> pure SSRollbackFailed
        "rollback_in_progress" -> pure SSRollbackInProgress
        "update_complete" -> pure SSUpdateComplete
        "update_complete_cleanup_in_progress" -> pure SSUpdateCompleteCleanupInProgress
        "update_in_progress" -> pure SSUpdateInProgress
        "update_rollback_complete" -> pure SSUpdateRollbackComplete
        "update_rollback_complete_cleanup_in_progress" -> pure SSUpdateRollbackCompleteCleanupInProgress
        "update_rollback_failed" -> pure SSUpdateRollbackFailed
        "update_rollback_in_progress" -> pure SSUpdateRollbackInProgress
        e -> fromTextError $ "Failure parsing StackStatus from value: '" <> e
           <> "'. Accepted values: create_complete, create_failed, create_in_progress, delete_complete, delete_failed, delete_in_progress, review_in_progress, rollback_complete, rollback_failed, rollback_in_progress, update_complete, update_complete_cleanup_in_progress, update_in_progress, update_rollback_complete, update_rollback_complete_cleanup_in_progress, update_rollback_failed, update_rollback_in_progress"

instance ToText StackStatus where
    toText = \case
        SSCreateComplete -> "CREATE_COMPLETE"
        SSCreateFailed -> "CREATE_FAILED"
        SSCreateInProgress -> "CREATE_IN_PROGRESS"
        SSDeleteComplete -> "DELETE_COMPLETE"
        SSDeleteFailed -> "DELETE_FAILED"
        SSDeleteInProgress -> "DELETE_IN_PROGRESS"
        SSReviewInProgress -> "REVIEW_IN_PROGRESS"
        SSRollbackComplete -> "ROLLBACK_COMPLETE"
        SSRollbackFailed -> "ROLLBACK_FAILED"
        SSRollbackInProgress -> "ROLLBACK_IN_PROGRESS"
        SSUpdateComplete -> "UPDATE_COMPLETE"
        SSUpdateCompleteCleanupInProgress -> "UPDATE_COMPLETE_CLEANUP_IN_PROGRESS"
        SSUpdateInProgress -> "UPDATE_IN_PROGRESS"
        SSUpdateRollbackComplete -> "UPDATE_ROLLBACK_COMPLETE"
        SSUpdateRollbackCompleteCleanupInProgress -> "UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS"
        SSUpdateRollbackFailed -> "UPDATE_ROLLBACK_FAILED"
        SSUpdateRollbackInProgress -> "UPDATE_ROLLBACK_IN_PROGRESS"

instance Hashable     StackStatus
instance NFData       StackStatus
instance ToByteString StackStatus
instance ToQuery      StackStatus
instance ToHeader     StackStatus

instance FromXML StackStatus where
    parseXML = parseXMLText "StackStatus"

data TemplateStage
  = Original
  | Processed
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TemplateStage where
    parser = takeLowerText >>= \case
        "original" -> pure Original
        "processed" -> pure Processed
        e -> fromTextError $ "Failure parsing TemplateStage from value: '" <> e
           <> "'. Accepted values: original, processed"

instance ToText TemplateStage where
    toText = \case
        Original -> "Original"
        Processed -> "Processed"

instance Hashable     TemplateStage
instance NFData       TemplateStage
instance ToByteString TemplateStage
instance ToQuery      TemplateStage
instance ToHeader     TemplateStage

instance FromXML TemplateStage where
    parseXML = parseXMLText "TemplateStage"
