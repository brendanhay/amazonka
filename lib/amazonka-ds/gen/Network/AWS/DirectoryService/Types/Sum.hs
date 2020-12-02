{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectoryService.Types.Sum where

import Network.AWS.Prelude

data DirectoryEdition
  = Enterprise
  | Standard
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DirectoryEdition where
    parser = takeLowerText >>= \case
        "enterprise" -> pure Enterprise
        "standard" -> pure Standard
        e -> fromTextError $ "Failure parsing DirectoryEdition from value: '" <> e
           <> "'. Accepted values: enterprise, standard"

instance ToText DirectoryEdition where
    toText = \case
        Enterprise -> "Enterprise"
        Standard -> "Standard"

instance Hashable     DirectoryEdition
instance NFData       DirectoryEdition
instance ToByteString DirectoryEdition
instance ToQuery      DirectoryEdition
instance ToHeader     DirectoryEdition

instance ToJSON DirectoryEdition where
    toJSON = toJSONText

instance FromJSON DirectoryEdition where
    parseJSON = parseJSONText "DirectoryEdition"

data DirectorySize
  = Large
  | Small
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DirectorySize where
    parser = takeLowerText >>= \case
        "large" -> pure Large
        "small" -> pure Small
        e -> fromTextError $ "Failure parsing DirectorySize from value: '" <> e
           <> "'. Accepted values: large, small"

instance ToText DirectorySize where
    toText = \case
        Large -> "Large"
        Small -> "Small"

instance Hashable     DirectorySize
instance NFData       DirectorySize
instance ToByteString DirectorySize
instance ToQuery      DirectorySize
instance ToHeader     DirectorySize

instance ToJSON DirectorySize where
    toJSON = toJSONText

instance FromJSON DirectorySize where
    parseJSON = parseJSONText "DirectorySize"

data DirectoryStage
  = DSActive
  | DSCreated
  | DSCreating
  | DSDeleted
  | DSDeleting
  | DSFailed
  | DSImpaired
  | DSInoperable
  | DSRequested
  | DSRestoreFailed
  | DSRestoring
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DirectoryStage where
    parser = takeLowerText >>= \case
        "active" -> pure DSActive
        "created" -> pure DSCreated
        "creating" -> pure DSCreating
        "deleted" -> pure DSDeleted
        "deleting" -> pure DSDeleting
        "failed" -> pure DSFailed
        "impaired" -> pure DSImpaired
        "inoperable" -> pure DSInoperable
        "requested" -> pure DSRequested
        "restorefailed" -> pure DSRestoreFailed
        "restoring" -> pure DSRestoring
        e -> fromTextError $ "Failure parsing DirectoryStage from value: '" <> e
           <> "'. Accepted values: active, created, creating, deleted, deleting, failed, impaired, inoperable, requested, restorefailed, restoring"

instance ToText DirectoryStage where
    toText = \case
        DSActive -> "Active"
        DSCreated -> "Created"
        DSCreating -> "Creating"
        DSDeleted -> "Deleted"
        DSDeleting -> "Deleting"
        DSFailed -> "Failed"
        DSImpaired -> "Impaired"
        DSInoperable -> "Inoperable"
        DSRequested -> "Requested"
        DSRestoreFailed -> "RestoreFailed"
        DSRestoring -> "Restoring"

instance Hashable     DirectoryStage
instance NFData       DirectoryStage
instance ToByteString DirectoryStage
instance ToQuery      DirectoryStage
instance ToHeader     DirectoryStage

instance FromJSON DirectoryStage where
    parseJSON = parseJSONText "DirectoryStage"

data DirectoryType
  = ADConnector
  | MicrosoftAD
  | SimpleAD
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DirectoryType where
    parser = takeLowerText >>= \case
        "adconnector" -> pure ADConnector
        "microsoftad" -> pure MicrosoftAD
        "simplead" -> pure SimpleAD
        e -> fromTextError $ "Failure parsing DirectoryType from value: '" <> e
           <> "'. Accepted values: adconnector, microsoftad, simplead"

instance ToText DirectoryType where
    toText = \case
        ADConnector -> "ADConnector"
        MicrosoftAD -> "MicrosoftAD"
        SimpleAD -> "SimpleAD"

instance Hashable     DirectoryType
instance NFData       DirectoryType
instance ToByteString DirectoryType
instance ToQuery      DirectoryType
instance ToHeader     DirectoryType

instance FromJSON DirectoryType where
    parseJSON = parseJSONText "DirectoryType"

data DomainControllerStatus
  = Active
  | Creating
  | Deleted
  | Deleting
  | Failed
  | Impaired
  | Restoring
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DomainControllerStatus where
    parser = takeLowerText >>= \case
        "active" -> pure Active
        "creating" -> pure Creating
        "deleted" -> pure Deleted
        "deleting" -> pure Deleting
        "failed" -> pure Failed
        "impaired" -> pure Impaired
        "restoring" -> pure Restoring
        e -> fromTextError $ "Failure parsing DomainControllerStatus from value: '" <> e
           <> "'. Accepted values: active, creating, deleted, deleting, failed, impaired, restoring"

instance ToText DomainControllerStatus where
    toText = \case
        Active -> "Active"
        Creating -> "Creating"
        Deleted -> "Deleted"
        Deleting -> "Deleting"
        Failed -> "Failed"
        Impaired -> "Impaired"
        Restoring -> "Restoring"

instance Hashable     DomainControllerStatus
instance NFData       DomainControllerStatus
instance ToByteString DomainControllerStatus
instance ToQuery      DomainControllerStatus
instance ToHeader     DomainControllerStatus

instance FromJSON DomainControllerStatus where
    parseJSON = parseJSONText "DomainControllerStatus"

data IPRouteStatusMsg
  = AddFailed
  | Added
  | Adding
  | RemoveFailed
  | Removed
  | Removing
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText IPRouteStatusMsg where
    parser = takeLowerText >>= \case
        "addfailed" -> pure AddFailed
        "added" -> pure Added
        "adding" -> pure Adding
        "removefailed" -> pure RemoveFailed
        "removed" -> pure Removed
        "removing" -> pure Removing
        e -> fromTextError $ "Failure parsing IPRouteStatusMsg from value: '" <> e
           <> "'. Accepted values: addfailed, added, adding, removefailed, removed, removing"

instance ToText IPRouteStatusMsg where
    toText = \case
        AddFailed -> "AddFailed"
        Added -> "Added"
        Adding -> "Adding"
        RemoveFailed -> "RemoveFailed"
        Removed -> "Removed"
        Removing -> "Removing"

instance Hashable     IPRouteStatusMsg
instance NFData       IPRouteStatusMsg
instance ToByteString IPRouteStatusMsg
instance ToQuery      IPRouteStatusMsg
instance ToHeader     IPRouteStatusMsg

instance FromJSON IPRouteStatusMsg where
    parseJSON = parseJSONText "IPRouteStatusMsg"

data RadiusAuthenticationProtocol
  = Chap
  | MsCHAPV1
  | MsCHAPV2
  | Pap
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RadiusAuthenticationProtocol where
    parser = takeLowerText >>= \case
        "chap" -> pure Chap
        "ms-chapv1" -> pure MsCHAPV1
        "ms-chapv2" -> pure MsCHAPV2
        "pap" -> pure Pap
        e -> fromTextError $ "Failure parsing RadiusAuthenticationProtocol from value: '" <> e
           <> "'. Accepted values: chap, ms-chapv1, ms-chapv2, pap"

instance ToText RadiusAuthenticationProtocol where
    toText = \case
        Chap -> "CHAP"
        MsCHAPV1 -> "MS-CHAPv1"
        MsCHAPV2 -> "MS-CHAPv2"
        Pap -> "PAP"

instance Hashable     RadiusAuthenticationProtocol
instance NFData       RadiusAuthenticationProtocol
instance ToByteString RadiusAuthenticationProtocol
instance ToQuery      RadiusAuthenticationProtocol
instance ToHeader     RadiusAuthenticationProtocol

instance ToJSON RadiusAuthenticationProtocol where
    toJSON = toJSONText

instance FromJSON RadiusAuthenticationProtocol where
    parseJSON = parseJSONText "RadiusAuthenticationProtocol"

data RadiusStatus
  = RSCompleted
  | RSCreating
  | RSFailed
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RadiusStatus where
    parser = takeLowerText >>= \case
        "completed" -> pure RSCompleted
        "creating" -> pure RSCreating
        "failed" -> pure RSFailed
        e -> fromTextError $ "Failure parsing RadiusStatus from value: '" <> e
           <> "'. Accepted values: completed, creating, failed"

instance ToText RadiusStatus where
    toText = \case
        RSCompleted -> "Completed"
        RSCreating -> "Creating"
        RSFailed -> "Failed"

instance Hashable     RadiusStatus
instance NFData       RadiusStatus
instance ToByteString RadiusStatus
instance ToQuery      RadiusStatus
instance ToHeader     RadiusStatus

instance FromJSON RadiusStatus where
    parseJSON = parseJSONText "RadiusStatus"

data ReplicationScope =
  Domain
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ReplicationScope where
    parser = takeLowerText >>= \case
        "domain" -> pure Domain
        e -> fromTextError $ "Failure parsing ReplicationScope from value: '" <> e
           <> "'. Accepted values: domain"

instance ToText ReplicationScope where
    toText = \case
        Domain -> "Domain"

instance Hashable     ReplicationScope
instance NFData       ReplicationScope
instance ToByteString ReplicationScope
instance ToQuery      ReplicationScope
instance ToHeader     ReplicationScope

instance FromJSON ReplicationScope where
    parseJSON = parseJSONText "ReplicationScope"

data SchemaExtensionStatus
  = SESCancelInProgress
  | SESCancelled
  | SESCompleted
  | SESCreatingSnapshot
  | SESFailed
  | SESInitializing
  | SESReplicating
  | SESRollbackInProgress
  | SESUpdatingSchema
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SchemaExtensionStatus where
    parser = takeLowerText >>= \case
        "cancelinprogress" -> pure SESCancelInProgress
        "cancelled" -> pure SESCancelled
        "completed" -> pure SESCompleted
        "creatingsnapshot" -> pure SESCreatingSnapshot
        "failed" -> pure SESFailed
        "initializing" -> pure SESInitializing
        "replicating" -> pure SESReplicating
        "rollbackinprogress" -> pure SESRollbackInProgress
        "updatingschema" -> pure SESUpdatingSchema
        e -> fromTextError $ "Failure parsing SchemaExtensionStatus from value: '" <> e
           <> "'. Accepted values: cancelinprogress, cancelled, completed, creatingsnapshot, failed, initializing, replicating, rollbackinprogress, updatingschema"

instance ToText SchemaExtensionStatus where
    toText = \case
        SESCancelInProgress -> "CancelInProgress"
        SESCancelled -> "Cancelled"
        SESCompleted -> "Completed"
        SESCreatingSnapshot -> "CreatingSnapshot"
        SESFailed -> "Failed"
        SESInitializing -> "Initializing"
        SESReplicating -> "Replicating"
        SESRollbackInProgress -> "RollbackInProgress"
        SESUpdatingSchema -> "UpdatingSchema"

instance Hashable     SchemaExtensionStatus
instance NFData       SchemaExtensionStatus
instance ToByteString SchemaExtensionStatus
instance ToQuery      SchemaExtensionStatus
instance ToHeader     SchemaExtensionStatus

instance FromJSON SchemaExtensionStatus where
    parseJSON = parseJSONText "SchemaExtensionStatus"

data SnapshotStatus
  = SSCompleted
  | SSCreating
  | SSFailed
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SnapshotStatus where
    parser = takeLowerText >>= \case
        "completed" -> pure SSCompleted
        "creating" -> pure SSCreating
        "failed" -> pure SSFailed
        e -> fromTextError $ "Failure parsing SnapshotStatus from value: '" <> e
           <> "'. Accepted values: completed, creating, failed"

instance ToText SnapshotStatus where
    toText = \case
        SSCompleted -> "Completed"
        SSCreating -> "Creating"
        SSFailed -> "Failed"

instance Hashable     SnapshotStatus
instance NFData       SnapshotStatus
instance ToByteString SnapshotStatus
instance ToQuery      SnapshotStatus
instance ToHeader     SnapshotStatus

instance FromJSON SnapshotStatus where
    parseJSON = parseJSONText "SnapshotStatus"

data SnapshotType
  = Auto
  | Manual
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SnapshotType where
    parser = takeLowerText >>= \case
        "auto" -> pure Auto
        "manual" -> pure Manual
        e -> fromTextError $ "Failure parsing SnapshotType from value: '" <> e
           <> "'. Accepted values: auto, manual"

instance ToText SnapshotType where
    toText = \case
        Auto -> "Auto"
        Manual -> "Manual"

instance Hashable     SnapshotType
instance NFData       SnapshotType
instance ToByteString SnapshotType
instance ToQuery      SnapshotType
instance ToHeader     SnapshotType

instance FromJSON SnapshotType where
    parseJSON = parseJSONText "SnapshotType"

data TopicStatus
  = TDeleted
  | TFailed
  | TRegistered
  | TTopicNotFound
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TopicStatus where
    parser = takeLowerText >>= \case
        "deleted" -> pure TDeleted
        "failed" -> pure TFailed
        "registered" -> pure TRegistered
        "topic not found" -> pure TTopicNotFound
        e -> fromTextError $ "Failure parsing TopicStatus from value: '" <> e
           <> "'. Accepted values: deleted, failed, registered, topic not found"

instance ToText TopicStatus where
    toText = \case
        TDeleted -> "Deleted"
        TFailed -> "Failed"
        TRegistered -> "Registered"
        TTopicNotFound -> "Topic not found"

instance Hashable     TopicStatus
instance NFData       TopicStatus
instance ToByteString TopicStatus
instance ToQuery      TopicStatus
instance ToHeader     TopicStatus

instance FromJSON TopicStatus where
    parseJSON = parseJSONText "TopicStatus"

data TrustDirection
  = OneWayIncoming
  | OneWayOutgoing
  | TwoWay
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TrustDirection where
    parser = takeLowerText >>= \case
        "one-way: incoming" -> pure OneWayIncoming
        "one-way: outgoing" -> pure OneWayOutgoing
        "two-way" -> pure TwoWay
        e -> fromTextError $ "Failure parsing TrustDirection from value: '" <> e
           <> "'. Accepted values: one-way: incoming, one-way: outgoing, two-way"

instance ToText TrustDirection where
    toText = \case
        OneWayIncoming -> "One-Way: Incoming"
        OneWayOutgoing -> "One-Way: Outgoing"
        TwoWay -> "Two-Way"

instance Hashable     TrustDirection
instance NFData       TrustDirection
instance ToByteString TrustDirection
instance ToQuery      TrustDirection
instance ToHeader     TrustDirection

instance ToJSON TrustDirection where
    toJSON = toJSONText

instance FromJSON TrustDirection where
    parseJSON = parseJSONText "TrustDirection"

data TrustState
  = TSCreated
  | TSCreating
  | TSDeleted
  | TSDeleting
  | TSFailed
  | TSVerified
  | TSVerifyFailed
  | TSVerifying
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TrustState where
    parser = takeLowerText >>= \case
        "created" -> pure TSCreated
        "creating" -> pure TSCreating
        "deleted" -> pure TSDeleted
        "deleting" -> pure TSDeleting
        "failed" -> pure TSFailed
        "verified" -> pure TSVerified
        "verifyfailed" -> pure TSVerifyFailed
        "verifying" -> pure TSVerifying
        e -> fromTextError $ "Failure parsing TrustState from value: '" <> e
           <> "'. Accepted values: created, creating, deleted, deleting, failed, verified, verifyfailed, verifying"

instance ToText TrustState where
    toText = \case
        TSCreated -> "Created"
        TSCreating -> "Creating"
        TSDeleted -> "Deleted"
        TSDeleting -> "Deleting"
        TSFailed -> "Failed"
        TSVerified -> "Verified"
        TSVerifyFailed -> "VerifyFailed"
        TSVerifying -> "Verifying"

instance Hashable     TrustState
instance NFData       TrustState
instance ToByteString TrustState
instance ToQuery      TrustState
instance ToHeader     TrustState

instance FromJSON TrustState where
    parseJSON = parseJSONText "TrustState"

data TrustType =
  Forest
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TrustType where
    parser = takeLowerText >>= \case
        "forest" -> pure Forest
        e -> fromTextError $ "Failure parsing TrustType from value: '" <> e
           <> "'. Accepted values: forest"

instance ToText TrustType where
    toText = \case
        Forest -> "Forest"

instance Hashable     TrustType
instance NFData       TrustType
instance ToByteString TrustType
instance ToQuery      TrustType
instance ToHeader     TrustType

instance ToJSON TrustType where
    toJSON = toJSONText

instance FromJSON TrustType where
    parseJSON = parseJSONText "TrustType"
