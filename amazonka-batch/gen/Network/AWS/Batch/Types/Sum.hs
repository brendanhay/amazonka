{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Batch.Types.Sum where

import Network.AWS.Prelude

data ArrayJobDependency
  = NToN
  | Sequential
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ArrayJobDependency where
    parser = takeLowerText >>= \case
        "n_to_n" -> pure NToN
        "sequential" -> pure Sequential
        e -> fromTextError $ "Failure parsing ArrayJobDependency from value: '" <> e
           <> "'. Accepted values: n_to_n, sequential"

instance ToText ArrayJobDependency where
    toText = \case
        NToN -> "N_TO_N"
        Sequential -> "SEQUENTIAL"

instance Hashable     ArrayJobDependency
instance NFData       ArrayJobDependency
instance ToByteString ArrayJobDependency
instance ToQuery      ArrayJobDependency
instance ToHeader     ArrayJobDependency

instance ToJSON ArrayJobDependency where
    toJSON = toJSONText

instance FromJSON ArrayJobDependency where
    parseJSON = parseJSONText "ArrayJobDependency"

data CEState
  = Disabled
  | Enabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CEState where
    parser = takeLowerText >>= \case
        "disabled" -> pure Disabled
        "enabled" -> pure Enabled
        e -> fromTextError $ "Failure parsing CEState from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText CEState where
    toText = \case
        Disabled -> "DISABLED"
        Enabled -> "ENABLED"

instance Hashable     CEState
instance NFData       CEState
instance ToByteString CEState
instance ToQuery      CEState
instance ToHeader     CEState

instance ToJSON CEState where
    toJSON = toJSONText

instance FromJSON CEState where
    parseJSON = parseJSONText "CEState"

data CEStatus
  = CESCreating
  | CESDeleted
  | CESDeleting
  | CESInvalid
  | CESUpdating
  | CESValid
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CEStatus where
    parser = takeLowerText >>= \case
        "creating" -> pure CESCreating
        "deleted" -> pure CESDeleted
        "deleting" -> pure CESDeleting
        "invalid" -> pure CESInvalid
        "updating" -> pure CESUpdating
        "valid" -> pure CESValid
        e -> fromTextError $ "Failure parsing CEStatus from value: '" <> e
           <> "'. Accepted values: creating, deleted, deleting, invalid, updating, valid"

instance ToText CEStatus where
    toText = \case
        CESCreating -> "CREATING"
        CESDeleted -> "DELETED"
        CESDeleting -> "DELETING"
        CESInvalid -> "INVALID"
        CESUpdating -> "UPDATING"
        CESValid -> "VALID"

instance Hashable     CEStatus
instance NFData       CEStatus
instance ToByteString CEStatus
instance ToQuery      CEStatus
instance ToHeader     CEStatus

instance FromJSON CEStatus where
    parseJSON = parseJSONText "CEStatus"

data CEType
  = Managed
  | Unmanaged
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CEType where
    parser = takeLowerText >>= \case
        "managed" -> pure Managed
        "unmanaged" -> pure Unmanaged
        e -> fromTextError $ "Failure parsing CEType from value: '" <> e
           <> "'. Accepted values: managed, unmanaged"

instance ToText CEType where
    toText = \case
        Managed -> "MANAGED"
        Unmanaged -> "UNMANAGED"

instance Hashable     CEType
instance NFData       CEType
instance ToByteString CEType
instance ToQuery      CEType
instance ToHeader     CEType

instance ToJSON CEType where
    toJSON = toJSONText

instance FromJSON CEType where
    parseJSON = parseJSONText "CEType"

data CRType
  = EC2
  | Spot
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CRType where
    parser = takeLowerText >>= \case
        "ec2" -> pure EC2
        "spot" -> pure Spot
        e -> fromTextError $ "Failure parsing CRType from value: '" <> e
           <> "'. Accepted values: ec2, spot"

instance ToText CRType where
    toText = \case
        EC2 -> "EC2"
        Spot -> "SPOT"

instance Hashable     CRType
instance NFData       CRType
instance ToByteString CRType
instance ToQuery      CRType
instance ToHeader     CRType

instance ToJSON CRType where
    toJSON = toJSONText

instance FromJSON CRType where
    parseJSON = parseJSONText "CRType"

data JQState
  = JQSDisabled
  | JQSEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText JQState where
    parser = takeLowerText >>= \case
        "disabled" -> pure JQSDisabled
        "enabled" -> pure JQSEnabled
        e -> fromTextError $ "Failure parsing JQState from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText JQState where
    toText = \case
        JQSDisabled -> "DISABLED"
        JQSEnabled -> "ENABLED"

instance Hashable     JQState
instance NFData       JQState
instance ToByteString JQState
instance ToQuery      JQState
instance ToHeader     JQState

instance ToJSON JQState where
    toJSON = toJSONText

instance FromJSON JQState where
    parseJSON = parseJSONText "JQState"

data JQStatus
  = Creating
  | Deleted
  | Deleting
  | Invalid
  | Updating
  | Valid
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText JQStatus where
    parser = takeLowerText >>= \case
        "creating" -> pure Creating
        "deleted" -> pure Deleted
        "deleting" -> pure Deleting
        "invalid" -> pure Invalid
        "updating" -> pure Updating
        "valid" -> pure Valid
        e -> fromTextError $ "Failure parsing JQStatus from value: '" <> e
           <> "'. Accepted values: creating, deleted, deleting, invalid, updating, valid"

instance ToText JQStatus where
    toText = \case
        Creating -> "CREATING"
        Deleted -> "DELETED"
        Deleting -> "DELETING"
        Invalid -> "INVALID"
        Updating -> "UPDATING"
        Valid -> "VALID"

instance Hashable     JQStatus
instance NFData       JQStatus
instance ToByteString JQStatus
instance ToQuery      JQStatus
instance ToHeader     JQStatus

instance FromJSON JQStatus where
    parseJSON = parseJSONText "JQStatus"

data JobDefinitionType =
  Container
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText JobDefinitionType where
    parser = takeLowerText >>= \case
        "container" -> pure Container
        e -> fromTextError $ "Failure parsing JobDefinitionType from value: '" <> e
           <> "'. Accepted values: container"

instance ToText JobDefinitionType where
    toText = \case
        Container -> "container"

instance Hashable     JobDefinitionType
instance NFData       JobDefinitionType
instance ToByteString JobDefinitionType
instance ToQuery      JobDefinitionType
instance ToHeader     JobDefinitionType

instance ToJSON JobDefinitionType where
    toJSON = toJSONText

data JobStatus
  = Failed
  | Pending
  | Runnable
  | Running
  | Starting
  | Submitted
  | Succeeded
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText JobStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure Failed
        "pending" -> pure Pending
        "runnable" -> pure Runnable
        "running" -> pure Running
        "starting" -> pure Starting
        "submitted" -> pure Submitted
        "succeeded" -> pure Succeeded
        e -> fromTextError $ "Failure parsing JobStatus from value: '" <> e
           <> "'. Accepted values: failed, pending, runnable, running, starting, submitted, succeeded"

instance ToText JobStatus where
    toText = \case
        Failed -> "FAILED"
        Pending -> "PENDING"
        Runnable -> "RUNNABLE"
        Running -> "RUNNING"
        Starting -> "STARTING"
        Submitted -> "SUBMITTED"
        Succeeded -> "SUCCEEDED"

instance Hashable     JobStatus
instance NFData       JobStatus
instance ToByteString JobStatus
instance ToQuery      JobStatus
instance ToHeader     JobStatus

instance ToJSON JobStatus where
    toJSON = toJSONText

instance FromJSON JobStatus where
    parseJSON = parseJSONText "JobStatus"
