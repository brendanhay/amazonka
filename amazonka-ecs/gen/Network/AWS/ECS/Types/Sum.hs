{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Sum
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.Sum where

import           Network.AWS.Prelude

data AgentUpdateStatus
    = AUSFailed
    | AUSPending
    | AUSStaged
    | AUSStaging
    | AUSUpdated
    | AUSUpdating
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText AgentUpdateStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure AUSFailed
        "pending" -> pure AUSPending
        "staged" -> pure AUSStaged
        "staging" -> pure AUSStaging
        "updated" -> pure AUSUpdated
        "updating" -> pure AUSUpdating
        e -> fromTextError $ "Failure parsing AgentUpdateStatus from value: '" <> e
           <> "'. Accepted values: FAILED, PENDING, STAGED, STAGING, UPDATED, UPDATING"

instance ToText AgentUpdateStatus where
    toText = \case
        AUSFailed -> "FAILED"
        AUSPending -> "PENDING"
        AUSStaged -> "STAGED"
        AUSStaging -> "STAGING"
        AUSUpdated -> "UPDATED"
        AUSUpdating -> "UPDATING"

instance Hashable     AgentUpdateStatus
instance ToByteString AgentUpdateStatus
instance ToQuery      AgentUpdateStatus
instance ToHeader     AgentUpdateStatus

instance FromJSON AgentUpdateStatus where
    parseJSON = parseJSONText "AgentUpdateStatus"

data DesiredStatus
    = Pending
    | Running
    | Stopped
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText DesiredStatus where
    parser = takeLowerText >>= \case
        "pending" -> pure Pending
        "running" -> pure Running
        "stopped" -> pure Stopped
        e -> fromTextError $ "Failure parsing DesiredStatus from value: '" <> e
           <> "'. Accepted values: PENDING, RUNNING, STOPPED"

instance ToText DesiredStatus where
    toText = \case
        Pending -> "PENDING"
        Running -> "RUNNING"
        Stopped -> "STOPPED"

instance Hashable     DesiredStatus
instance ToByteString DesiredStatus
instance ToQuery      DesiredStatus
instance ToHeader     DesiredStatus

instance ToJSON DesiredStatus where
    toJSON = toJSONText

data SortOrder
    = Asc
    | Desc
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText SortOrder where
    parser = takeLowerText >>= \case
        "asc" -> pure Asc
        "desc" -> pure Desc
        e -> fromTextError $ "Failure parsing SortOrder from value: '" <> e
           <> "'. Accepted values: ASC, DESC"

instance ToText SortOrder where
    toText = \case
        Asc -> "ASC"
        Desc -> "DESC"

instance Hashable     SortOrder
instance ToByteString SortOrder
instance ToQuery      SortOrder
instance ToHeader     SortOrder

instance ToJSON SortOrder where
    toJSON = toJSONText

data TaskDefinitionStatus
    = Active
    | Inactive
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText TaskDefinitionStatus where
    parser = takeLowerText >>= \case
        "active" -> pure Active
        "inactive" -> pure Inactive
        e -> fromTextError $ "Failure parsing TaskDefinitionStatus from value: '" <> e
           <> "'. Accepted values: ACTIVE, INACTIVE"

instance ToText TaskDefinitionStatus where
    toText = \case
        Active -> "ACTIVE"
        Inactive -> "INACTIVE"

instance Hashable     TaskDefinitionStatus
instance ToByteString TaskDefinitionStatus
instance ToQuery      TaskDefinitionStatus
instance ToHeader     TaskDefinitionStatus

instance ToJSON TaskDefinitionStatus where
    toJSON = toJSONText

instance FromJSON TaskDefinitionStatus where
    parseJSON = parseJSONText "TaskDefinitionStatus"

data TransportProtocol
    = TCP
    | Udp
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText TransportProtocol where
    parser = takeLowerText >>= \case
        "tcp" -> pure TCP
        "udp" -> pure Udp
        e -> fromTextError $ "Failure parsing TransportProtocol from value: '" <> e
           <> "'. Accepted values: tcp, udp"

instance ToText TransportProtocol where
    toText = \case
        TCP -> "tcp"
        Udp -> "udp"

instance Hashable     TransportProtocol
instance ToByteString TransportProtocol
instance ToQuery      TransportProtocol
instance ToHeader     TransportProtocol

instance ToJSON TransportProtocol where
    toJSON = toJSONText

instance FromJSON TransportProtocol where
    parseJSON = parseJSONText "TransportProtocol"
