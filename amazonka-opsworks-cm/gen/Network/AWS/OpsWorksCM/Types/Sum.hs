{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorksCM.Types.Sum where

import Network.AWS.Prelude

data BackupStatus
  = BSDeleting
  | BSFailed
  | BSInProgress
  | BSOK
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BackupStatus where
    parser = takeLowerText >>= \case
        "deleting" -> pure BSDeleting
        "failed" -> pure BSFailed
        "in_progress" -> pure BSInProgress
        "ok" -> pure BSOK
        e -> fromTextError $ "Failure parsing BackupStatus from value: '" <> e
           <> "'. Accepted values: deleting, failed, in_progress, ok"

instance ToText BackupStatus where
    toText = \case
        BSDeleting -> "DELETING"
        BSFailed -> "FAILED"
        BSInProgress -> "IN_PROGRESS"
        BSOK -> "OK"

instance Hashable     BackupStatus
instance NFData       BackupStatus
instance ToByteString BackupStatus
instance ToQuery      BackupStatus
instance ToHeader     BackupStatus

instance FromJSON BackupStatus where
    parseJSON = parseJSONText "BackupStatus"

data BackupType
  = Automated
  | Manual
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BackupType where
    parser = takeLowerText >>= \case
        "automated" -> pure Automated
        "manual" -> pure Manual
        e -> fromTextError $ "Failure parsing BackupType from value: '" <> e
           <> "'. Accepted values: automated, manual"

instance ToText BackupType where
    toText = \case
        Automated -> "AUTOMATED"
        Manual -> "MANUAL"

instance Hashable     BackupType
instance NFData       BackupType
instance ToByteString BackupType
instance ToQuery      BackupType
instance ToHeader     BackupType

instance FromJSON BackupType where
    parseJSON = parseJSONText "BackupType"

data MaintenanceStatus
  = MSFailed
  | MSSuccess
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MaintenanceStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure MSFailed
        "success" -> pure MSSuccess
        e -> fromTextError $ "Failure parsing MaintenanceStatus from value: '" <> e
           <> "'. Accepted values: failed, success"

instance ToText MaintenanceStatus where
    toText = \case
        MSFailed -> "FAILED"
        MSSuccess -> "SUCCESS"

instance Hashable     MaintenanceStatus
instance NFData       MaintenanceStatus
instance ToByteString MaintenanceStatus
instance ToQuery      MaintenanceStatus
instance ToHeader     MaintenanceStatus

instance FromJSON MaintenanceStatus where
    parseJSON = parseJSONText "MaintenanceStatus"

-- | The status of the association or disassociation request.
--
--
-- __Possible values:__
--
--     * @SUCCESS@ : The association or disassociation succeeded.
--
--     * @FAILED@ : The association or disassociation failed.
--
--     * @IN_PROGRESS@ : The association or disassociation is still in progress.
--
--
--
data NodeAssociationStatus
  = NASFailed
  | NASInProgress
  | NASSuccess
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText NodeAssociationStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure NASFailed
        "in_progress" -> pure NASInProgress
        "success" -> pure NASSuccess
        e -> fromTextError $ "Failure parsing NodeAssociationStatus from value: '" <> e
           <> "'. Accepted values: failed, in_progress, success"

instance ToText NodeAssociationStatus where
    toText = \case
        NASFailed -> "FAILED"
        NASInProgress -> "IN_PROGRESS"
        NASSuccess -> "SUCCESS"

instance Hashable     NodeAssociationStatus
instance NFData       NodeAssociationStatus
instance ToByteString NodeAssociationStatus
instance ToQuery      NodeAssociationStatus
instance ToHeader     NodeAssociationStatus

instance FromJSON NodeAssociationStatus where
    parseJSON = parseJSONText "NodeAssociationStatus"

data ServerStatus
  = BackingUp
  | ConnectionLost
  | Creating
  | Deleting
  | Failed
  | Healthy
  | Modifying
  | Restoring
  | Running
  | Setup
  | Terminated
  | UnderMaintenance
  | Unhealthy
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ServerStatus where
    parser = takeLowerText >>= \case
        "backing_up" -> pure BackingUp
        "connection_lost" -> pure ConnectionLost
        "creating" -> pure Creating
        "deleting" -> pure Deleting
        "failed" -> pure Failed
        "healthy" -> pure Healthy
        "modifying" -> pure Modifying
        "restoring" -> pure Restoring
        "running" -> pure Running
        "setup" -> pure Setup
        "terminated" -> pure Terminated
        "under_maintenance" -> pure UnderMaintenance
        "unhealthy" -> pure Unhealthy
        e -> fromTextError $ "Failure parsing ServerStatus from value: '" <> e
           <> "'. Accepted values: backing_up, connection_lost, creating, deleting, failed, healthy, modifying, restoring, running, setup, terminated, under_maintenance, unhealthy"

instance ToText ServerStatus where
    toText = \case
        BackingUp -> "BACKING_UP"
        ConnectionLost -> "CONNECTION_LOST"
        Creating -> "CREATING"
        Deleting -> "DELETING"
        Failed -> "FAILED"
        Healthy -> "HEALTHY"
        Modifying -> "MODIFYING"
        Restoring -> "RESTORING"
        Running -> "RUNNING"
        Setup -> "SETUP"
        Terminated -> "TERMINATED"
        UnderMaintenance -> "UNDER_MAINTENANCE"
        Unhealthy -> "UNHEALTHY"

instance Hashable     ServerStatus
instance NFData       ServerStatus
instance ToByteString ServerStatus
instance ToQuery      ServerStatus
instance ToHeader     ServerStatus

instance FromJSON ServerStatus where
    parseJSON = parseJSONText "ServerStatus"
