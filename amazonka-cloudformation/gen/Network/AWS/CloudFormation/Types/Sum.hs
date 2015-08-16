{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.Sum
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.Sum where

import           Network.AWS.Prelude

data Capability =
    CapabilityIAM
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText Capability where
    parser = takeLowerText >>= \case
        "capability_iam" -> pure CapabilityIAM
        e -> fromTextError $ "Failure parsing Capability from value: '" <> e
           <> "'. Accepted values: capability_iam"

instance ToText Capability where
    toText = \case
        CapabilityIAM -> "capability_iam"

instance Hashable     Capability
instance ToByteString Capability
instance ToQuery      Capability
instance ToHeader     Capability

instance FromXML Capability where
    parseXML = parseXMLText "Capability"

data OnFailure
    = Delete
    | DoNothing
    | Rollback
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText OnFailure where
    parser = takeLowerText >>= \case
        "delete" -> pure Delete
        "do_nothing" -> pure DoNothing
        "rollback" -> pure Rollback
        e -> fromTextError $ "Failure parsing OnFailure from value: '" <> e
           <> "'. Accepted values: delete, do_nothing, rollback"

instance ToText OnFailure where
    toText = \case
        Delete -> "delete"
        DoNothing -> "do_nothing"
        Rollback -> "rollback"

instance Hashable     OnFailure
instance ToByteString OnFailure
instance ToQuery      OnFailure
instance ToHeader     OnFailure

data ResourceSignalStatus
    = Failure
    | Success
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ResourceSignalStatus where
    parser = takeLowerText >>= \case
        "failure" -> pure Failure
        "success" -> pure Success
        e -> fromTextError $ "Failure parsing ResourceSignalStatus from value: '" <> e
           <> "'. Accepted values: failure, success"

instance ToText ResourceSignalStatus where
    toText = \case
        Failure -> "failure"
        Success -> "success"

instance Hashable     ResourceSignalStatus
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
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

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
        CreateComplete -> "create_complete"
        CreateFailed -> "create_failed"
        CreateInProgress -> "create_in_progress"
        DeleteComplete -> "delete_complete"
        DeleteFailed -> "delete_failed"
        DeleteInProgress -> "delete_in_progress"
        DeleteSkipped -> "delete_skipped"
        UpdateComplete -> "update_complete"
        UpdateFailed -> "update_failed"
        UpdateInProgress -> "update_in_progress"

instance Hashable     ResourceStatus
instance ToByteString ResourceStatus
instance ToQuery      ResourceStatus
instance ToHeader     ResourceStatus

instance FromXML ResourceStatus where
    parseXML = parseXMLText "ResourceStatus"

data StackStatus
    = SSCreateComplete
    | SSCreateFailed
    | SSCreateInProgress
    | SSDeleteComplete
    | SSDeleteFailed
    | SSDeleteInProgress
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
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText StackStatus where
    parser = takeLowerText >>= \case
        "create_complete" -> pure SSCreateComplete
        "create_failed" -> pure SSCreateFailed
        "create_in_progress" -> pure SSCreateInProgress
        "delete_complete" -> pure SSDeleteComplete
        "delete_failed" -> pure SSDeleteFailed
        "delete_in_progress" -> pure SSDeleteInProgress
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
           <> "'. Accepted values: create_complete, create_failed, create_in_progress, delete_complete, delete_failed, delete_in_progress, rollback_complete, rollback_failed, rollback_in_progress, update_complete, update_complete_cleanup_in_progress, update_in_progress, update_rollback_complete, update_rollback_complete_cleanup_in_progress, update_rollback_failed, update_rollback_in_progress"

instance ToText StackStatus where
    toText = \case
        SSCreateComplete -> "create_complete"
        SSCreateFailed -> "create_failed"
        SSCreateInProgress -> "create_in_progress"
        SSDeleteComplete -> "delete_complete"
        SSDeleteFailed -> "delete_failed"
        SSDeleteInProgress -> "delete_in_progress"
        SSRollbackComplete -> "rollback_complete"
        SSRollbackFailed -> "rollback_failed"
        SSRollbackInProgress -> "rollback_in_progress"
        SSUpdateComplete -> "update_complete"
        SSUpdateCompleteCleanupInProgress -> "update_complete_cleanup_in_progress"
        SSUpdateInProgress -> "update_in_progress"
        SSUpdateRollbackComplete -> "update_rollback_complete"
        SSUpdateRollbackCompleteCleanupInProgress -> "update_rollback_complete_cleanup_in_progress"
        SSUpdateRollbackFailed -> "update_rollback_failed"
        SSUpdateRollbackInProgress -> "update_rollback_in_progress"

instance Hashable     StackStatus
instance ToByteString StackStatus
instance ToQuery      StackStatus
instance ToHeader     StackStatus

instance FromXML StackStatus where
    parseXML = parseXMLText "StackStatus"
