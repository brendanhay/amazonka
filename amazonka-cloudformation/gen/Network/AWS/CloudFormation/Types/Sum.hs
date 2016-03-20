{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.Sum
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.Sum where

import           Network.AWS.Prelude

data Capability =
    CapabilityIAM
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText Capability where
    parser = takeLowerText >>= \case
        "capability_iam" -> pure CapabilityIAM
        e -> fromTextError $ "Failure parsing Capability from value: '" <> e
           <> "'. Accepted values: CAPABILITY_IAM"

instance ToText Capability where
    toText = \case
        CapabilityIAM -> "CAPABILITY_IAM"

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
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText OnFailure where
    parser = takeLowerText >>= \case
        "delete" -> pure Delete
        "do_nothing" -> pure DoNothing
        "rollback" -> pure Rollback
        e -> fromTextError $ "Failure parsing OnFailure from value: '" <> e
           <> "'. Accepted values: DELETE, DO_NOTHING, ROLLBACK"

instance ToText OnFailure where
    toText = \case
        Delete -> "DELETE"
        DoNothing -> "DO_NOTHING"
        Rollback -> "ROLLBACK"

instance Hashable     OnFailure
instance ToByteString OnFailure
instance ToQuery      OnFailure
instance ToHeader     OnFailure

data ResourceSignalStatus
    = Failure
    | Success
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText ResourceSignalStatus where
    parser = takeLowerText >>= \case
        "failure" -> pure Failure
        "success" -> pure Success
        e -> fromTextError $ "Failure parsing ResourceSignalStatus from value: '" <> e
           <> "'. Accepted values: FAILURE, SUCCESS"

instance ToText ResourceSignalStatus where
    toText = \case
        Failure -> "FAILURE"
        Success -> "SUCCESS"

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
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

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
           <> "'. Accepted values: CREATE_COMPLETE, CREATE_FAILED, CREATE_IN_PROGRESS, DELETE_COMPLETE, DELETE_FAILED, DELETE_IN_PROGRESS, DELETE_SKIPPED, UPDATE_COMPLETE, UPDATE_FAILED, UPDATE_IN_PROGRESS"

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
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

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
           <> "'. Accepted values: CREATE_COMPLETE, CREATE_FAILED, CREATE_IN_PROGRESS, DELETE_COMPLETE, DELETE_FAILED, DELETE_IN_PROGRESS, ROLLBACK_COMPLETE, ROLLBACK_FAILED, ROLLBACK_IN_PROGRESS, UPDATE_COMPLETE, UPDATE_COMPLETE_CLEANUP_IN_PROGRESS, UPDATE_IN_PROGRESS, UPDATE_ROLLBACK_COMPLETE, UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS, UPDATE_ROLLBACK_FAILED, UPDATE_ROLLBACK_IN_PROGRESS"

instance ToText StackStatus where
    toText = \case
        SSCreateComplete -> "CREATE_COMPLETE"
        SSCreateFailed -> "CREATE_FAILED"
        SSCreateInProgress -> "CREATE_IN_PROGRESS"
        SSDeleteComplete -> "DELETE_COMPLETE"
        SSDeleteFailed -> "DELETE_FAILED"
        SSDeleteInProgress -> "DELETE_IN_PROGRESS"
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
instance ToByteString StackStatus
instance ToQuery      StackStatus
instance ToHeader     StackStatus

instance FromXML StackStatus where
    parseXML = parseXMLText "StackStatus"
