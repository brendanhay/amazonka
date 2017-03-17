{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.Sum
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppStream.Types.Sum where

import           Network.AWS.Prelude

data FleetErrorCode
    = IAMServiceRoleIsMissing
    | IAMServiceRoleMissingDescribeSubnetAction
    | IAMServiceRoleMissingEniCreateAction
    | IAMServiceRoleMissingEniDeleteAction
    | IAMServiceRoleMissingEniDescribeAction
    | ImageNotFound
    | InternalServiceError
    | InvalidSubnetConfiguration
    | NetworkInterfaceLimitExceeded
    | SubnetHasInsufficientIPAddresses
    | SubnetNotFound
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText FleetErrorCode where
    parser = takeLowerText >>= \case
        "iam_service_role_is_missing" -> pure IAMServiceRoleIsMissing
        "iam_service_role_missing_describe_subnet_action" -> pure IAMServiceRoleMissingDescribeSubnetAction
        "iam_service_role_missing_eni_create_action" -> pure IAMServiceRoleMissingEniCreateAction
        "iam_service_role_missing_eni_delete_action" -> pure IAMServiceRoleMissingEniDeleteAction
        "iam_service_role_missing_eni_describe_action" -> pure IAMServiceRoleMissingEniDescribeAction
        "image_not_found" -> pure ImageNotFound
        "internal_service_error" -> pure InternalServiceError
        "invalid_subnet_configuration" -> pure InvalidSubnetConfiguration
        "network_interface_limit_exceeded" -> pure NetworkInterfaceLimitExceeded
        "subnet_has_insufficient_ip_addresses" -> pure SubnetHasInsufficientIPAddresses
        "subnet_not_found" -> pure SubnetNotFound
        e -> fromTextError $ "Failure parsing FleetErrorCode from value: '" <> e
           <> "'. Accepted values: iam_service_role_is_missing, iam_service_role_missing_describe_subnet_action, iam_service_role_missing_eni_create_action, iam_service_role_missing_eni_delete_action, iam_service_role_missing_eni_describe_action, image_not_found, internal_service_error, invalid_subnet_configuration, network_interface_limit_exceeded, subnet_has_insufficient_ip_addresses, subnet_not_found"

instance ToText FleetErrorCode where
    toText = \case
        IAMServiceRoleIsMissing -> "IAM_SERVICE_ROLE_IS_MISSING"
        IAMServiceRoleMissingDescribeSubnetAction -> "IAM_SERVICE_ROLE_MISSING_DESCRIBE_SUBNET_ACTION"
        IAMServiceRoleMissingEniCreateAction -> "IAM_SERVICE_ROLE_MISSING_ENI_CREATE_ACTION"
        IAMServiceRoleMissingEniDeleteAction -> "IAM_SERVICE_ROLE_MISSING_ENI_DELETE_ACTION"
        IAMServiceRoleMissingEniDescribeAction -> "IAM_SERVICE_ROLE_MISSING_ENI_DESCRIBE_ACTION"
        ImageNotFound -> "IMAGE_NOT_FOUND"
        InternalServiceError -> "INTERNAL_SERVICE_ERROR"
        InvalidSubnetConfiguration -> "INVALID_SUBNET_CONFIGURATION"
        NetworkInterfaceLimitExceeded -> "NETWORK_INTERFACE_LIMIT_EXCEEDED"
        SubnetHasInsufficientIPAddresses -> "SUBNET_HAS_INSUFFICIENT_IP_ADDRESSES"
        SubnetNotFound -> "SUBNET_NOT_FOUND"

instance Hashable     FleetErrorCode
instance NFData       FleetErrorCode
instance ToByteString FleetErrorCode
instance ToQuery      FleetErrorCode
instance ToHeader     FleetErrorCode

instance FromJSON FleetErrorCode where
    parseJSON = parseJSONText "FleetErrorCode"

data FleetState
    = Running
    | Starting
    | Stopped
    | Stopping
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText FleetState where
    parser = takeLowerText >>= \case
        "running" -> pure Running
        "starting" -> pure Starting
        "stopped" -> pure Stopped
        "stopping" -> pure Stopping
        e -> fromTextError $ "Failure parsing FleetState from value: '" <> e
           <> "'. Accepted values: running, starting, stopped, stopping"

instance ToText FleetState where
    toText = \case
        Running -> "RUNNING"
        Starting -> "STARTING"
        Stopped -> "STOPPED"
        Stopping -> "STOPPING"

instance Hashable     FleetState
instance NFData       FleetState
instance ToByteString FleetState
instance ToQuery      FleetState
instance ToHeader     FleetState

instance FromJSON FleetState where
    parseJSON = parseJSONText "FleetState"

data ImageState
    = ISAvailable
    | ISDeleting
    | ISFailed
    | ISPending
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText ImageState where
    parser = takeLowerText >>= \case
        "available" -> pure ISAvailable
        "deleting" -> pure ISDeleting
        "failed" -> pure ISFailed
        "pending" -> pure ISPending
        e -> fromTextError $ "Failure parsing ImageState from value: '" <> e
           <> "'. Accepted values: available, deleting, failed, pending"

instance ToText ImageState where
    toText = \case
        ISAvailable -> "AVAILABLE"
        ISDeleting -> "DELETING"
        ISFailed -> "FAILED"
        ISPending -> "PENDING"

instance Hashable     ImageState
instance NFData       ImageState
instance ToByteString ImageState
instance ToQuery      ImageState
instance ToHeader     ImageState

instance FromJSON ImageState where
    parseJSON = parseJSONText "ImageState"

data ImageStateChangeReasonCode
    = ImageBuilderNotAvailable
    | InternalError
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText ImageStateChangeReasonCode where
    parser = takeLowerText >>= \case
        "image_builder_not_available" -> pure ImageBuilderNotAvailable
        "internal_error" -> pure InternalError
        e -> fromTextError $ "Failure parsing ImageStateChangeReasonCode from value: '" <> e
           <> "'. Accepted values: image_builder_not_available, internal_error"

instance ToText ImageStateChangeReasonCode where
    toText = \case
        ImageBuilderNotAvailable -> "IMAGE_BUILDER_NOT_AVAILABLE"
        InternalError -> "INTERNAL_ERROR"

instance Hashable     ImageStateChangeReasonCode
instance NFData       ImageStateChangeReasonCode
instance ToByteString ImageStateChangeReasonCode
instance ToQuery      ImageStateChangeReasonCode
instance ToHeader     ImageStateChangeReasonCode

instance FromJSON ImageStateChangeReasonCode where
    parseJSON = parseJSONText "ImageStateChangeReasonCode"

data PlatformType =
    Windows
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText PlatformType where
    parser = takeLowerText >>= \case
        "windows" -> pure Windows
        e -> fromTextError $ "Failure parsing PlatformType from value: '" <> e
           <> "'. Accepted values: windows"

instance ToText PlatformType where
    toText = \case
        Windows -> "WINDOWS"

instance Hashable     PlatformType
instance NFData       PlatformType
instance ToByteString PlatformType
instance ToQuery      PlatformType
instance ToHeader     PlatformType

instance FromJSON PlatformType where
    parseJSON = parseJSONText "PlatformType"

-- | Possible values for the state of a streaming session.
--
--
data SessionState
    = Active
    | Expired
    | Pending
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText SessionState where
    parser = takeLowerText >>= \case
        "active" -> pure Active
        "expired" -> pure Expired
        "pending" -> pure Pending
        e -> fromTextError $ "Failure parsing SessionState from value: '" <> e
           <> "'. Accepted values: active, expired, pending"

instance ToText SessionState where
    toText = \case
        Active -> "ACTIVE"
        Expired -> "EXPIRED"
        Pending -> "PENDING"

instance Hashable     SessionState
instance NFData       SessionState
instance ToByteString SessionState
instance ToQuery      SessionState
instance ToHeader     SessionState

instance FromJSON SessionState where
    parseJSON = parseJSONText "SessionState"

data VisibilityType
    = Private
    | Public
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText VisibilityType where
    parser = takeLowerText >>= \case
        "private" -> pure Private
        "public" -> pure Public
        e -> fromTextError $ "Failure parsing VisibilityType from value: '" <> e
           <> "'. Accepted values: private, public"

instance ToText VisibilityType where
    toText = \case
        Private -> "PRIVATE"
        Public -> "PUBLIC"

instance Hashable     VisibilityType
instance NFData       VisibilityType
instance ToByteString VisibilityType
instance ToQuery      VisibilityType
instance ToHeader     VisibilityType

instance FromJSON VisibilityType where
    parseJSON = parseJSONText "VisibilityType"
