{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppStream.Types.Sum where

import Network.AWS.Prelude

data AuthenticationType
  = API
  | Saml
  | Userpool
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AuthenticationType where
    parser = takeLowerText >>= \case
        "api" -> pure API
        "saml" -> pure Saml
        "userpool" -> pure Userpool
        e -> fromTextError $ "Failure parsing AuthenticationType from value: '" <> e
           <> "'. Accepted values: api, saml, userpool"

instance ToText AuthenticationType where
    toText = \case
        API -> "API"
        Saml -> "SAML"
        Userpool -> "USERPOOL"

instance Hashable     AuthenticationType
instance NFData       AuthenticationType
instance ToByteString AuthenticationType
instance ToQuery      AuthenticationType
instance ToHeader     AuthenticationType

instance ToJSON AuthenticationType where
    toJSON = toJSONText

instance FromJSON AuthenticationType where
    parseJSON = parseJSONText "AuthenticationType"

-- | The fleet attribute.
--
--
data FleetAttribute
  = DomainJoinInfo
  | VPCConfiguration
  | VPCConfigurationSecurityGroupIds
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FleetAttribute where
    parser = takeLowerText >>= \case
        "domain_join_info" -> pure DomainJoinInfo
        "vpc_configuration" -> pure VPCConfiguration
        "vpc_configuration_security_group_ids" -> pure VPCConfigurationSecurityGroupIds
        e -> fromTextError $ "Failure parsing FleetAttribute from value: '" <> e
           <> "'. Accepted values: domain_join_info, vpc_configuration, vpc_configuration_security_group_ids"

instance ToText FleetAttribute where
    toText = \case
        DomainJoinInfo -> "DOMAIN_JOIN_INFO"
        VPCConfiguration -> "VPC_CONFIGURATION"
        VPCConfigurationSecurityGroupIds -> "VPC_CONFIGURATION_SECURITY_GROUP_IDS"

instance Hashable     FleetAttribute
instance NFData       FleetAttribute
instance ToByteString FleetAttribute
instance ToQuery      FleetAttribute
instance ToHeader     FleetAttribute

instance ToJSON FleetAttribute where
    toJSON = toJSONText

data FleetErrorCode
  = DomainJoinErrorAccessDenied
  | DomainJoinErrorDsMachineAccountQuotaExceeded
  | DomainJoinErrorFileNotFound
  | DomainJoinErrorInvalidParameter
  | DomainJoinErrorLogonFailure
  | DomainJoinErrorMoreData
  | DomainJoinErrorNoSuchDomain
  | DomainJoinErrorNotSupported
  | DomainJoinInternalServiceError
  | DomainJoinNerrInvalidWorkgroupName
  | DomainJoinNerrPasswordExpired
  | DomainJoinNerrWorkstationNotStarted
  | IAMServiceRoleIsMissing
  | IAMServiceRoleMissingDescribeSecurityGroupsAction
  | IAMServiceRoleMissingDescribeSubnetAction
  | IAMServiceRoleMissingEniCreateAction
  | IAMServiceRoleMissingEniDeleteAction
  | IAMServiceRoleMissingEniDescribeAction
  | IgwNotAttached
  | ImageNotFound
  | InternalServiceError
  | InvalidSubnetConfiguration
  | NetworkInterfaceLimitExceeded
  | SecurityGroupsNotFound
  | SubnetHasInsufficientIPAddresses
  | SubnetNotFound
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FleetErrorCode where
    parser = takeLowerText >>= \case
        "domain_join_error_access_denied" -> pure DomainJoinErrorAccessDenied
        "domain_join_error_ds_machine_account_quota_exceeded" -> pure DomainJoinErrorDsMachineAccountQuotaExceeded
        "domain_join_error_file_not_found" -> pure DomainJoinErrorFileNotFound
        "domain_join_error_invalid_parameter" -> pure DomainJoinErrorInvalidParameter
        "domain_join_error_logon_failure" -> pure DomainJoinErrorLogonFailure
        "domain_join_error_more_data" -> pure DomainJoinErrorMoreData
        "domain_join_error_no_such_domain" -> pure DomainJoinErrorNoSuchDomain
        "domain_join_error_not_supported" -> pure DomainJoinErrorNotSupported
        "domain_join_internal_service_error" -> pure DomainJoinInternalServiceError
        "domain_join_nerr_invalid_workgroup_name" -> pure DomainJoinNerrInvalidWorkgroupName
        "domain_join_nerr_password_expired" -> pure DomainJoinNerrPasswordExpired
        "domain_join_nerr_workstation_not_started" -> pure DomainJoinNerrWorkstationNotStarted
        "iam_service_role_is_missing" -> pure IAMServiceRoleIsMissing
        "iam_service_role_missing_describe_security_groups_action" -> pure IAMServiceRoleMissingDescribeSecurityGroupsAction
        "iam_service_role_missing_describe_subnet_action" -> pure IAMServiceRoleMissingDescribeSubnetAction
        "iam_service_role_missing_eni_create_action" -> pure IAMServiceRoleMissingEniCreateAction
        "iam_service_role_missing_eni_delete_action" -> pure IAMServiceRoleMissingEniDeleteAction
        "iam_service_role_missing_eni_describe_action" -> pure IAMServiceRoleMissingEniDescribeAction
        "igw_not_attached" -> pure IgwNotAttached
        "image_not_found" -> pure ImageNotFound
        "internal_service_error" -> pure InternalServiceError
        "invalid_subnet_configuration" -> pure InvalidSubnetConfiguration
        "network_interface_limit_exceeded" -> pure NetworkInterfaceLimitExceeded
        "security_groups_not_found" -> pure SecurityGroupsNotFound
        "subnet_has_insufficient_ip_addresses" -> pure SubnetHasInsufficientIPAddresses
        "subnet_not_found" -> pure SubnetNotFound
        e -> fromTextError $ "Failure parsing FleetErrorCode from value: '" <> e
           <> "'. Accepted values: domain_join_error_access_denied, domain_join_error_ds_machine_account_quota_exceeded, domain_join_error_file_not_found, domain_join_error_invalid_parameter, domain_join_error_logon_failure, domain_join_error_more_data, domain_join_error_no_such_domain, domain_join_error_not_supported, domain_join_internal_service_error, domain_join_nerr_invalid_workgroup_name, domain_join_nerr_password_expired, domain_join_nerr_workstation_not_started, iam_service_role_is_missing, iam_service_role_missing_describe_security_groups_action, iam_service_role_missing_describe_subnet_action, iam_service_role_missing_eni_create_action, iam_service_role_missing_eni_delete_action, iam_service_role_missing_eni_describe_action, igw_not_attached, image_not_found, internal_service_error, invalid_subnet_configuration, network_interface_limit_exceeded, security_groups_not_found, subnet_has_insufficient_ip_addresses, subnet_not_found"

instance ToText FleetErrorCode where
    toText = \case
        DomainJoinErrorAccessDenied -> "DOMAIN_JOIN_ERROR_ACCESS_DENIED"
        DomainJoinErrorDsMachineAccountQuotaExceeded -> "DOMAIN_JOIN_ERROR_DS_MACHINE_ACCOUNT_QUOTA_EXCEEDED"
        DomainJoinErrorFileNotFound -> "DOMAIN_JOIN_ERROR_FILE_NOT_FOUND"
        DomainJoinErrorInvalidParameter -> "DOMAIN_JOIN_ERROR_INVALID_PARAMETER"
        DomainJoinErrorLogonFailure -> "DOMAIN_JOIN_ERROR_LOGON_FAILURE"
        DomainJoinErrorMoreData -> "DOMAIN_JOIN_ERROR_MORE_DATA"
        DomainJoinErrorNoSuchDomain -> "DOMAIN_JOIN_ERROR_NO_SUCH_DOMAIN"
        DomainJoinErrorNotSupported -> "DOMAIN_JOIN_ERROR_NOT_SUPPORTED"
        DomainJoinInternalServiceError -> "DOMAIN_JOIN_INTERNAL_SERVICE_ERROR"
        DomainJoinNerrInvalidWorkgroupName -> "DOMAIN_JOIN_NERR_INVALID_WORKGROUP_NAME"
        DomainJoinNerrPasswordExpired -> "DOMAIN_JOIN_NERR_PASSWORD_EXPIRED"
        DomainJoinNerrWorkstationNotStarted -> "DOMAIN_JOIN_NERR_WORKSTATION_NOT_STARTED"
        IAMServiceRoleIsMissing -> "IAM_SERVICE_ROLE_IS_MISSING"
        IAMServiceRoleMissingDescribeSecurityGroupsAction -> "IAM_SERVICE_ROLE_MISSING_DESCRIBE_SECURITY_GROUPS_ACTION"
        IAMServiceRoleMissingDescribeSubnetAction -> "IAM_SERVICE_ROLE_MISSING_DESCRIBE_SUBNET_ACTION"
        IAMServiceRoleMissingEniCreateAction -> "IAM_SERVICE_ROLE_MISSING_ENI_CREATE_ACTION"
        IAMServiceRoleMissingEniDeleteAction -> "IAM_SERVICE_ROLE_MISSING_ENI_DELETE_ACTION"
        IAMServiceRoleMissingEniDescribeAction -> "IAM_SERVICE_ROLE_MISSING_ENI_DESCRIBE_ACTION"
        IgwNotAttached -> "IGW_NOT_ATTACHED"
        ImageNotFound -> "IMAGE_NOT_FOUND"
        InternalServiceError -> "INTERNAL_SERVICE_ERROR"
        InvalidSubnetConfiguration -> "INVALID_SUBNET_CONFIGURATION"
        NetworkInterfaceLimitExceeded -> "NETWORK_INTERFACE_LIMIT_EXCEEDED"
        SecurityGroupsNotFound -> "SECURITY_GROUPS_NOT_FOUND"
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
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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

data FleetType
  = AlwaysOn
  | OnDemand
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FleetType where
    parser = takeLowerText >>= \case
        "always_on" -> pure AlwaysOn
        "on_demand" -> pure OnDemand
        e -> fromTextError $ "Failure parsing FleetType from value: '" <> e
           <> "'. Accepted values: always_on, on_demand"

instance ToText FleetType where
    toText = \case
        AlwaysOn -> "ALWAYS_ON"
        OnDemand -> "ON_DEMAND"

instance Hashable     FleetType
instance NFData       FleetType
instance ToByteString FleetType
instance ToQuery      FleetType
instance ToHeader     FleetType

instance ToJSON FleetType where
    toJSON = toJSONText

instance FromJSON FleetType where
    parseJSON = parseJSONText "FleetType"

data ImageBuilderState
  = IBSDeleting
  | IBSFailed
  | IBSPending
  | IBSRebooting
  | IBSRunning
  | IBSSnapshotting
  | IBSStopped
  | IBSStopping
  | IBSUpdatingAgent
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ImageBuilderState where
    parser = takeLowerText >>= \case
        "deleting" -> pure IBSDeleting
        "failed" -> pure IBSFailed
        "pending" -> pure IBSPending
        "rebooting" -> pure IBSRebooting
        "running" -> pure IBSRunning
        "snapshotting" -> pure IBSSnapshotting
        "stopped" -> pure IBSStopped
        "stopping" -> pure IBSStopping
        "updating_agent" -> pure IBSUpdatingAgent
        e -> fromTextError $ "Failure parsing ImageBuilderState from value: '" <> e
           <> "'. Accepted values: deleting, failed, pending, rebooting, running, snapshotting, stopped, stopping, updating_agent"

instance ToText ImageBuilderState where
    toText = \case
        IBSDeleting -> "DELETING"
        IBSFailed -> "FAILED"
        IBSPending -> "PENDING"
        IBSRebooting -> "REBOOTING"
        IBSRunning -> "RUNNING"
        IBSSnapshotting -> "SNAPSHOTTING"
        IBSStopped -> "STOPPED"
        IBSStopping -> "STOPPING"
        IBSUpdatingAgent -> "UPDATING_AGENT"

instance Hashable     ImageBuilderState
instance NFData       ImageBuilderState
instance ToByteString ImageBuilderState
instance ToQuery      ImageBuilderState
instance ToHeader     ImageBuilderState

instance FromJSON ImageBuilderState where
    parseJSON = parseJSONText "ImageBuilderState"

data ImageBuilderStateChangeReasonCode
  = ImageUnavailable
  | InternalError
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ImageBuilderStateChangeReasonCode where
    parser = takeLowerText >>= \case
        "image_unavailable" -> pure ImageUnavailable
        "internal_error" -> pure InternalError
        e -> fromTextError $ "Failure parsing ImageBuilderStateChangeReasonCode from value: '" <> e
           <> "'. Accepted values: image_unavailable, internal_error"

instance ToText ImageBuilderStateChangeReasonCode where
    toText = \case
        ImageUnavailable -> "IMAGE_UNAVAILABLE"
        InternalError -> "INTERNAL_ERROR"

instance Hashable     ImageBuilderStateChangeReasonCode
instance NFData       ImageBuilderStateChangeReasonCode
instance ToByteString ImageBuilderStateChangeReasonCode
instance ToQuery      ImageBuilderStateChangeReasonCode
instance ToHeader     ImageBuilderStateChangeReasonCode

instance FromJSON ImageBuilderStateChangeReasonCode where
    parseJSON = parseJSONText "ImageBuilderStateChangeReasonCode"

data ImageState
  = ISAvailable
  | ISCopying
  | ISDeleting
  | ISFailed
  | ISPending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ImageState where
    parser = takeLowerText >>= \case
        "available" -> pure ISAvailable
        "copying" -> pure ISCopying
        "deleting" -> pure ISDeleting
        "failed" -> pure ISFailed
        "pending" -> pure ISPending
        e -> fromTextError $ "Failure parsing ImageState from value: '" <> e
           <> "'. Accepted values: available, copying, deleting, failed, pending"

instance ToText ImageState where
    toText = \case
        ISAvailable -> "AVAILABLE"
        ISCopying -> "COPYING"
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
  = ISCRCImageBuilderNotAvailable
  | ISCRCImageCopyFailure
  | ISCRCInternalError
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ImageStateChangeReasonCode where
    parser = takeLowerText >>= \case
        "image_builder_not_available" -> pure ISCRCImageBuilderNotAvailable
        "image_copy_failure" -> pure ISCRCImageCopyFailure
        "internal_error" -> pure ISCRCInternalError
        e -> fromTextError $ "Failure parsing ImageStateChangeReasonCode from value: '" <> e
           <> "'. Accepted values: image_builder_not_available, image_copy_failure, internal_error"

instance ToText ImageStateChangeReasonCode where
    toText = \case
        ISCRCImageBuilderNotAvailable -> "IMAGE_BUILDER_NOT_AVAILABLE"
        ISCRCImageCopyFailure -> "IMAGE_COPY_FAILURE"
        ISCRCInternalError -> "INTERNAL_ERROR"

instance Hashable     ImageStateChangeReasonCode
instance NFData       ImageStateChangeReasonCode
instance ToByteString ImageStateChangeReasonCode
instance ToQuery      ImageStateChangeReasonCode
instance ToHeader     ImageStateChangeReasonCode

instance FromJSON ImageStateChangeReasonCode where
    parseJSON = parseJSONText "ImageStateChangeReasonCode"

data PlatformType =
  Windows
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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

data StackAttribute
  = FeedbackURL
  | RedirectURL
  | StorageConnectors
  | ThemeName
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StackAttribute where
    parser = takeLowerText >>= \case
        "feedback_url" -> pure FeedbackURL
        "redirect_url" -> pure RedirectURL
        "storage_connectors" -> pure StorageConnectors
        "theme_name" -> pure ThemeName
        e -> fromTextError $ "Failure parsing StackAttribute from value: '" <> e
           <> "'. Accepted values: feedback_url, redirect_url, storage_connectors, theme_name"

instance ToText StackAttribute where
    toText = \case
        FeedbackURL -> "FEEDBACK_URL"
        RedirectURL -> "REDIRECT_URL"
        StorageConnectors -> "STORAGE_CONNECTORS"
        ThemeName -> "THEME_NAME"

instance Hashable     StackAttribute
instance NFData       StackAttribute
instance ToByteString StackAttribute
instance ToQuery      StackAttribute
instance ToHeader     StackAttribute

instance ToJSON StackAttribute where
    toJSON = toJSONText

data StackErrorCode
  = SECInternalServiceError
  | SECStorageConnectorError
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StackErrorCode where
    parser = takeLowerText >>= \case
        "internal_service_error" -> pure SECInternalServiceError
        "storage_connector_error" -> pure SECStorageConnectorError
        e -> fromTextError $ "Failure parsing StackErrorCode from value: '" <> e
           <> "'. Accepted values: internal_service_error, storage_connector_error"

instance ToText StackErrorCode where
    toText = \case
        SECInternalServiceError -> "INTERNAL_SERVICE_ERROR"
        SECStorageConnectorError -> "STORAGE_CONNECTOR_ERROR"

instance Hashable     StackErrorCode
instance NFData       StackErrorCode
instance ToByteString StackErrorCode
instance ToQuery      StackErrorCode
instance ToHeader     StackErrorCode

instance FromJSON StackErrorCode where
    parseJSON = parseJSONText "StackErrorCode"

-- | The type of storage connector.
--
--
data StorageConnectorType =
  Homefolders
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StorageConnectorType where
    parser = takeLowerText >>= \case
        "homefolders" -> pure Homefolders
        e -> fromTextError $ "Failure parsing StorageConnectorType from value: '" <> e
           <> "'. Accepted values: homefolders"

instance ToText StorageConnectorType where
    toText = \case
        Homefolders -> "HOMEFOLDERS"

instance Hashable     StorageConnectorType
instance NFData       StorageConnectorType
instance ToByteString StorageConnectorType
instance ToQuery      StorageConnectorType
instance ToHeader     StorageConnectorType

instance ToJSON StorageConnectorType where
    toJSON = toJSONText

instance FromJSON StorageConnectorType where
    parseJSON = parseJSONText "StorageConnectorType"

data VisibilityType
  = Private
  | Public
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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
