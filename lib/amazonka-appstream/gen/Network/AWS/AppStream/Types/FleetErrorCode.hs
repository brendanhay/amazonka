{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.FleetErrorCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.FleetErrorCode where

import Network.AWS.Prelude

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
  | MachineRoleIsMissing
  | NetworkInterfaceLimitExceeded
  | SecurityGroupsNotFound
  | StsDisabledInRegion
  | SubnetHasInsufficientIPAddresses
  | SubnetNotFound
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText FleetErrorCode where
  parser =
    takeLowerText >>= \case
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
      "machine_role_is_missing" -> pure MachineRoleIsMissing
      "network_interface_limit_exceeded" -> pure NetworkInterfaceLimitExceeded
      "security_groups_not_found" -> pure SecurityGroupsNotFound
      "sts_disabled_in_region" -> pure StsDisabledInRegion
      "subnet_has_insufficient_ip_addresses" -> pure SubnetHasInsufficientIPAddresses
      "subnet_not_found" -> pure SubnetNotFound
      e ->
        fromTextError $
          "Failure parsing FleetErrorCode from value: '" <> e
            <> "'. Accepted values: domain_join_error_access_denied, domain_join_error_ds_machine_account_quota_exceeded, domain_join_error_file_not_found, domain_join_error_invalid_parameter, domain_join_error_logon_failure, domain_join_error_more_data, domain_join_error_no_such_domain, domain_join_error_not_supported, domain_join_internal_service_error, domain_join_nerr_invalid_workgroup_name, domain_join_nerr_password_expired, domain_join_nerr_workstation_not_started, iam_service_role_is_missing, iam_service_role_missing_describe_security_groups_action, iam_service_role_missing_describe_subnet_action, iam_service_role_missing_eni_create_action, iam_service_role_missing_eni_delete_action, iam_service_role_missing_eni_describe_action, igw_not_attached, image_not_found, internal_service_error, invalid_subnet_configuration, machine_role_is_missing, network_interface_limit_exceeded, security_groups_not_found, sts_disabled_in_region, subnet_has_insufficient_ip_addresses, subnet_not_found"

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
    MachineRoleIsMissing -> "MACHINE_ROLE_IS_MISSING"
    NetworkInterfaceLimitExceeded -> "NETWORK_INTERFACE_LIMIT_EXCEEDED"
    SecurityGroupsNotFound -> "SECURITY_GROUPS_NOT_FOUND"
    StsDisabledInRegion -> "STS_DISABLED_IN_REGION"
    SubnetHasInsufficientIPAddresses -> "SUBNET_HAS_INSUFFICIENT_IP_ADDRESSES"
    SubnetNotFound -> "SUBNET_NOT_FOUND"

instance Hashable FleetErrorCode

instance NFData FleetErrorCode

instance ToByteString FleetErrorCode

instance ToQuery FleetErrorCode

instance ToHeader FleetErrorCode

instance FromJSON FleetErrorCode where
  parseJSON = parseJSONText "FleetErrorCode"
