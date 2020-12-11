-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.FleetErrorCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.FleetErrorCode
  ( FleetErrorCode
      ( FleetErrorCode',
        DomainJoinErrorAccessDenied,
        DomainJoinErrorDsMachineAccountQuotaExceeded,
        DomainJoinErrorFileNotFound,
        DomainJoinErrorInvalidParameter,
        DomainJoinErrorLogonFailure,
        DomainJoinErrorMoreData,
        DomainJoinErrorNoSuchDomain,
        DomainJoinErrorNotSupported,
        DomainJoinInternalServiceError,
        DomainJoinNerrInvalidWorkgroupName,
        DomainJoinNerrPasswordExpired,
        DomainJoinNerrWorkstationNotStarted,
        IAMServiceRoleIsMissing,
        IAMServiceRoleMissingDescribeSecurityGroupsAction,
        IAMServiceRoleMissingDescribeSubnetAction,
        IAMServiceRoleMissingEniCreateAction,
        IAMServiceRoleMissingEniDeleteAction,
        IAMServiceRoleMissingEniDescribeAction,
        IgwNotAttached,
        ImageNotFound,
        InternalServiceError,
        InvalidSubnetConfiguration,
        MachineRoleIsMissing,
        NetworkInterfaceLimitExceeded,
        SecurityGroupsNotFound,
        StsDisabledInRegion,
        SubnetHasInsufficientIPAddresses,
        SubnetNotFound
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype FleetErrorCode = FleetErrorCode' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern DomainJoinErrorAccessDenied :: FleetErrorCode
pattern DomainJoinErrorAccessDenied = FleetErrorCode' "DOMAIN_JOIN_ERROR_ACCESS_DENIED"

pattern DomainJoinErrorDsMachineAccountQuotaExceeded :: FleetErrorCode
pattern DomainJoinErrorDsMachineAccountQuotaExceeded = FleetErrorCode' "DOMAIN_JOIN_ERROR_DS_MACHINE_ACCOUNT_QUOTA_EXCEEDED"

pattern DomainJoinErrorFileNotFound :: FleetErrorCode
pattern DomainJoinErrorFileNotFound = FleetErrorCode' "DOMAIN_JOIN_ERROR_FILE_NOT_FOUND"

pattern DomainJoinErrorInvalidParameter :: FleetErrorCode
pattern DomainJoinErrorInvalidParameter = FleetErrorCode' "DOMAIN_JOIN_ERROR_INVALID_PARAMETER"

pattern DomainJoinErrorLogonFailure :: FleetErrorCode
pattern DomainJoinErrorLogonFailure = FleetErrorCode' "DOMAIN_JOIN_ERROR_LOGON_FAILURE"

pattern DomainJoinErrorMoreData :: FleetErrorCode
pattern DomainJoinErrorMoreData = FleetErrorCode' "DOMAIN_JOIN_ERROR_MORE_DATA"

pattern DomainJoinErrorNoSuchDomain :: FleetErrorCode
pattern DomainJoinErrorNoSuchDomain = FleetErrorCode' "DOMAIN_JOIN_ERROR_NO_SUCH_DOMAIN"

pattern DomainJoinErrorNotSupported :: FleetErrorCode
pattern DomainJoinErrorNotSupported = FleetErrorCode' "DOMAIN_JOIN_ERROR_NOT_SUPPORTED"

pattern DomainJoinInternalServiceError :: FleetErrorCode
pattern DomainJoinInternalServiceError = FleetErrorCode' "DOMAIN_JOIN_INTERNAL_SERVICE_ERROR"

pattern DomainJoinNerrInvalidWorkgroupName :: FleetErrorCode
pattern DomainJoinNerrInvalidWorkgroupName = FleetErrorCode' "DOMAIN_JOIN_NERR_INVALID_WORKGROUP_NAME"

pattern DomainJoinNerrPasswordExpired :: FleetErrorCode
pattern DomainJoinNerrPasswordExpired = FleetErrorCode' "DOMAIN_JOIN_NERR_PASSWORD_EXPIRED"

pattern DomainJoinNerrWorkstationNotStarted :: FleetErrorCode
pattern DomainJoinNerrWorkstationNotStarted = FleetErrorCode' "DOMAIN_JOIN_NERR_WORKSTATION_NOT_STARTED"

pattern IAMServiceRoleIsMissing :: FleetErrorCode
pattern IAMServiceRoleIsMissing = FleetErrorCode' "IAM_SERVICE_ROLE_IS_MISSING"

pattern IAMServiceRoleMissingDescribeSecurityGroupsAction :: FleetErrorCode
pattern IAMServiceRoleMissingDescribeSecurityGroupsAction = FleetErrorCode' "IAM_SERVICE_ROLE_MISSING_DESCRIBE_SECURITY_GROUPS_ACTION"

pattern IAMServiceRoleMissingDescribeSubnetAction :: FleetErrorCode
pattern IAMServiceRoleMissingDescribeSubnetAction = FleetErrorCode' "IAM_SERVICE_ROLE_MISSING_DESCRIBE_SUBNET_ACTION"

pattern IAMServiceRoleMissingEniCreateAction :: FleetErrorCode
pattern IAMServiceRoleMissingEniCreateAction = FleetErrorCode' "IAM_SERVICE_ROLE_MISSING_ENI_CREATE_ACTION"

pattern IAMServiceRoleMissingEniDeleteAction :: FleetErrorCode
pattern IAMServiceRoleMissingEniDeleteAction = FleetErrorCode' "IAM_SERVICE_ROLE_MISSING_ENI_DELETE_ACTION"

pattern IAMServiceRoleMissingEniDescribeAction :: FleetErrorCode
pattern IAMServiceRoleMissingEniDescribeAction = FleetErrorCode' "IAM_SERVICE_ROLE_MISSING_ENI_DESCRIBE_ACTION"

pattern IgwNotAttached :: FleetErrorCode
pattern IgwNotAttached = FleetErrorCode' "IGW_NOT_ATTACHED"

pattern ImageNotFound :: FleetErrorCode
pattern ImageNotFound = FleetErrorCode' "IMAGE_NOT_FOUND"

pattern InternalServiceError :: FleetErrorCode
pattern InternalServiceError = FleetErrorCode' "INTERNAL_SERVICE_ERROR"

pattern InvalidSubnetConfiguration :: FleetErrorCode
pattern InvalidSubnetConfiguration = FleetErrorCode' "INVALID_SUBNET_CONFIGURATION"

pattern MachineRoleIsMissing :: FleetErrorCode
pattern MachineRoleIsMissing = FleetErrorCode' "MACHINE_ROLE_IS_MISSING"

pattern NetworkInterfaceLimitExceeded :: FleetErrorCode
pattern NetworkInterfaceLimitExceeded = FleetErrorCode' "NETWORK_INTERFACE_LIMIT_EXCEEDED"

pattern SecurityGroupsNotFound :: FleetErrorCode
pattern SecurityGroupsNotFound = FleetErrorCode' "SECURITY_GROUPS_NOT_FOUND"

pattern StsDisabledInRegion :: FleetErrorCode
pattern StsDisabledInRegion = FleetErrorCode' "STS_DISABLED_IN_REGION"

pattern SubnetHasInsufficientIPAddresses :: FleetErrorCode
pattern SubnetHasInsufficientIPAddresses = FleetErrorCode' "SUBNET_HAS_INSUFFICIENT_IP_ADDRESSES"

pattern SubnetNotFound :: FleetErrorCode
pattern SubnetNotFound = FleetErrorCode' "SUBNET_NOT_FOUND"

{-# COMPLETE
  DomainJoinErrorAccessDenied,
  DomainJoinErrorDsMachineAccountQuotaExceeded,
  DomainJoinErrorFileNotFound,
  DomainJoinErrorInvalidParameter,
  DomainJoinErrorLogonFailure,
  DomainJoinErrorMoreData,
  DomainJoinErrorNoSuchDomain,
  DomainJoinErrorNotSupported,
  DomainJoinInternalServiceError,
  DomainJoinNerrInvalidWorkgroupName,
  DomainJoinNerrPasswordExpired,
  DomainJoinNerrWorkstationNotStarted,
  IAMServiceRoleIsMissing,
  IAMServiceRoleMissingDescribeSecurityGroupsAction,
  IAMServiceRoleMissingDescribeSubnetAction,
  IAMServiceRoleMissingEniCreateAction,
  IAMServiceRoleMissingEniDeleteAction,
  IAMServiceRoleMissingEniDescribeAction,
  IgwNotAttached,
  ImageNotFound,
  InternalServiceError,
  InvalidSubnetConfiguration,
  MachineRoleIsMissing,
  NetworkInterfaceLimitExceeded,
  SecurityGroupsNotFound,
  StsDisabledInRegion,
  SubnetHasInsufficientIPAddresses,
  SubnetNotFound,
  FleetErrorCode'
  #-}
