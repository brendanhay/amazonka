{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.FleetErrorCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppStream.Types.FleetErrorCode
  ( FleetErrorCode
    ( FleetErrorCode'
    , FleetErrorCodeIamServiceRoleMissingEniDescribeAction
    , FleetErrorCodeIamServiceRoleMissingEniCreateAction
    , FleetErrorCodeIamServiceRoleMissingEniDeleteAction
    , FleetErrorCodeNetworkInterfaceLimitExceeded
    , FleetErrorCodeInternalServiceError
    , FleetErrorCodeIamServiceRoleIsMissing
    , FleetErrorCodeMachineRoleIsMissing
    , FleetErrorCodeStsDisabledInRegion
    , FleetErrorCodeSubnetHasInsufficientIpAddresses
    , FleetErrorCodeIamServiceRoleMissingDescribeSubnetAction
    , FleetErrorCodeSubnetNotFound
    , FleetErrorCodeImageNotFound
    , FleetErrorCodeInvalidSubnetConfiguration
    , FleetErrorCodeSecurityGroupsNotFound
    , FleetErrorCodeIgwNotAttached
    , FleetErrorCodeIamServiceRoleMissingDescribeSecurityGroupsAction
    , FleetErrorCodeDomainJoinErrorFileNotFound
    , FleetErrorCodeDomainJoinErrorAccessDenied
    , FleetErrorCodeDomainJoinErrorLogonFailure
    , FleetErrorCodeDomainJoinErrorInvalidParameter
    , FleetErrorCodeDomainJoinErrorMoreData
    , FleetErrorCodeDomainJoinErrorNoSuchDomain
    , FleetErrorCodeDomainJoinErrorNotSupported
    , FleetErrorCodeDomainJoinNerrInvalidWorkgroupName
    , FleetErrorCodeDomainJoinNerrWorkstationNotStarted
    , FleetErrorCodeDomainJoinErrorDsMachineAccountQuotaExceeded
    , FleetErrorCodeDomainJoinNerrPasswordExpired
    , FleetErrorCodeDomainJoinInternalServiceError
    , fromFleetErrorCode
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype FleetErrorCode = FleetErrorCode'{fromFleetErrorCode ::
                                         Core.Text}
                           deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                           Core.Generic)
                           deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                             Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                             Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                             Core.FromText, Core.ToByteString, Core.ToQuery,
                                             Core.ToHeader)

pattern FleetErrorCodeIamServiceRoleMissingEniDescribeAction :: FleetErrorCode
pattern FleetErrorCodeIamServiceRoleMissingEniDescribeAction = FleetErrorCode' "IAM_SERVICE_ROLE_MISSING_ENI_DESCRIBE_ACTION"

pattern FleetErrorCodeIamServiceRoleMissingEniCreateAction :: FleetErrorCode
pattern FleetErrorCodeIamServiceRoleMissingEniCreateAction = FleetErrorCode' "IAM_SERVICE_ROLE_MISSING_ENI_CREATE_ACTION"

pattern FleetErrorCodeIamServiceRoleMissingEniDeleteAction :: FleetErrorCode
pattern FleetErrorCodeIamServiceRoleMissingEniDeleteAction = FleetErrorCode' "IAM_SERVICE_ROLE_MISSING_ENI_DELETE_ACTION"

pattern FleetErrorCodeNetworkInterfaceLimitExceeded :: FleetErrorCode
pattern FleetErrorCodeNetworkInterfaceLimitExceeded = FleetErrorCode' "NETWORK_INTERFACE_LIMIT_EXCEEDED"

pattern FleetErrorCodeInternalServiceError :: FleetErrorCode
pattern FleetErrorCodeInternalServiceError = FleetErrorCode' "INTERNAL_SERVICE_ERROR"

pattern FleetErrorCodeIamServiceRoleIsMissing :: FleetErrorCode
pattern FleetErrorCodeIamServiceRoleIsMissing = FleetErrorCode' "IAM_SERVICE_ROLE_IS_MISSING"

pattern FleetErrorCodeMachineRoleIsMissing :: FleetErrorCode
pattern FleetErrorCodeMachineRoleIsMissing = FleetErrorCode' "MACHINE_ROLE_IS_MISSING"

pattern FleetErrorCodeStsDisabledInRegion :: FleetErrorCode
pattern FleetErrorCodeStsDisabledInRegion = FleetErrorCode' "STS_DISABLED_IN_REGION"

pattern FleetErrorCodeSubnetHasInsufficientIpAddresses :: FleetErrorCode
pattern FleetErrorCodeSubnetHasInsufficientIpAddresses = FleetErrorCode' "SUBNET_HAS_INSUFFICIENT_IP_ADDRESSES"

pattern FleetErrorCodeIamServiceRoleMissingDescribeSubnetAction :: FleetErrorCode
pattern FleetErrorCodeIamServiceRoleMissingDescribeSubnetAction = FleetErrorCode' "IAM_SERVICE_ROLE_MISSING_DESCRIBE_SUBNET_ACTION"

pattern FleetErrorCodeSubnetNotFound :: FleetErrorCode
pattern FleetErrorCodeSubnetNotFound = FleetErrorCode' "SUBNET_NOT_FOUND"

pattern FleetErrorCodeImageNotFound :: FleetErrorCode
pattern FleetErrorCodeImageNotFound = FleetErrorCode' "IMAGE_NOT_FOUND"

pattern FleetErrorCodeInvalidSubnetConfiguration :: FleetErrorCode
pattern FleetErrorCodeInvalidSubnetConfiguration = FleetErrorCode' "INVALID_SUBNET_CONFIGURATION"

pattern FleetErrorCodeSecurityGroupsNotFound :: FleetErrorCode
pattern FleetErrorCodeSecurityGroupsNotFound = FleetErrorCode' "SECURITY_GROUPS_NOT_FOUND"

pattern FleetErrorCodeIgwNotAttached :: FleetErrorCode
pattern FleetErrorCodeIgwNotAttached = FleetErrorCode' "IGW_NOT_ATTACHED"

pattern FleetErrorCodeIamServiceRoleMissingDescribeSecurityGroupsAction :: FleetErrorCode
pattern FleetErrorCodeIamServiceRoleMissingDescribeSecurityGroupsAction = FleetErrorCode' "IAM_SERVICE_ROLE_MISSING_DESCRIBE_SECURITY_GROUPS_ACTION"

pattern FleetErrorCodeDomainJoinErrorFileNotFound :: FleetErrorCode
pattern FleetErrorCodeDomainJoinErrorFileNotFound = FleetErrorCode' "DOMAIN_JOIN_ERROR_FILE_NOT_FOUND"

pattern FleetErrorCodeDomainJoinErrorAccessDenied :: FleetErrorCode
pattern FleetErrorCodeDomainJoinErrorAccessDenied = FleetErrorCode' "DOMAIN_JOIN_ERROR_ACCESS_DENIED"

pattern FleetErrorCodeDomainJoinErrorLogonFailure :: FleetErrorCode
pattern FleetErrorCodeDomainJoinErrorLogonFailure = FleetErrorCode' "DOMAIN_JOIN_ERROR_LOGON_FAILURE"

pattern FleetErrorCodeDomainJoinErrorInvalidParameter :: FleetErrorCode
pattern FleetErrorCodeDomainJoinErrorInvalidParameter = FleetErrorCode' "DOMAIN_JOIN_ERROR_INVALID_PARAMETER"

pattern FleetErrorCodeDomainJoinErrorMoreData :: FleetErrorCode
pattern FleetErrorCodeDomainJoinErrorMoreData = FleetErrorCode' "DOMAIN_JOIN_ERROR_MORE_DATA"

pattern FleetErrorCodeDomainJoinErrorNoSuchDomain :: FleetErrorCode
pattern FleetErrorCodeDomainJoinErrorNoSuchDomain = FleetErrorCode' "DOMAIN_JOIN_ERROR_NO_SUCH_DOMAIN"

pattern FleetErrorCodeDomainJoinErrorNotSupported :: FleetErrorCode
pattern FleetErrorCodeDomainJoinErrorNotSupported = FleetErrorCode' "DOMAIN_JOIN_ERROR_NOT_SUPPORTED"

pattern FleetErrorCodeDomainJoinNerrInvalidWorkgroupName :: FleetErrorCode
pattern FleetErrorCodeDomainJoinNerrInvalidWorkgroupName = FleetErrorCode' "DOMAIN_JOIN_NERR_INVALID_WORKGROUP_NAME"

pattern FleetErrorCodeDomainJoinNerrWorkstationNotStarted :: FleetErrorCode
pattern FleetErrorCodeDomainJoinNerrWorkstationNotStarted = FleetErrorCode' "DOMAIN_JOIN_NERR_WORKSTATION_NOT_STARTED"

pattern FleetErrorCodeDomainJoinErrorDsMachineAccountQuotaExceeded :: FleetErrorCode
pattern FleetErrorCodeDomainJoinErrorDsMachineAccountQuotaExceeded = FleetErrorCode' "DOMAIN_JOIN_ERROR_DS_MACHINE_ACCOUNT_QUOTA_EXCEEDED"

pattern FleetErrorCodeDomainJoinNerrPasswordExpired :: FleetErrorCode
pattern FleetErrorCodeDomainJoinNerrPasswordExpired = FleetErrorCode' "DOMAIN_JOIN_NERR_PASSWORD_EXPIRED"

pattern FleetErrorCodeDomainJoinInternalServiceError :: FleetErrorCode
pattern FleetErrorCodeDomainJoinInternalServiceError = FleetErrorCode' "DOMAIN_JOIN_INTERNAL_SERVICE_ERROR"

{-# COMPLETE 
  FleetErrorCodeIamServiceRoleMissingEniDescribeAction,

  FleetErrorCodeIamServiceRoleMissingEniCreateAction,

  FleetErrorCodeIamServiceRoleMissingEniDeleteAction,

  FleetErrorCodeNetworkInterfaceLimitExceeded,

  FleetErrorCodeInternalServiceError,

  FleetErrorCodeIamServiceRoleIsMissing,

  FleetErrorCodeMachineRoleIsMissing,

  FleetErrorCodeStsDisabledInRegion,

  FleetErrorCodeSubnetHasInsufficientIpAddresses,

  FleetErrorCodeIamServiceRoleMissingDescribeSubnetAction,

  FleetErrorCodeSubnetNotFound,

  FleetErrorCodeImageNotFound,

  FleetErrorCodeInvalidSubnetConfiguration,

  FleetErrorCodeSecurityGroupsNotFound,

  FleetErrorCodeIgwNotAttached,

  FleetErrorCodeIamServiceRoleMissingDescribeSecurityGroupsAction,

  FleetErrorCodeDomainJoinErrorFileNotFound,

  FleetErrorCodeDomainJoinErrorAccessDenied,

  FleetErrorCodeDomainJoinErrorLogonFailure,

  FleetErrorCodeDomainJoinErrorInvalidParameter,

  FleetErrorCodeDomainJoinErrorMoreData,

  FleetErrorCodeDomainJoinErrorNoSuchDomain,

  FleetErrorCodeDomainJoinErrorNotSupported,

  FleetErrorCodeDomainJoinNerrInvalidWorkgroupName,

  FleetErrorCodeDomainJoinNerrWorkstationNotStarted,

  FleetErrorCodeDomainJoinErrorDsMachineAccountQuotaExceeded,

  FleetErrorCodeDomainJoinNerrPasswordExpired,

  FleetErrorCodeDomainJoinInternalServiceError,
  FleetErrorCode'
  #-}
