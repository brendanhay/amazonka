{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AppStream.Types.FleetErrorCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.FleetErrorCode
  ( FleetErrorCode
      ( ..,
        FleetErrorCode_DOMAIN_JOIN_ERROR_ACCESS_DENIED,
        FleetErrorCode_DOMAIN_JOIN_ERROR_DS_MACHINE_ACCOUNT_QUOTA_EXCEEDED,
        FleetErrorCode_DOMAIN_JOIN_ERROR_FILE_NOT_FOUND,
        FleetErrorCode_DOMAIN_JOIN_ERROR_INVALID_PARAMETER,
        FleetErrorCode_DOMAIN_JOIN_ERROR_LOGON_FAILURE,
        FleetErrorCode_DOMAIN_JOIN_ERROR_MORE_DATA,
        FleetErrorCode_DOMAIN_JOIN_ERROR_NOT_SUPPORTED,
        FleetErrorCode_DOMAIN_JOIN_ERROR_NO_SUCH_DOMAIN,
        FleetErrorCode_DOMAIN_JOIN_INTERNAL_SERVICE_ERROR,
        FleetErrorCode_DOMAIN_JOIN_NERR_INVALID_WORKGROUP_NAME,
        FleetErrorCode_DOMAIN_JOIN_NERR_PASSWORD_EXPIRED,
        FleetErrorCode_DOMAIN_JOIN_NERR_WORKSTATION_NOT_STARTED,
        FleetErrorCode_FLEET_INSTANCE_PROVISIONING_FAILURE,
        FleetErrorCode_FLEET_STOPPED,
        FleetErrorCode_IAM_SERVICE_ROLE_IS_MISSING,
        FleetErrorCode_IAM_SERVICE_ROLE_MISSING_DESCRIBE_SECURITY_GROUPS_ACTION,
        FleetErrorCode_IAM_SERVICE_ROLE_MISSING_DESCRIBE_SUBNET_ACTION,
        FleetErrorCode_IAM_SERVICE_ROLE_MISSING_ENI_CREATE_ACTION,
        FleetErrorCode_IAM_SERVICE_ROLE_MISSING_ENI_DELETE_ACTION,
        FleetErrorCode_IAM_SERVICE_ROLE_MISSING_ENI_DESCRIBE_ACTION,
        FleetErrorCode_IGW_NOT_ATTACHED,
        FleetErrorCode_IMAGE_NOT_FOUND,
        FleetErrorCode_INTERNAL_SERVICE_ERROR,
        FleetErrorCode_INVALID_SUBNET_CONFIGURATION,
        FleetErrorCode_MACHINE_ROLE_IS_MISSING,
        FleetErrorCode_NETWORK_INTERFACE_LIMIT_EXCEEDED,
        FleetErrorCode_SECURITY_GROUPS_NOT_FOUND,
        FleetErrorCode_STS_DISABLED_IN_REGION,
        FleetErrorCode_SUBNET_HAS_INSUFFICIENT_IP_ADDRESSES,
        FleetErrorCode_SUBNET_NOT_FOUND
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FleetErrorCode = FleetErrorCode'
  { fromFleetErrorCode ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern FleetErrorCode_DOMAIN_JOIN_ERROR_ACCESS_DENIED :: FleetErrorCode
pattern FleetErrorCode_DOMAIN_JOIN_ERROR_ACCESS_DENIED = FleetErrorCode' "DOMAIN_JOIN_ERROR_ACCESS_DENIED"

pattern FleetErrorCode_DOMAIN_JOIN_ERROR_DS_MACHINE_ACCOUNT_QUOTA_EXCEEDED :: FleetErrorCode
pattern FleetErrorCode_DOMAIN_JOIN_ERROR_DS_MACHINE_ACCOUNT_QUOTA_EXCEEDED = FleetErrorCode' "DOMAIN_JOIN_ERROR_DS_MACHINE_ACCOUNT_QUOTA_EXCEEDED"

pattern FleetErrorCode_DOMAIN_JOIN_ERROR_FILE_NOT_FOUND :: FleetErrorCode
pattern FleetErrorCode_DOMAIN_JOIN_ERROR_FILE_NOT_FOUND = FleetErrorCode' "DOMAIN_JOIN_ERROR_FILE_NOT_FOUND"

pattern FleetErrorCode_DOMAIN_JOIN_ERROR_INVALID_PARAMETER :: FleetErrorCode
pattern FleetErrorCode_DOMAIN_JOIN_ERROR_INVALID_PARAMETER = FleetErrorCode' "DOMAIN_JOIN_ERROR_INVALID_PARAMETER"

pattern FleetErrorCode_DOMAIN_JOIN_ERROR_LOGON_FAILURE :: FleetErrorCode
pattern FleetErrorCode_DOMAIN_JOIN_ERROR_LOGON_FAILURE = FleetErrorCode' "DOMAIN_JOIN_ERROR_LOGON_FAILURE"

pattern FleetErrorCode_DOMAIN_JOIN_ERROR_MORE_DATA :: FleetErrorCode
pattern FleetErrorCode_DOMAIN_JOIN_ERROR_MORE_DATA = FleetErrorCode' "DOMAIN_JOIN_ERROR_MORE_DATA"

pattern FleetErrorCode_DOMAIN_JOIN_ERROR_NOT_SUPPORTED :: FleetErrorCode
pattern FleetErrorCode_DOMAIN_JOIN_ERROR_NOT_SUPPORTED = FleetErrorCode' "DOMAIN_JOIN_ERROR_NOT_SUPPORTED"

pattern FleetErrorCode_DOMAIN_JOIN_ERROR_NO_SUCH_DOMAIN :: FleetErrorCode
pattern FleetErrorCode_DOMAIN_JOIN_ERROR_NO_SUCH_DOMAIN = FleetErrorCode' "DOMAIN_JOIN_ERROR_NO_SUCH_DOMAIN"

pattern FleetErrorCode_DOMAIN_JOIN_INTERNAL_SERVICE_ERROR :: FleetErrorCode
pattern FleetErrorCode_DOMAIN_JOIN_INTERNAL_SERVICE_ERROR = FleetErrorCode' "DOMAIN_JOIN_INTERNAL_SERVICE_ERROR"

pattern FleetErrorCode_DOMAIN_JOIN_NERR_INVALID_WORKGROUP_NAME :: FleetErrorCode
pattern FleetErrorCode_DOMAIN_JOIN_NERR_INVALID_WORKGROUP_NAME = FleetErrorCode' "DOMAIN_JOIN_NERR_INVALID_WORKGROUP_NAME"

pattern FleetErrorCode_DOMAIN_JOIN_NERR_PASSWORD_EXPIRED :: FleetErrorCode
pattern FleetErrorCode_DOMAIN_JOIN_NERR_PASSWORD_EXPIRED = FleetErrorCode' "DOMAIN_JOIN_NERR_PASSWORD_EXPIRED"

pattern FleetErrorCode_DOMAIN_JOIN_NERR_WORKSTATION_NOT_STARTED :: FleetErrorCode
pattern FleetErrorCode_DOMAIN_JOIN_NERR_WORKSTATION_NOT_STARTED = FleetErrorCode' "DOMAIN_JOIN_NERR_WORKSTATION_NOT_STARTED"

pattern FleetErrorCode_FLEET_INSTANCE_PROVISIONING_FAILURE :: FleetErrorCode
pattern FleetErrorCode_FLEET_INSTANCE_PROVISIONING_FAILURE = FleetErrorCode' "FLEET_INSTANCE_PROVISIONING_FAILURE"

pattern FleetErrorCode_FLEET_STOPPED :: FleetErrorCode
pattern FleetErrorCode_FLEET_STOPPED = FleetErrorCode' "FLEET_STOPPED"

pattern FleetErrorCode_IAM_SERVICE_ROLE_IS_MISSING :: FleetErrorCode
pattern FleetErrorCode_IAM_SERVICE_ROLE_IS_MISSING = FleetErrorCode' "IAM_SERVICE_ROLE_IS_MISSING"

pattern FleetErrorCode_IAM_SERVICE_ROLE_MISSING_DESCRIBE_SECURITY_GROUPS_ACTION :: FleetErrorCode
pattern FleetErrorCode_IAM_SERVICE_ROLE_MISSING_DESCRIBE_SECURITY_GROUPS_ACTION = FleetErrorCode' "IAM_SERVICE_ROLE_MISSING_DESCRIBE_SECURITY_GROUPS_ACTION"

pattern FleetErrorCode_IAM_SERVICE_ROLE_MISSING_DESCRIBE_SUBNET_ACTION :: FleetErrorCode
pattern FleetErrorCode_IAM_SERVICE_ROLE_MISSING_DESCRIBE_SUBNET_ACTION = FleetErrorCode' "IAM_SERVICE_ROLE_MISSING_DESCRIBE_SUBNET_ACTION"

pattern FleetErrorCode_IAM_SERVICE_ROLE_MISSING_ENI_CREATE_ACTION :: FleetErrorCode
pattern FleetErrorCode_IAM_SERVICE_ROLE_MISSING_ENI_CREATE_ACTION = FleetErrorCode' "IAM_SERVICE_ROLE_MISSING_ENI_CREATE_ACTION"

pattern FleetErrorCode_IAM_SERVICE_ROLE_MISSING_ENI_DELETE_ACTION :: FleetErrorCode
pattern FleetErrorCode_IAM_SERVICE_ROLE_MISSING_ENI_DELETE_ACTION = FleetErrorCode' "IAM_SERVICE_ROLE_MISSING_ENI_DELETE_ACTION"

pattern FleetErrorCode_IAM_SERVICE_ROLE_MISSING_ENI_DESCRIBE_ACTION :: FleetErrorCode
pattern FleetErrorCode_IAM_SERVICE_ROLE_MISSING_ENI_DESCRIBE_ACTION = FleetErrorCode' "IAM_SERVICE_ROLE_MISSING_ENI_DESCRIBE_ACTION"

pattern FleetErrorCode_IGW_NOT_ATTACHED :: FleetErrorCode
pattern FleetErrorCode_IGW_NOT_ATTACHED = FleetErrorCode' "IGW_NOT_ATTACHED"

pattern FleetErrorCode_IMAGE_NOT_FOUND :: FleetErrorCode
pattern FleetErrorCode_IMAGE_NOT_FOUND = FleetErrorCode' "IMAGE_NOT_FOUND"

pattern FleetErrorCode_INTERNAL_SERVICE_ERROR :: FleetErrorCode
pattern FleetErrorCode_INTERNAL_SERVICE_ERROR = FleetErrorCode' "INTERNAL_SERVICE_ERROR"

pattern FleetErrorCode_INVALID_SUBNET_CONFIGURATION :: FleetErrorCode
pattern FleetErrorCode_INVALID_SUBNET_CONFIGURATION = FleetErrorCode' "INVALID_SUBNET_CONFIGURATION"

pattern FleetErrorCode_MACHINE_ROLE_IS_MISSING :: FleetErrorCode
pattern FleetErrorCode_MACHINE_ROLE_IS_MISSING = FleetErrorCode' "MACHINE_ROLE_IS_MISSING"

pattern FleetErrorCode_NETWORK_INTERFACE_LIMIT_EXCEEDED :: FleetErrorCode
pattern FleetErrorCode_NETWORK_INTERFACE_LIMIT_EXCEEDED = FleetErrorCode' "NETWORK_INTERFACE_LIMIT_EXCEEDED"

pattern FleetErrorCode_SECURITY_GROUPS_NOT_FOUND :: FleetErrorCode
pattern FleetErrorCode_SECURITY_GROUPS_NOT_FOUND = FleetErrorCode' "SECURITY_GROUPS_NOT_FOUND"

pattern FleetErrorCode_STS_DISABLED_IN_REGION :: FleetErrorCode
pattern FleetErrorCode_STS_DISABLED_IN_REGION = FleetErrorCode' "STS_DISABLED_IN_REGION"

pattern FleetErrorCode_SUBNET_HAS_INSUFFICIENT_IP_ADDRESSES :: FleetErrorCode
pattern FleetErrorCode_SUBNET_HAS_INSUFFICIENT_IP_ADDRESSES = FleetErrorCode' "SUBNET_HAS_INSUFFICIENT_IP_ADDRESSES"

pattern FleetErrorCode_SUBNET_NOT_FOUND :: FleetErrorCode
pattern FleetErrorCode_SUBNET_NOT_FOUND = FleetErrorCode' "SUBNET_NOT_FOUND"

{-# COMPLETE
  FleetErrorCode_DOMAIN_JOIN_ERROR_ACCESS_DENIED,
  FleetErrorCode_DOMAIN_JOIN_ERROR_DS_MACHINE_ACCOUNT_QUOTA_EXCEEDED,
  FleetErrorCode_DOMAIN_JOIN_ERROR_FILE_NOT_FOUND,
  FleetErrorCode_DOMAIN_JOIN_ERROR_INVALID_PARAMETER,
  FleetErrorCode_DOMAIN_JOIN_ERROR_LOGON_FAILURE,
  FleetErrorCode_DOMAIN_JOIN_ERROR_MORE_DATA,
  FleetErrorCode_DOMAIN_JOIN_ERROR_NOT_SUPPORTED,
  FleetErrorCode_DOMAIN_JOIN_ERROR_NO_SUCH_DOMAIN,
  FleetErrorCode_DOMAIN_JOIN_INTERNAL_SERVICE_ERROR,
  FleetErrorCode_DOMAIN_JOIN_NERR_INVALID_WORKGROUP_NAME,
  FleetErrorCode_DOMAIN_JOIN_NERR_PASSWORD_EXPIRED,
  FleetErrorCode_DOMAIN_JOIN_NERR_WORKSTATION_NOT_STARTED,
  FleetErrorCode_FLEET_INSTANCE_PROVISIONING_FAILURE,
  FleetErrorCode_FLEET_STOPPED,
  FleetErrorCode_IAM_SERVICE_ROLE_IS_MISSING,
  FleetErrorCode_IAM_SERVICE_ROLE_MISSING_DESCRIBE_SECURITY_GROUPS_ACTION,
  FleetErrorCode_IAM_SERVICE_ROLE_MISSING_DESCRIBE_SUBNET_ACTION,
  FleetErrorCode_IAM_SERVICE_ROLE_MISSING_ENI_CREATE_ACTION,
  FleetErrorCode_IAM_SERVICE_ROLE_MISSING_ENI_DELETE_ACTION,
  FleetErrorCode_IAM_SERVICE_ROLE_MISSING_ENI_DESCRIBE_ACTION,
  FleetErrorCode_IGW_NOT_ATTACHED,
  FleetErrorCode_IMAGE_NOT_FOUND,
  FleetErrorCode_INTERNAL_SERVICE_ERROR,
  FleetErrorCode_INVALID_SUBNET_CONFIGURATION,
  FleetErrorCode_MACHINE_ROLE_IS_MISSING,
  FleetErrorCode_NETWORK_INTERFACE_LIMIT_EXCEEDED,
  FleetErrorCode_SECURITY_GROUPS_NOT_FOUND,
  FleetErrorCode_STS_DISABLED_IN_REGION,
  FleetErrorCode_SUBNET_HAS_INSUFFICIENT_IP_ADDRESSES,
  FleetErrorCode_SUBNET_NOT_FOUND,
  FleetErrorCode'
  #-}
