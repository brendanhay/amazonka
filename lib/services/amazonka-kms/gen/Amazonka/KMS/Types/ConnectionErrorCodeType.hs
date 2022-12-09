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
-- Module      : Amazonka.KMS.Types.ConnectionErrorCodeType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KMS.Types.ConnectionErrorCodeType
  ( ConnectionErrorCodeType
      ( ..,
        ConnectionErrorCodeType_CLUSTER_NOT_FOUND,
        ConnectionErrorCodeType_INSUFFICIENT_CLOUDHSM_HSMS,
        ConnectionErrorCodeType_INSUFFICIENT_FREE_ADDRESSES_IN_SUBNET,
        ConnectionErrorCodeType_INTERNAL_ERROR,
        ConnectionErrorCodeType_INVALID_CREDENTIALS,
        ConnectionErrorCodeType_NETWORK_ERRORS,
        ConnectionErrorCodeType_SUBNET_NOT_FOUND,
        ConnectionErrorCodeType_USER_LOCKED_OUT,
        ConnectionErrorCodeType_USER_LOGGED_IN,
        ConnectionErrorCodeType_USER_NOT_FOUND,
        ConnectionErrorCodeType_XKS_PROXY_ACCESS_DENIED,
        ConnectionErrorCodeType_XKS_PROXY_INVALID_CONFIGURATION,
        ConnectionErrorCodeType_XKS_PROXY_INVALID_RESPONSE,
        ConnectionErrorCodeType_XKS_PROXY_INVALID_TLS_CONFIGURATION,
        ConnectionErrorCodeType_XKS_PROXY_NOT_REACHABLE,
        ConnectionErrorCodeType_XKS_PROXY_TIMED_OUT,
        ConnectionErrorCodeType_XKS_VPC_ENDPOINT_SERVICE_INVALID_CONFIGURATION,
        ConnectionErrorCodeType_XKS_VPC_ENDPOINT_SERVICE_NOT_FOUND
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ConnectionErrorCodeType = ConnectionErrorCodeType'
  { fromConnectionErrorCodeType ::
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

pattern ConnectionErrorCodeType_CLUSTER_NOT_FOUND :: ConnectionErrorCodeType
pattern ConnectionErrorCodeType_CLUSTER_NOT_FOUND = ConnectionErrorCodeType' "CLUSTER_NOT_FOUND"

pattern ConnectionErrorCodeType_INSUFFICIENT_CLOUDHSM_HSMS :: ConnectionErrorCodeType
pattern ConnectionErrorCodeType_INSUFFICIENT_CLOUDHSM_HSMS = ConnectionErrorCodeType' "INSUFFICIENT_CLOUDHSM_HSMS"

pattern ConnectionErrorCodeType_INSUFFICIENT_FREE_ADDRESSES_IN_SUBNET :: ConnectionErrorCodeType
pattern ConnectionErrorCodeType_INSUFFICIENT_FREE_ADDRESSES_IN_SUBNET = ConnectionErrorCodeType' "INSUFFICIENT_FREE_ADDRESSES_IN_SUBNET"

pattern ConnectionErrorCodeType_INTERNAL_ERROR :: ConnectionErrorCodeType
pattern ConnectionErrorCodeType_INTERNAL_ERROR = ConnectionErrorCodeType' "INTERNAL_ERROR"

pattern ConnectionErrorCodeType_INVALID_CREDENTIALS :: ConnectionErrorCodeType
pattern ConnectionErrorCodeType_INVALID_CREDENTIALS = ConnectionErrorCodeType' "INVALID_CREDENTIALS"

pattern ConnectionErrorCodeType_NETWORK_ERRORS :: ConnectionErrorCodeType
pattern ConnectionErrorCodeType_NETWORK_ERRORS = ConnectionErrorCodeType' "NETWORK_ERRORS"

pattern ConnectionErrorCodeType_SUBNET_NOT_FOUND :: ConnectionErrorCodeType
pattern ConnectionErrorCodeType_SUBNET_NOT_FOUND = ConnectionErrorCodeType' "SUBNET_NOT_FOUND"

pattern ConnectionErrorCodeType_USER_LOCKED_OUT :: ConnectionErrorCodeType
pattern ConnectionErrorCodeType_USER_LOCKED_OUT = ConnectionErrorCodeType' "USER_LOCKED_OUT"

pattern ConnectionErrorCodeType_USER_LOGGED_IN :: ConnectionErrorCodeType
pattern ConnectionErrorCodeType_USER_LOGGED_IN = ConnectionErrorCodeType' "USER_LOGGED_IN"

pattern ConnectionErrorCodeType_USER_NOT_FOUND :: ConnectionErrorCodeType
pattern ConnectionErrorCodeType_USER_NOT_FOUND = ConnectionErrorCodeType' "USER_NOT_FOUND"

pattern ConnectionErrorCodeType_XKS_PROXY_ACCESS_DENIED :: ConnectionErrorCodeType
pattern ConnectionErrorCodeType_XKS_PROXY_ACCESS_DENIED = ConnectionErrorCodeType' "XKS_PROXY_ACCESS_DENIED"

pattern ConnectionErrorCodeType_XKS_PROXY_INVALID_CONFIGURATION :: ConnectionErrorCodeType
pattern ConnectionErrorCodeType_XKS_PROXY_INVALID_CONFIGURATION = ConnectionErrorCodeType' "XKS_PROXY_INVALID_CONFIGURATION"

pattern ConnectionErrorCodeType_XKS_PROXY_INVALID_RESPONSE :: ConnectionErrorCodeType
pattern ConnectionErrorCodeType_XKS_PROXY_INVALID_RESPONSE = ConnectionErrorCodeType' "XKS_PROXY_INVALID_RESPONSE"

pattern ConnectionErrorCodeType_XKS_PROXY_INVALID_TLS_CONFIGURATION :: ConnectionErrorCodeType
pattern ConnectionErrorCodeType_XKS_PROXY_INVALID_TLS_CONFIGURATION = ConnectionErrorCodeType' "XKS_PROXY_INVALID_TLS_CONFIGURATION"

pattern ConnectionErrorCodeType_XKS_PROXY_NOT_REACHABLE :: ConnectionErrorCodeType
pattern ConnectionErrorCodeType_XKS_PROXY_NOT_REACHABLE = ConnectionErrorCodeType' "XKS_PROXY_NOT_REACHABLE"

pattern ConnectionErrorCodeType_XKS_PROXY_TIMED_OUT :: ConnectionErrorCodeType
pattern ConnectionErrorCodeType_XKS_PROXY_TIMED_OUT = ConnectionErrorCodeType' "XKS_PROXY_TIMED_OUT"

pattern ConnectionErrorCodeType_XKS_VPC_ENDPOINT_SERVICE_INVALID_CONFIGURATION :: ConnectionErrorCodeType
pattern ConnectionErrorCodeType_XKS_VPC_ENDPOINT_SERVICE_INVALID_CONFIGURATION = ConnectionErrorCodeType' "XKS_VPC_ENDPOINT_SERVICE_INVALID_CONFIGURATION"

pattern ConnectionErrorCodeType_XKS_VPC_ENDPOINT_SERVICE_NOT_FOUND :: ConnectionErrorCodeType
pattern ConnectionErrorCodeType_XKS_VPC_ENDPOINT_SERVICE_NOT_FOUND = ConnectionErrorCodeType' "XKS_VPC_ENDPOINT_SERVICE_NOT_FOUND"

{-# COMPLETE
  ConnectionErrorCodeType_CLUSTER_NOT_FOUND,
  ConnectionErrorCodeType_INSUFFICIENT_CLOUDHSM_HSMS,
  ConnectionErrorCodeType_INSUFFICIENT_FREE_ADDRESSES_IN_SUBNET,
  ConnectionErrorCodeType_INTERNAL_ERROR,
  ConnectionErrorCodeType_INVALID_CREDENTIALS,
  ConnectionErrorCodeType_NETWORK_ERRORS,
  ConnectionErrorCodeType_SUBNET_NOT_FOUND,
  ConnectionErrorCodeType_USER_LOCKED_OUT,
  ConnectionErrorCodeType_USER_LOGGED_IN,
  ConnectionErrorCodeType_USER_NOT_FOUND,
  ConnectionErrorCodeType_XKS_PROXY_ACCESS_DENIED,
  ConnectionErrorCodeType_XKS_PROXY_INVALID_CONFIGURATION,
  ConnectionErrorCodeType_XKS_PROXY_INVALID_RESPONSE,
  ConnectionErrorCodeType_XKS_PROXY_INVALID_TLS_CONFIGURATION,
  ConnectionErrorCodeType_XKS_PROXY_NOT_REACHABLE,
  ConnectionErrorCodeType_XKS_PROXY_TIMED_OUT,
  ConnectionErrorCodeType_XKS_VPC_ENDPOINT_SERVICE_INVALID_CONFIGURATION,
  ConnectionErrorCodeType_XKS_VPC_ENDPOINT_SERVICE_NOT_FOUND,
  ConnectionErrorCodeType'
  #-}
