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
-- Module      : Network.AWS.KMS.Types.ConnectionErrorCodeType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.ConnectionErrorCodeType
  ( ConnectionErrorCodeType
      ( ..,
        ConnectionErrorCodeType_CLUSTER_NOT_FOUND,
        ConnectionErrorCodeType_INSUFFICIENT_CLOUDHSM_HSMS,
        ConnectionErrorCodeType_INTERNAL_ERROR,
        ConnectionErrorCodeType_INVALID_CREDENTIALS,
        ConnectionErrorCodeType_NETWORK_ERRORS,
        ConnectionErrorCodeType_SUBNET_NOT_FOUND,
        ConnectionErrorCodeType_USER_LOCKED_OUT,
        ConnectionErrorCodeType_USER_LOGGED_IN,
        ConnectionErrorCodeType_USER_NOT_FOUND
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ConnectionErrorCodeType = ConnectionErrorCodeType'
  { fromConnectionErrorCodeType ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern ConnectionErrorCodeType_CLUSTER_NOT_FOUND :: ConnectionErrorCodeType
pattern ConnectionErrorCodeType_CLUSTER_NOT_FOUND = ConnectionErrorCodeType' "CLUSTER_NOT_FOUND"

pattern ConnectionErrorCodeType_INSUFFICIENT_CLOUDHSM_HSMS :: ConnectionErrorCodeType
pattern ConnectionErrorCodeType_INSUFFICIENT_CLOUDHSM_HSMS = ConnectionErrorCodeType' "INSUFFICIENT_CLOUDHSM_HSMS"

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

{-# COMPLETE
  ConnectionErrorCodeType_CLUSTER_NOT_FOUND,
  ConnectionErrorCodeType_INSUFFICIENT_CLOUDHSM_HSMS,
  ConnectionErrorCodeType_INTERNAL_ERROR,
  ConnectionErrorCodeType_INVALID_CREDENTIALS,
  ConnectionErrorCodeType_NETWORK_ERRORS,
  ConnectionErrorCodeType_SUBNET_NOT_FOUND,
  ConnectionErrorCodeType_USER_LOCKED_OUT,
  ConnectionErrorCodeType_USER_LOGGED_IN,
  ConnectionErrorCodeType_USER_NOT_FOUND,
  ConnectionErrorCodeType'
  #-}
