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
-- Module      : Network.AWS.Firehose.Types.DeliveryStreamFailureType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.DeliveryStreamFailureType
  ( DeliveryStreamFailureType
      ( ..,
        DeliveryStreamFailureType_CREATE_ENI_FAILED,
        DeliveryStreamFailureType_CREATE_KMS_GRANT_FAILED,
        DeliveryStreamFailureType_DELETE_ENI_FAILED,
        DeliveryStreamFailureType_DISABLED_KMS_KEY,
        DeliveryStreamFailureType_ENI_ACCESS_DENIED,
        DeliveryStreamFailureType_INVALID_KMS_KEY,
        DeliveryStreamFailureType_KMS_ACCESS_DENIED,
        DeliveryStreamFailureType_KMS_KEY_NOT_FOUND,
        DeliveryStreamFailureType_KMS_OPT_IN_REQUIRED,
        DeliveryStreamFailureType_RETIRE_KMS_GRANT_FAILED,
        DeliveryStreamFailureType_SECURITY_GROUP_ACCESS_DENIED,
        DeliveryStreamFailureType_SECURITY_GROUP_NOT_FOUND,
        DeliveryStreamFailureType_SUBNET_ACCESS_DENIED,
        DeliveryStreamFailureType_SUBNET_NOT_FOUND,
        DeliveryStreamFailureType_UNKNOWN_ERROR
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype DeliveryStreamFailureType = DeliveryStreamFailureType'
  { fromDeliveryStreamFailureType ::
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

pattern DeliveryStreamFailureType_CREATE_ENI_FAILED :: DeliveryStreamFailureType
pattern DeliveryStreamFailureType_CREATE_ENI_FAILED = DeliveryStreamFailureType' "CREATE_ENI_FAILED"

pattern DeliveryStreamFailureType_CREATE_KMS_GRANT_FAILED :: DeliveryStreamFailureType
pattern DeliveryStreamFailureType_CREATE_KMS_GRANT_FAILED = DeliveryStreamFailureType' "CREATE_KMS_GRANT_FAILED"

pattern DeliveryStreamFailureType_DELETE_ENI_FAILED :: DeliveryStreamFailureType
pattern DeliveryStreamFailureType_DELETE_ENI_FAILED = DeliveryStreamFailureType' "DELETE_ENI_FAILED"

pattern DeliveryStreamFailureType_DISABLED_KMS_KEY :: DeliveryStreamFailureType
pattern DeliveryStreamFailureType_DISABLED_KMS_KEY = DeliveryStreamFailureType' "DISABLED_KMS_KEY"

pattern DeliveryStreamFailureType_ENI_ACCESS_DENIED :: DeliveryStreamFailureType
pattern DeliveryStreamFailureType_ENI_ACCESS_DENIED = DeliveryStreamFailureType' "ENI_ACCESS_DENIED"

pattern DeliveryStreamFailureType_INVALID_KMS_KEY :: DeliveryStreamFailureType
pattern DeliveryStreamFailureType_INVALID_KMS_KEY = DeliveryStreamFailureType' "INVALID_KMS_KEY"

pattern DeliveryStreamFailureType_KMS_ACCESS_DENIED :: DeliveryStreamFailureType
pattern DeliveryStreamFailureType_KMS_ACCESS_DENIED = DeliveryStreamFailureType' "KMS_ACCESS_DENIED"

pattern DeliveryStreamFailureType_KMS_KEY_NOT_FOUND :: DeliveryStreamFailureType
pattern DeliveryStreamFailureType_KMS_KEY_NOT_FOUND = DeliveryStreamFailureType' "KMS_KEY_NOT_FOUND"

pattern DeliveryStreamFailureType_KMS_OPT_IN_REQUIRED :: DeliveryStreamFailureType
pattern DeliveryStreamFailureType_KMS_OPT_IN_REQUIRED = DeliveryStreamFailureType' "KMS_OPT_IN_REQUIRED"

pattern DeliveryStreamFailureType_RETIRE_KMS_GRANT_FAILED :: DeliveryStreamFailureType
pattern DeliveryStreamFailureType_RETIRE_KMS_GRANT_FAILED = DeliveryStreamFailureType' "RETIRE_KMS_GRANT_FAILED"

pattern DeliveryStreamFailureType_SECURITY_GROUP_ACCESS_DENIED :: DeliveryStreamFailureType
pattern DeliveryStreamFailureType_SECURITY_GROUP_ACCESS_DENIED = DeliveryStreamFailureType' "SECURITY_GROUP_ACCESS_DENIED"

pattern DeliveryStreamFailureType_SECURITY_GROUP_NOT_FOUND :: DeliveryStreamFailureType
pattern DeliveryStreamFailureType_SECURITY_GROUP_NOT_FOUND = DeliveryStreamFailureType' "SECURITY_GROUP_NOT_FOUND"

pattern DeliveryStreamFailureType_SUBNET_ACCESS_DENIED :: DeliveryStreamFailureType
pattern DeliveryStreamFailureType_SUBNET_ACCESS_DENIED = DeliveryStreamFailureType' "SUBNET_ACCESS_DENIED"

pattern DeliveryStreamFailureType_SUBNET_NOT_FOUND :: DeliveryStreamFailureType
pattern DeliveryStreamFailureType_SUBNET_NOT_FOUND = DeliveryStreamFailureType' "SUBNET_NOT_FOUND"

pattern DeliveryStreamFailureType_UNKNOWN_ERROR :: DeliveryStreamFailureType
pattern DeliveryStreamFailureType_UNKNOWN_ERROR = DeliveryStreamFailureType' "UNKNOWN_ERROR"

{-# COMPLETE
  DeliveryStreamFailureType_CREATE_ENI_FAILED,
  DeliveryStreamFailureType_CREATE_KMS_GRANT_FAILED,
  DeliveryStreamFailureType_DELETE_ENI_FAILED,
  DeliveryStreamFailureType_DISABLED_KMS_KEY,
  DeliveryStreamFailureType_ENI_ACCESS_DENIED,
  DeliveryStreamFailureType_INVALID_KMS_KEY,
  DeliveryStreamFailureType_KMS_ACCESS_DENIED,
  DeliveryStreamFailureType_KMS_KEY_NOT_FOUND,
  DeliveryStreamFailureType_KMS_OPT_IN_REQUIRED,
  DeliveryStreamFailureType_RETIRE_KMS_GRANT_FAILED,
  DeliveryStreamFailureType_SECURITY_GROUP_ACCESS_DENIED,
  DeliveryStreamFailureType_SECURITY_GROUP_NOT_FOUND,
  DeliveryStreamFailureType_SUBNET_ACCESS_DENIED,
  DeliveryStreamFailureType_SUBNET_NOT_FOUND,
  DeliveryStreamFailureType_UNKNOWN_ERROR,
  DeliveryStreamFailureType'
  #-}
