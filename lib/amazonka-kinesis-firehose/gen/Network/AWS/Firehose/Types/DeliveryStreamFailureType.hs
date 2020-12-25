{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.DeliveryStreamFailureType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.DeliveryStreamFailureType
  ( DeliveryStreamFailureType
      ( DeliveryStreamFailureType',
        DeliveryStreamFailureTypeRetireKmsGrantFailed,
        DeliveryStreamFailureTypeCreateKmsGrantFailed,
        DeliveryStreamFailureTypeKmsAccessDenied,
        DeliveryStreamFailureTypeDisabledKmsKey,
        DeliveryStreamFailureTypeInvalidKmsKey,
        DeliveryStreamFailureTypeKmsKeyNotFound,
        DeliveryStreamFailureTypeKmsOptInRequired,
        DeliveryStreamFailureTypeCreateEniFailed,
        DeliveryStreamFailureTypeDeleteEniFailed,
        DeliveryStreamFailureTypeSubnetNotFound,
        DeliveryStreamFailureTypeSecurityGroupNotFound,
        DeliveryStreamFailureTypeEniAccessDenied,
        DeliveryStreamFailureTypeSubnetAccessDenied,
        DeliveryStreamFailureTypeSecurityGroupAccessDenied,
        DeliveryStreamFailureTypeUnknownError,
        fromDeliveryStreamFailureType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype DeliveryStreamFailureType = DeliveryStreamFailureType'
  { fromDeliveryStreamFailureType ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern DeliveryStreamFailureTypeRetireKmsGrantFailed :: DeliveryStreamFailureType
pattern DeliveryStreamFailureTypeRetireKmsGrantFailed = DeliveryStreamFailureType' "RETIRE_KMS_GRANT_FAILED"

pattern DeliveryStreamFailureTypeCreateKmsGrantFailed :: DeliveryStreamFailureType
pattern DeliveryStreamFailureTypeCreateKmsGrantFailed = DeliveryStreamFailureType' "CREATE_KMS_GRANT_FAILED"

pattern DeliveryStreamFailureTypeKmsAccessDenied :: DeliveryStreamFailureType
pattern DeliveryStreamFailureTypeKmsAccessDenied = DeliveryStreamFailureType' "KMS_ACCESS_DENIED"

pattern DeliveryStreamFailureTypeDisabledKmsKey :: DeliveryStreamFailureType
pattern DeliveryStreamFailureTypeDisabledKmsKey = DeliveryStreamFailureType' "DISABLED_KMS_KEY"

pattern DeliveryStreamFailureTypeInvalidKmsKey :: DeliveryStreamFailureType
pattern DeliveryStreamFailureTypeInvalidKmsKey = DeliveryStreamFailureType' "INVALID_KMS_KEY"

pattern DeliveryStreamFailureTypeKmsKeyNotFound :: DeliveryStreamFailureType
pattern DeliveryStreamFailureTypeKmsKeyNotFound = DeliveryStreamFailureType' "KMS_KEY_NOT_FOUND"

pattern DeliveryStreamFailureTypeKmsOptInRequired :: DeliveryStreamFailureType
pattern DeliveryStreamFailureTypeKmsOptInRequired = DeliveryStreamFailureType' "KMS_OPT_IN_REQUIRED"

pattern DeliveryStreamFailureTypeCreateEniFailed :: DeliveryStreamFailureType
pattern DeliveryStreamFailureTypeCreateEniFailed = DeliveryStreamFailureType' "CREATE_ENI_FAILED"

pattern DeliveryStreamFailureTypeDeleteEniFailed :: DeliveryStreamFailureType
pattern DeliveryStreamFailureTypeDeleteEniFailed = DeliveryStreamFailureType' "DELETE_ENI_FAILED"

pattern DeliveryStreamFailureTypeSubnetNotFound :: DeliveryStreamFailureType
pattern DeliveryStreamFailureTypeSubnetNotFound = DeliveryStreamFailureType' "SUBNET_NOT_FOUND"

pattern DeliveryStreamFailureTypeSecurityGroupNotFound :: DeliveryStreamFailureType
pattern DeliveryStreamFailureTypeSecurityGroupNotFound = DeliveryStreamFailureType' "SECURITY_GROUP_NOT_FOUND"

pattern DeliveryStreamFailureTypeEniAccessDenied :: DeliveryStreamFailureType
pattern DeliveryStreamFailureTypeEniAccessDenied = DeliveryStreamFailureType' "ENI_ACCESS_DENIED"

pattern DeliveryStreamFailureTypeSubnetAccessDenied :: DeliveryStreamFailureType
pattern DeliveryStreamFailureTypeSubnetAccessDenied = DeliveryStreamFailureType' "SUBNET_ACCESS_DENIED"

pattern DeliveryStreamFailureTypeSecurityGroupAccessDenied :: DeliveryStreamFailureType
pattern DeliveryStreamFailureTypeSecurityGroupAccessDenied = DeliveryStreamFailureType' "SECURITY_GROUP_ACCESS_DENIED"

pattern DeliveryStreamFailureTypeUnknownError :: DeliveryStreamFailureType
pattern DeliveryStreamFailureTypeUnknownError = DeliveryStreamFailureType' "UNKNOWN_ERROR"

{-# COMPLETE
  DeliveryStreamFailureTypeRetireKmsGrantFailed,
  DeliveryStreamFailureTypeCreateKmsGrantFailed,
  DeliveryStreamFailureTypeKmsAccessDenied,
  DeliveryStreamFailureTypeDisabledKmsKey,
  DeliveryStreamFailureTypeInvalidKmsKey,
  DeliveryStreamFailureTypeKmsKeyNotFound,
  DeliveryStreamFailureTypeKmsOptInRequired,
  DeliveryStreamFailureTypeCreateEniFailed,
  DeliveryStreamFailureTypeDeleteEniFailed,
  DeliveryStreamFailureTypeSubnetNotFound,
  DeliveryStreamFailureTypeSecurityGroupNotFound,
  DeliveryStreamFailureTypeEniAccessDenied,
  DeliveryStreamFailureTypeSubnetAccessDenied,
  DeliveryStreamFailureTypeSecurityGroupAccessDenied,
  DeliveryStreamFailureTypeUnknownError,
  DeliveryStreamFailureType'
  #-}
