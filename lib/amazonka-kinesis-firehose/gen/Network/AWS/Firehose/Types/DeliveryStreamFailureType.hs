{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.DeliveryStreamFailureType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.DeliveryStreamFailureType where

import Network.AWS.Prelude

data DeliveryStreamFailureType
  = CreateEniFailed
  | CreateKMSGrantFailed
  | DeleteEniFailed
  | DisabledKMSKey
  | EniAccessDenied
  | InvalidKMSKey
  | KMSAccessDenied
  | KMSKeyNotFound
  | KMSOptInRequired
  | RetireKMSGrantFailed
  | SecurityGroupAccessDenied
  | SecurityGroupNotFound
  | SubnetAccessDenied
  | SubnetNotFound
  | UnknownError
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

instance FromText DeliveryStreamFailureType where
  parser =
    takeLowerText >>= \case
      "create_eni_failed" -> pure CreateEniFailed
      "create_kms_grant_failed" -> pure CreateKMSGrantFailed
      "delete_eni_failed" -> pure DeleteEniFailed
      "disabled_kms_key" -> pure DisabledKMSKey
      "eni_access_denied" -> pure EniAccessDenied
      "invalid_kms_key" -> pure InvalidKMSKey
      "kms_access_denied" -> pure KMSAccessDenied
      "kms_key_not_found" -> pure KMSKeyNotFound
      "kms_opt_in_required" -> pure KMSOptInRequired
      "retire_kms_grant_failed" -> pure RetireKMSGrantFailed
      "security_group_access_denied" -> pure SecurityGroupAccessDenied
      "security_group_not_found" -> pure SecurityGroupNotFound
      "subnet_access_denied" -> pure SubnetAccessDenied
      "subnet_not_found" -> pure SubnetNotFound
      "unknown_error" -> pure UnknownError
      e ->
        fromTextError $
          "Failure parsing DeliveryStreamFailureType from value: '" <> e
            <> "'. Accepted values: create_eni_failed, create_kms_grant_failed, delete_eni_failed, disabled_kms_key, eni_access_denied, invalid_kms_key, kms_access_denied, kms_key_not_found, kms_opt_in_required, retire_kms_grant_failed, security_group_access_denied, security_group_not_found, subnet_access_denied, subnet_not_found, unknown_error"

instance ToText DeliveryStreamFailureType where
  toText = \case
    CreateEniFailed -> "CREATE_ENI_FAILED"
    CreateKMSGrantFailed -> "CREATE_KMS_GRANT_FAILED"
    DeleteEniFailed -> "DELETE_ENI_FAILED"
    DisabledKMSKey -> "DISABLED_KMS_KEY"
    EniAccessDenied -> "ENI_ACCESS_DENIED"
    InvalidKMSKey -> "INVALID_KMS_KEY"
    KMSAccessDenied -> "KMS_ACCESS_DENIED"
    KMSKeyNotFound -> "KMS_KEY_NOT_FOUND"
    KMSOptInRequired -> "KMS_OPT_IN_REQUIRED"
    RetireKMSGrantFailed -> "RETIRE_KMS_GRANT_FAILED"
    SecurityGroupAccessDenied -> "SECURITY_GROUP_ACCESS_DENIED"
    SecurityGroupNotFound -> "SECURITY_GROUP_NOT_FOUND"
    SubnetAccessDenied -> "SUBNET_ACCESS_DENIED"
    SubnetNotFound -> "SUBNET_NOT_FOUND"
    UnknownError -> "UNKNOWN_ERROR"

instance Hashable DeliveryStreamFailureType

instance NFData DeliveryStreamFailureType

instance ToByteString DeliveryStreamFailureType

instance ToQuery DeliveryStreamFailureType

instance ToHeader DeliveryStreamFailureType

instance FromJSON DeliveryStreamFailureType where
  parseJSON = parseJSONText "DeliveryStreamFailureType"
