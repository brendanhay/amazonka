{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateStatus where

import Network.AWS.Prelude

data LoadBalancerTLSCertificateStatus
  = LBTCSExpired
  | LBTCSFailed
  | LBTCSInactive
  | LBTCSIssued
  | LBTCSPendingValidation
  | LBTCSRevoked
  | LBTCSUnknown
  | LBTCSValidationTimedOut
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

instance FromText LoadBalancerTLSCertificateStatus where
  parser =
    takeLowerText >>= \case
      "expired" -> pure LBTCSExpired
      "failed" -> pure LBTCSFailed
      "inactive" -> pure LBTCSInactive
      "issued" -> pure LBTCSIssued
      "pending_validation" -> pure LBTCSPendingValidation
      "revoked" -> pure LBTCSRevoked
      "unknown" -> pure LBTCSUnknown
      "validation_timed_out" -> pure LBTCSValidationTimedOut
      e ->
        fromTextError $
          "Failure parsing LoadBalancerTLSCertificateStatus from value: '" <> e
            <> "'. Accepted values: expired, failed, inactive, issued, pending_validation, revoked, unknown, validation_timed_out"

instance ToText LoadBalancerTLSCertificateStatus where
  toText = \case
    LBTCSExpired -> "EXPIRED"
    LBTCSFailed -> "FAILED"
    LBTCSInactive -> "INACTIVE"
    LBTCSIssued -> "ISSUED"
    LBTCSPendingValidation -> "PENDING_VALIDATION"
    LBTCSRevoked -> "REVOKED"
    LBTCSUnknown -> "UNKNOWN"
    LBTCSValidationTimedOut -> "VALIDATION_TIMED_OUT"

instance Hashable LoadBalancerTLSCertificateStatus

instance NFData LoadBalancerTLSCertificateStatus

instance ToByteString LoadBalancerTLSCertificateStatus

instance ToQuery LoadBalancerTLSCertificateStatus

instance ToHeader LoadBalancerTLSCertificateStatus

instance FromJSON LoadBalancerTLSCertificateStatus where
  parseJSON = parseJSONText "LoadBalancerTLSCertificateStatus"
