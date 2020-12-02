{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.CertificateStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.CertificateStatus where

import Network.AWS.Prelude

data CertificateStatus
  = CSExpired
  | CSFailed
  | CSInactive
  | CSIssued
  | CSPendingValidation
  | CSRevoked
  | CSValidationTimedOut
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

instance FromText CertificateStatus where
  parser =
    takeLowerText >>= \case
      "expired" -> pure CSExpired
      "failed" -> pure CSFailed
      "inactive" -> pure CSInactive
      "issued" -> pure CSIssued
      "pending_validation" -> pure CSPendingValidation
      "revoked" -> pure CSRevoked
      "validation_timed_out" -> pure CSValidationTimedOut
      e ->
        fromTextError $
          "Failure parsing CertificateStatus from value: '" <> e
            <> "'. Accepted values: expired, failed, inactive, issued, pending_validation, revoked, validation_timed_out"

instance ToText CertificateStatus where
  toText = \case
    CSExpired -> "EXPIRED"
    CSFailed -> "FAILED"
    CSInactive -> "INACTIVE"
    CSIssued -> "ISSUED"
    CSPendingValidation -> "PENDING_VALIDATION"
    CSRevoked -> "REVOKED"
    CSValidationTimedOut -> "VALIDATION_TIMED_OUT"

instance Hashable CertificateStatus

instance NFData CertificateStatus

instance ToByteString CertificateStatus

instance ToQuery CertificateStatus

instance ToHeader CertificateStatus

instance ToJSON CertificateStatus where
  toJSON = toJSONText

instance FromJSON CertificateStatus where
  parseJSON = parseJSONText "CertificateStatus"
