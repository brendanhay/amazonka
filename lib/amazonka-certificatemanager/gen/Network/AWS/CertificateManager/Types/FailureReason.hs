{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.FailureReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.FailureReason where

import Network.AWS.Prelude

data FailureReason
  = AdditionalVerificationRequired
  | CaaError
  | DomainNotAllowed
  | DomainValidationDenied
  | InvalidPublicDomain
  | NoAvailableContacts
  | Other
  | PcaAccessDenied
  | PcaInvalidARN
  | PcaInvalidArgs
  | PcaInvalidDuration
  | PcaInvalidState
  | PcaLimitExceeded
  | PcaNameConstraintsValidation
  | PcaRequestFailed
  | PcaResourceNotFound
  | SlrNotFound
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

instance FromText FailureReason where
  parser =
    takeLowerText >>= \case
      "additional_verification_required" -> pure AdditionalVerificationRequired
      "caa_error" -> pure CaaError
      "domain_not_allowed" -> pure DomainNotAllowed
      "domain_validation_denied" -> pure DomainValidationDenied
      "invalid_public_domain" -> pure InvalidPublicDomain
      "no_available_contacts" -> pure NoAvailableContacts
      "other" -> pure Other
      "pca_access_denied" -> pure PcaAccessDenied
      "pca_invalid_arn" -> pure PcaInvalidARN
      "pca_invalid_args" -> pure PcaInvalidArgs
      "pca_invalid_duration" -> pure PcaInvalidDuration
      "pca_invalid_state" -> pure PcaInvalidState
      "pca_limit_exceeded" -> pure PcaLimitExceeded
      "pca_name_constraints_validation" -> pure PcaNameConstraintsValidation
      "pca_request_failed" -> pure PcaRequestFailed
      "pca_resource_not_found" -> pure PcaResourceNotFound
      "slr_not_found" -> pure SlrNotFound
      e ->
        fromTextError $
          "Failure parsing FailureReason from value: '" <> e
            <> "'. Accepted values: additional_verification_required, caa_error, domain_not_allowed, domain_validation_denied, invalid_public_domain, no_available_contacts, other, pca_access_denied, pca_invalid_arn, pca_invalid_args, pca_invalid_duration, pca_invalid_state, pca_limit_exceeded, pca_name_constraints_validation, pca_request_failed, pca_resource_not_found, slr_not_found"

instance ToText FailureReason where
  toText = \case
    AdditionalVerificationRequired -> "ADDITIONAL_VERIFICATION_REQUIRED"
    CaaError -> "CAA_ERROR"
    DomainNotAllowed -> "DOMAIN_NOT_ALLOWED"
    DomainValidationDenied -> "DOMAIN_VALIDATION_DENIED"
    InvalidPublicDomain -> "INVALID_PUBLIC_DOMAIN"
    NoAvailableContacts -> "NO_AVAILABLE_CONTACTS"
    Other -> "OTHER"
    PcaAccessDenied -> "PCA_ACCESS_DENIED"
    PcaInvalidARN -> "PCA_INVALID_ARN"
    PcaInvalidArgs -> "PCA_INVALID_ARGS"
    PcaInvalidDuration -> "PCA_INVALID_DURATION"
    PcaInvalidState -> "PCA_INVALID_STATE"
    PcaLimitExceeded -> "PCA_LIMIT_EXCEEDED"
    PcaNameConstraintsValidation -> "PCA_NAME_CONSTRAINTS_VALIDATION"
    PcaRequestFailed -> "PCA_REQUEST_FAILED"
    PcaResourceNotFound -> "PCA_RESOURCE_NOT_FOUND"
    SlrNotFound -> "SLR_NOT_FOUND"

instance Hashable FailureReason

instance NFData FailureReason

instance ToByteString FailureReason

instance ToQuery FailureReason

instance ToHeader FailureReason

instance FromJSON FailureReason where
  parseJSON = parseJSONText "FailureReason"
