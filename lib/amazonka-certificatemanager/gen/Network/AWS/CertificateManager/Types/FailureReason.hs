{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.FailureReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.FailureReason
  ( FailureReason
      ( FailureReason',
        AdditionalVerificationRequired,
        CaaError,
        DomainNotAllowed,
        DomainValidationDenied,
        InvalidPublicDomain,
        NoAvailableContacts,
        Other,
        PcaAccessDenied,
        PcaInvalidARN,
        PcaInvalidArgs,
        PcaInvalidDuration,
        PcaInvalidState,
        PcaLimitExceeded,
        PcaNameConstraintsValidation,
        PcaRequestFailed,
        PcaResourceNotFound,
        SlrNotFound
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype FailureReason = FailureReason' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern AdditionalVerificationRequired :: FailureReason
pattern AdditionalVerificationRequired = FailureReason' "ADDITIONAL_VERIFICATION_REQUIRED"

pattern CaaError :: FailureReason
pattern CaaError = FailureReason' "CAA_ERROR"

pattern DomainNotAllowed :: FailureReason
pattern DomainNotAllowed = FailureReason' "DOMAIN_NOT_ALLOWED"

pattern DomainValidationDenied :: FailureReason
pattern DomainValidationDenied = FailureReason' "DOMAIN_VALIDATION_DENIED"

pattern InvalidPublicDomain :: FailureReason
pattern InvalidPublicDomain = FailureReason' "INVALID_PUBLIC_DOMAIN"

pattern NoAvailableContacts :: FailureReason
pattern NoAvailableContacts = FailureReason' "NO_AVAILABLE_CONTACTS"

pattern Other :: FailureReason
pattern Other = FailureReason' "OTHER"

pattern PcaAccessDenied :: FailureReason
pattern PcaAccessDenied = FailureReason' "PCA_ACCESS_DENIED"

pattern PcaInvalidARN :: FailureReason
pattern PcaInvalidARN = FailureReason' "PCA_INVALID_ARN"

pattern PcaInvalidArgs :: FailureReason
pattern PcaInvalidArgs = FailureReason' "PCA_INVALID_ARGS"

pattern PcaInvalidDuration :: FailureReason
pattern PcaInvalidDuration = FailureReason' "PCA_INVALID_DURATION"

pattern PcaInvalidState :: FailureReason
pattern PcaInvalidState = FailureReason' "PCA_INVALID_STATE"

pattern PcaLimitExceeded :: FailureReason
pattern PcaLimitExceeded = FailureReason' "PCA_LIMIT_EXCEEDED"

pattern PcaNameConstraintsValidation :: FailureReason
pattern PcaNameConstraintsValidation = FailureReason' "PCA_NAME_CONSTRAINTS_VALIDATION"

pattern PcaRequestFailed :: FailureReason
pattern PcaRequestFailed = FailureReason' "PCA_REQUEST_FAILED"

pattern PcaResourceNotFound :: FailureReason
pattern PcaResourceNotFound = FailureReason' "PCA_RESOURCE_NOT_FOUND"

pattern SlrNotFound :: FailureReason
pattern SlrNotFound = FailureReason' "SLR_NOT_FOUND"

{-# COMPLETE
  AdditionalVerificationRequired,
  CaaError,
  DomainNotAllowed,
  DomainValidationDenied,
  InvalidPublicDomain,
  NoAvailableContacts,
  Other,
  PcaAccessDenied,
  PcaInvalidARN,
  PcaInvalidArgs,
  PcaInvalidDuration,
  PcaInvalidState,
  PcaLimitExceeded,
  PcaNameConstraintsValidation,
  PcaRequestFailed,
  PcaResourceNotFound,
  SlrNotFound,
  FailureReason'
  #-}
