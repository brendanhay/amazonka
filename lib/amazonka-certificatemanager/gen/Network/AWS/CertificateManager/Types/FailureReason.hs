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
        FailureReasonNoAvailableContacts,
        FailureReasonAdditionalVerificationRequired,
        FailureReasonDomainNotAllowed,
        FailureReasonInvalidPublicDomain,
        FailureReasonDomainValidationDenied,
        FailureReasonCaaError,
        FailureReasonPcaLimitExceeded,
        FailureReasonPcaInvalidArn,
        FailureReasonPcaInvalidState,
        FailureReasonPcaRequestFailed,
        FailureReasonPcaNameConstraintsValidation,
        FailureReasonPcaResourceNotFound,
        FailureReasonPcaInvalidArgs,
        FailureReasonPcaInvalidDuration,
        FailureReasonPcaAccessDenied,
        FailureReasonSlrNotFound,
        FailureReasonOther,
        fromFailureReason
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype FailureReason = FailureReason'
  { fromFailureReason ::
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

pattern FailureReasonNoAvailableContacts :: FailureReason
pattern FailureReasonNoAvailableContacts = FailureReason' "NO_AVAILABLE_CONTACTS"

pattern FailureReasonAdditionalVerificationRequired :: FailureReason
pattern FailureReasonAdditionalVerificationRequired = FailureReason' "ADDITIONAL_VERIFICATION_REQUIRED"

pattern FailureReasonDomainNotAllowed :: FailureReason
pattern FailureReasonDomainNotAllowed = FailureReason' "DOMAIN_NOT_ALLOWED"

pattern FailureReasonInvalidPublicDomain :: FailureReason
pattern FailureReasonInvalidPublicDomain = FailureReason' "INVALID_PUBLIC_DOMAIN"

pattern FailureReasonDomainValidationDenied :: FailureReason
pattern FailureReasonDomainValidationDenied = FailureReason' "DOMAIN_VALIDATION_DENIED"

pattern FailureReasonCaaError :: FailureReason
pattern FailureReasonCaaError = FailureReason' "CAA_ERROR"

pattern FailureReasonPcaLimitExceeded :: FailureReason
pattern FailureReasonPcaLimitExceeded = FailureReason' "PCA_LIMIT_EXCEEDED"

pattern FailureReasonPcaInvalidArn :: FailureReason
pattern FailureReasonPcaInvalidArn = FailureReason' "PCA_INVALID_ARN"

pattern FailureReasonPcaInvalidState :: FailureReason
pattern FailureReasonPcaInvalidState = FailureReason' "PCA_INVALID_STATE"

pattern FailureReasonPcaRequestFailed :: FailureReason
pattern FailureReasonPcaRequestFailed = FailureReason' "PCA_REQUEST_FAILED"

pattern FailureReasonPcaNameConstraintsValidation :: FailureReason
pattern FailureReasonPcaNameConstraintsValidation = FailureReason' "PCA_NAME_CONSTRAINTS_VALIDATION"

pattern FailureReasonPcaResourceNotFound :: FailureReason
pattern FailureReasonPcaResourceNotFound = FailureReason' "PCA_RESOURCE_NOT_FOUND"

pattern FailureReasonPcaInvalidArgs :: FailureReason
pattern FailureReasonPcaInvalidArgs = FailureReason' "PCA_INVALID_ARGS"

pattern FailureReasonPcaInvalidDuration :: FailureReason
pattern FailureReasonPcaInvalidDuration = FailureReason' "PCA_INVALID_DURATION"

pattern FailureReasonPcaAccessDenied :: FailureReason
pattern FailureReasonPcaAccessDenied = FailureReason' "PCA_ACCESS_DENIED"

pattern FailureReasonSlrNotFound :: FailureReason
pattern FailureReasonSlrNotFound = FailureReason' "SLR_NOT_FOUND"

pattern FailureReasonOther :: FailureReason
pattern FailureReasonOther = FailureReason' "OTHER"

{-# COMPLETE
  FailureReasonNoAvailableContacts,
  FailureReasonAdditionalVerificationRequired,
  FailureReasonDomainNotAllowed,
  FailureReasonInvalidPublicDomain,
  FailureReasonDomainValidationDenied,
  FailureReasonCaaError,
  FailureReasonPcaLimitExceeded,
  FailureReasonPcaInvalidArn,
  FailureReasonPcaInvalidState,
  FailureReasonPcaRequestFailed,
  FailureReasonPcaNameConstraintsValidation,
  FailureReasonPcaResourceNotFound,
  FailureReasonPcaInvalidArgs,
  FailureReasonPcaInvalidDuration,
  FailureReasonPcaAccessDenied,
  FailureReasonSlrNotFound,
  FailureReasonOther,
  FailureReason'
  #-}
