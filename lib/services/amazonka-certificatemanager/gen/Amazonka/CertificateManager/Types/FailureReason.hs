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
-- Module      : Amazonka.CertificateManager.Types.FailureReason
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManager.Types.FailureReason
  ( FailureReason
      ( ..,
        FailureReason_ADDITIONAL_VERIFICATION_REQUIRED,
        FailureReason_CAA_ERROR,
        FailureReason_DOMAIN_NOT_ALLOWED,
        FailureReason_DOMAIN_VALIDATION_DENIED,
        FailureReason_INVALID_PUBLIC_DOMAIN,
        FailureReason_NO_AVAILABLE_CONTACTS,
        FailureReason_OTHER,
        FailureReason_PCA_ACCESS_DENIED,
        FailureReason_PCA_INVALID_ARGS,
        FailureReason_PCA_INVALID_ARN,
        FailureReason_PCA_INVALID_DURATION,
        FailureReason_PCA_INVALID_STATE,
        FailureReason_PCA_LIMIT_EXCEEDED,
        FailureReason_PCA_NAME_CONSTRAINTS_VALIDATION,
        FailureReason_PCA_REQUEST_FAILED,
        FailureReason_PCA_RESOURCE_NOT_FOUND,
        FailureReason_SLR_NOT_FOUND
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FailureReason = FailureReason'
  { fromFailureReason ::
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

pattern FailureReason_ADDITIONAL_VERIFICATION_REQUIRED :: FailureReason
pattern FailureReason_ADDITIONAL_VERIFICATION_REQUIRED = FailureReason' "ADDITIONAL_VERIFICATION_REQUIRED"

pattern FailureReason_CAA_ERROR :: FailureReason
pattern FailureReason_CAA_ERROR = FailureReason' "CAA_ERROR"

pattern FailureReason_DOMAIN_NOT_ALLOWED :: FailureReason
pattern FailureReason_DOMAIN_NOT_ALLOWED = FailureReason' "DOMAIN_NOT_ALLOWED"

pattern FailureReason_DOMAIN_VALIDATION_DENIED :: FailureReason
pattern FailureReason_DOMAIN_VALIDATION_DENIED = FailureReason' "DOMAIN_VALIDATION_DENIED"

pattern FailureReason_INVALID_PUBLIC_DOMAIN :: FailureReason
pattern FailureReason_INVALID_PUBLIC_DOMAIN = FailureReason' "INVALID_PUBLIC_DOMAIN"

pattern FailureReason_NO_AVAILABLE_CONTACTS :: FailureReason
pattern FailureReason_NO_AVAILABLE_CONTACTS = FailureReason' "NO_AVAILABLE_CONTACTS"

pattern FailureReason_OTHER :: FailureReason
pattern FailureReason_OTHER = FailureReason' "OTHER"

pattern FailureReason_PCA_ACCESS_DENIED :: FailureReason
pattern FailureReason_PCA_ACCESS_DENIED = FailureReason' "PCA_ACCESS_DENIED"

pattern FailureReason_PCA_INVALID_ARGS :: FailureReason
pattern FailureReason_PCA_INVALID_ARGS = FailureReason' "PCA_INVALID_ARGS"

pattern FailureReason_PCA_INVALID_ARN :: FailureReason
pattern FailureReason_PCA_INVALID_ARN = FailureReason' "PCA_INVALID_ARN"

pattern FailureReason_PCA_INVALID_DURATION :: FailureReason
pattern FailureReason_PCA_INVALID_DURATION = FailureReason' "PCA_INVALID_DURATION"

pattern FailureReason_PCA_INVALID_STATE :: FailureReason
pattern FailureReason_PCA_INVALID_STATE = FailureReason' "PCA_INVALID_STATE"

pattern FailureReason_PCA_LIMIT_EXCEEDED :: FailureReason
pattern FailureReason_PCA_LIMIT_EXCEEDED = FailureReason' "PCA_LIMIT_EXCEEDED"

pattern FailureReason_PCA_NAME_CONSTRAINTS_VALIDATION :: FailureReason
pattern FailureReason_PCA_NAME_CONSTRAINTS_VALIDATION = FailureReason' "PCA_NAME_CONSTRAINTS_VALIDATION"

pattern FailureReason_PCA_REQUEST_FAILED :: FailureReason
pattern FailureReason_PCA_REQUEST_FAILED = FailureReason' "PCA_REQUEST_FAILED"

pattern FailureReason_PCA_RESOURCE_NOT_FOUND :: FailureReason
pattern FailureReason_PCA_RESOURCE_NOT_FOUND = FailureReason' "PCA_RESOURCE_NOT_FOUND"

pattern FailureReason_SLR_NOT_FOUND :: FailureReason
pattern FailureReason_SLR_NOT_FOUND = FailureReason' "SLR_NOT_FOUND"

{-# COMPLETE
  FailureReason_ADDITIONAL_VERIFICATION_REQUIRED,
  FailureReason_CAA_ERROR,
  FailureReason_DOMAIN_NOT_ALLOWED,
  FailureReason_DOMAIN_VALIDATION_DENIED,
  FailureReason_INVALID_PUBLIC_DOMAIN,
  FailureReason_NO_AVAILABLE_CONTACTS,
  FailureReason_OTHER,
  FailureReason_PCA_ACCESS_DENIED,
  FailureReason_PCA_INVALID_ARGS,
  FailureReason_PCA_INVALID_ARN,
  FailureReason_PCA_INVALID_DURATION,
  FailureReason_PCA_INVALID_STATE,
  FailureReason_PCA_LIMIT_EXCEEDED,
  FailureReason_PCA_NAME_CONSTRAINTS_VALIDATION,
  FailureReason_PCA_REQUEST_FAILED,
  FailureReason_PCA_RESOURCE_NOT_FOUND,
  FailureReason_SLR_NOT_FOUND,
  FailureReason'
  #-}
