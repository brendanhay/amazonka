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
-- Module      : Amazonka.Organizations.Types.CreateAccountFailureReason
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Organizations.Types.CreateAccountFailureReason
  ( CreateAccountFailureReason
      ( ..,
        CreateAccountFailureReason_ACCOUNT_LIMIT_EXCEEDED,
        CreateAccountFailureReason_CONCURRENT_ACCOUNT_MODIFICATION,
        CreateAccountFailureReason_EMAIL_ALREADY_EXISTS,
        CreateAccountFailureReason_FAILED_BUSINESS_VALIDATION,
        CreateAccountFailureReason_GOVCLOUD_ACCOUNT_ALREADY_EXISTS,
        CreateAccountFailureReason_INTERNAL_FAILURE,
        CreateAccountFailureReason_INVALID_ADDRESS,
        CreateAccountFailureReason_INVALID_EMAIL,
        CreateAccountFailureReason_INVALID_IDENTITY_FOR_BUSINESS_VALIDATION,
        CreateAccountFailureReason_INVALID_PAYMENT_INSTRUMENT,
        CreateAccountFailureReason_MISSING_BUSINESS_VALIDATION,
        CreateAccountFailureReason_MISSING_PAYMENT_INSTRUMENT,
        CreateAccountFailureReason_PENDING_BUSINESS_VALIDATION,
        CreateAccountFailureReason_UNKNOWN_BUSINESS_VALIDATION,
        CreateAccountFailureReason_UPDATE_EXISTING_RESOURCE_POLICY_WITH_TAGS_NOT_SUPPORTED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CreateAccountFailureReason = CreateAccountFailureReason'
  { fromCreateAccountFailureReason ::
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

pattern CreateAccountFailureReason_ACCOUNT_LIMIT_EXCEEDED :: CreateAccountFailureReason
pattern CreateAccountFailureReason_ACCOUNT_LIMIT_EXCEEDED = CreateAccountFailureReason' "ACCOUNT_LIMIT_EXCEEDED"

pattern CreateAccountFailureReason_CONCURRENT_ACCOUNT_MODIFICATION :: CreateAccountFailureReason
pattern CreateAccountFailureReason_CONCURRENT_ACCOUNT_MODIFICATION = CreateAccountFailureReason' "CONCURRENT_ACCOUNT_MODIFICATION"

pattern CreateAccountFailureReason_EMAIL_ALREADY_EXISTS :: CreateAccountFailureReason
pattern CreateAccountFailureReason_EMAIL_ALREADY_EXISTS = CreateAccountFailureReason' "EMAIL_ALREADY_EXISTS"

pattern CreateAccountFailureReason_FAILED_BUSINESS_VALIDATION :: CreateAccountFailureReason
pattern CreateAccountFailureReason_FAILED_BUSINESS_VALIDATION = CreateAccountFailureReason' "FAILED_BUSINESS_VALIDATION"

pattern CreateAccountFailureReason_GOVCLOUD_ACCOUNT_ALREADY_EXISTS :: CreateAccountFailureReason
pattern CreateAccountFailureReason_GOVCLOUD_ACCOUNT_ALREADY_EXISTS = CreateAccountFailureReason' "GOVCLOUD_ACCOUNT_ALREADY_EXISTS"

pattern CreateAccountFailureReason_INTERNAL_FAILURE :: CreateAccountFailureReason
pattern CreateAccountFailureReason_INTERNAL_FAILURE = CreateAccountFailureReason' "INTERNAL_FAILURE"

pattern CreateAccountFailureReason_INVALID_ADDRESS :: CreateAccountFailureReason
pattern CreateAccountFailureReason_INVALID_ADDRESS = CreateAccountFailureReason' "INVALID_ADDRESS"

pattern CreateAccountFailureReason_INVALID_EMAIL :: CreateAccountFailureReason
pattern CreateAccountFailureReason_INVALID_EMAIL = CreateAccountFailureReason' "INVALID_EMAIL"

pattern CreateAccountFailureReason_INVALID_IDENTITY_FOR_BUSINESS_VALIDATION :: CreateAccountFailureReason
pattern CreateAccountFailureReason_INVALID_IDENTITY_FOR_BUSINESS_VALIDATION = CreateAccountFailureReason' "INVALID_IDENTITY_FOR_BUSINESS_VALIDATION"

pattern CreateAccountFailureReason_INVALID_PAYMENT_INSTRUMENT :: CreateAccountFailureReason
pattern CreateAccountFailureReason_INVALID_PAYMENT_INSTRUMENT = CreateAccountFailureReason' "INVALID_PAYMENT_INSTRUMENT"

pattern CreateAccountFailureReason_MISSING_BUSINESS_VALIDATION :: CreateAccountFailureReason
pattern CreateAccountFailureReason_MISSING_BUSINESS_VALIDATION = CreateAccountFailureReason' "MISSING_BUSINESS_VALIDATION"

pattern CreateAccountFailureReason_MISSING_PAYMENT_INSTRUMENT :: CreateAccountFailureReason
pattern CreateAccountFailureReason_MISSING_PAYMENT_INSTRUMENT = CreateAccountFailureReason' "MISSING_PAYMENT_INSTRUMENT"

pattern CreateAccountFailureReason_PENDING_BUSINESS_VALIDATION :: CreateAccountFailureReason
pattern CreateAccountFailureReason_PENDING_BUSINESS_VALIDATION = CreateAccountFailureReason' "PENDING_BUSINESS_VALIDATION"

pattern CreateAccountFailureReason_UNKNOWN_BUSINESS_VALIDATION :: CreateAccountFailureReason
pattern CreateAccountFailureReason_UNKNOWN_BUSINESS_VALIDATION = CreateAccountFailureReason' "UNKNOWN_BUSINESS_VALIDATION"

pattern CreateAccountFailureReason_UPDATE_EXISTING_RESOURCE_POLICY_WITH_TAGS_NOT_SUPPORTED :: CreateAccountFailureReason
pattern CreateAccountFailureReason_UPDATE_EXISTING_RESOURCE_POLICY_WITH_TAGS_NOT_SUPPORTED = CreateAccountFailureReason' "UPDATE_EXISTING_RESOURCE_POLICY_WITH_TAGS_NOT_SUPPORTED"

{-# COMPLETE
  CreateAccountFailureReason_ACCOUNT_LIMIT_EXCEEDED,
  CreateAccountFailureReason_CONCURRENT_ACCOUNT_MODIFICATION,
  CreateAccountFailureReason_EMAIL_ALREADY_EXISTS,
  CreateAccountFailureReason_FAILED_BUSINESS_VALIDATION,
  CreateAccountFailureReason_GOVCLOUD_ACCOUNT_ALREADY_EXISTS,
  CreateAccountFailureReason_INTERNAL_FAILURE,
  CreateAccountFailureReason_INVALID_ADDRESS,
  CreateAccountFailureReason_INVALID_EMAIL,
  CreateAccountFailureReason_INVALID_IDENTITY_FOR_BUSINESS_VALIDATION,
  CreateAccountFailureReason_INVALID_PAYMENT_INSTRUMENT,
  CreateAccountFailureReason_MISSING_BUSINESS_VALIDATION,
  CreateAccountFailureReason_MISSING_PAYMENT_INSTRUMENT,
  CreateAccountFailureReason_PENDING_BUSINESS_VALIDATION,
  CreateAccountFailureReason_UNKNOWN_BUSINESS_VALIDATION,
  CreateAccountFailureReason_UPDATE_EXISTING_RESOURCE_POLICY_WITH_TAGS_NOT_SUPPORTED,
  CreateAccountFailureReason'
  #-}
