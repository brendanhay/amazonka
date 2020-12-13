{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.CreateAccountFailureReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.CreateAccountFailureReason
  ( CreateAccountFailureReason
      ( CreateAccountFailureReason',
        AccountLimitExceeded,
        EmailAlreadyExists,
        InvalidAddress,
        InvalidEmail,
        ConcurrentAccountModification,
        InternalFailure,
        GovcloudAccountAlreadyExists,
        MissingBusinessValidation,
        MissingPaymentInstrument
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype CreateAccountFailureReason = CreateAccountFailureReason' Lude.Text
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

pattern AccountLimitExceeded :: CreateAccountFailureReason
pattern AccountLimitExceeded = CreateAccountFailureReason' "ACCOUNT_LIMIT_EXCEEDED"

pattern EmailAlreadyExists :: CreateAccountFailureReason
pattern EmailAlreadyExists = CreateAccountFailureReason' "EMAIL_ALREADY_EXISTS"

pattern InvalidAddress :: CreateAccountFailureReason
pattern InvalidAddress = CreateAccountFailureReason' "INVALID_ADDRESS"

pattern InvalidEmail :: CreateAccountFailureReason
pattern InvalidEmail = CreateAccountFailureReason' "INVALID_EMAIL"

pattern ConcurrentAccountModification :: CreateAccountFailureReason
pattern ConcurrentAccountModification = CreateAccountFailureReason' "CONCURRENT_ACCOUNT_MODIFICATION"

pattern InternalFailure :: CreateAccountFailureReason
pattern InternalFailure = CreateAccountFailureReason' "INTERNAL_FAILURE"

pattern GovcloudAccountAlreadyExists :: CreateAccountFailureReason
pattern GovcloudAccountAlreadyExists = CreateAccountFailureReason' "GOVCLOUD_ACCOUNT_ALREADY_EXISTS"

pattern MissingBusinessValidation :: CreateAccountFailureReason
pattern MissingBusinessValidation = CreateAccountFailureReason' "MISSING_BUSINESS_VALIDATION"

pattern MissingPaymentInstrument :: CreateAccountFailureReason
pattern MissingPaymentInstrument = CreateAccountFailureReason' "MISSING_PAYMENT_INSTRUMENT"

{-# COMPLETE
  AccountLimitExceeded,
  EmailAlreadyExists,
  InvalidAddress,
  InvalidEmail,
  ConcurrentAccountModification,
  InternalFailure,
  GovcloudAccountAlreadyExists,
  MissingBusinessValidation,
  MissingPaymentInstrument,
  CreateAccountFailureReason'
  #-}
