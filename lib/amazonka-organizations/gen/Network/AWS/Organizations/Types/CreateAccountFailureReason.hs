{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.CreateAccountFailureReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Organizations.Types.CreateAccountFailureReason
  ( CreateAccountFailureReason
    ( CreateAccountFailureReason'
    , CreateAccountFailureReasonAccountLimitExceeded
    , CreateAccountFailureReasonEmailAlreadyExists
    , CreateAccountFailureReasonInvalidAddress
    , CreateAccountFailureReasonInvalidEmail
    , CreateAccountFailureReasonConcurrentAccountModification
    , CreateAccountFailureReasonInternalFailure
    , CreateAccountFailureReasonGovcloudAccountAlreadyExists
    , CreateAccountFailureReasonMissingBusinessValidation
    , CreateAccountFailureReasonMissingPaymentInstrument
    , fromCreateAccountFailureReason
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype CreateAccountFailureReason = CreateAccountFailureReason'{fromCreateAccountFailureReason
                                                                 :: Core.Text}
                                       deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                       Core.Generic)
                                       deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                         Core.ToJSONKey, Core.FromJSONKey,
                                                         Core.ToJSON, Core.FromJSON, Core.ToXML,
                                                         Core.FromXML, Core.ToText, Core.FromText,
                                                         Core.ToByteString, Core.ToQuery,
                                                         Core.ToHeader)

pattern CreateAccountFailureReasonAccountLimitExceeded :: CreateAccountFailureReason
pattern CreateAccountFailureReasonAccountLimitExceeded = CreateAccountFailureReason' "ACCOUNT_LIMIT_EXCEEDED"

pattern CreateAccountFailureReasonEmailAlreadyExists :: CreateAccountFailureReason
pattern CreateAccountFailureReasonEmailAlreadyExists = CreateAccountFailureReason' "EMAIL_ALREADY_EXISTS"

pattern CreateAccountFailureReasonInvalidAddress :: CreateAccountFailureReason
pattern CreateAccountFailureReasonInvalidAddress = CreateAccountFailureReason' "INVALID_ADDRESS"

pattern CreateAccountFailureReasonInvalidEmail :: CreateAccountFailureReason
pattern CreateAccountFailureReasonInvalidEmail = CreateAccountFailureReason' "INVALID_EMAIL"

pattern CreateAccountFailureReasonConcurrentAccountModification :: CreateAccountFailureReason
pattern CreateAccountFailureReasonConcurrentAccountModification = CreateAccountFailureReason' "CONCURRENT_ACCOUNT_MODIFICATION"

pattern CreateAccountFailureReasonInternalFailure :: CreateAccountFailureReason
pattern CreateAccountFailureReasonInternalFailure = CreateAccountFailureReason' "INTERNAL_FAILURE"

pattern CreateAccountFailureReasonGovcloudAccountAlreadyExists :: CreateAccountFailureReason
pattern CreateAccountFailureReasonGovcloudAccountAlreadyExists = CreateAccountFailureReason' "GOVCLOUD_ACCOUNT_ALREADY_EXISTS"

pattern CreateAccountFailureReasonMissingBusinessValidation :: CreateAccountFailureReason
pattern CreateAccountFailureReasonMissingBusinessValidation = CreateAccountFailureReason' "MISSING_BUSINESS_VALIDATION"

pattern CreateAccountFailureReasonMissingPaymentInstrument :: CreateAccountFailureReason
pattern CreateAccountFailureReasonMissingPaymentInstrument = CreateAccountFailureReason' "MISSING_PAYMENT_INSTRUMENT"

{-# COMPLETE 
  CreateAccountFailureReasonAccountLimitExceeded,

  CreateAccountFailureReasonEmailAlreadyExists,

  CreateAccountFailureReasonInvalidAddress,

  CreateAccountFailureReasonInvalidEmail,

  CreateAccountFailureReasonConcurrentAccountModification,

  CreateAccountFailureReasonInternalFailure,

  CreateAccountFailureReasonGovcloudAccountAlreadyExists,

  CreateAccountFailureReasonMissingBusinessValidation,

  CreateAccountFailureReasonMissingPaymentInstrument,
  CreateAccountFailureReason'
  #-}
