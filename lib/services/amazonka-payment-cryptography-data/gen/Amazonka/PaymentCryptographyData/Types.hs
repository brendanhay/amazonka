{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.PaymentCryptographyData.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ThrottlingException,
    _ValidationException,
    _VerificationFailedException,

    -- * DukptDerivationType
    DukptDerivationType (..),

    -- * DukptEncryptionMode
    DukptEncryptionMode (..),

    -- * DukptKeyVariant
    DukptKeyVariant (..),

    -- * EncryptionMode
    EncryptionMode (..),

    -- * MacAlgorithm
    MacAlgorithm (..),

    -- * MajorKeyDerivationMode
    MajorKeyDerivationMode (..),

    -- * PaddingType
    PaddingType (..),

    -- * PinBlockFormatForPinData
    PinBlockFormatForPinData (..),

    -- * SessionKeyDerivationMode
    SessionKeyDerivationMode (..),

    -- * AmexCardSecurityCodeVersion1
    AmexCardSecurityCodeVersion1 (..),
    newAmexCardSecurityCodeVersion1,
    amexCardSecurityCodeVersion1_cardExpiryDate,

    -- * AmexCardSecurityCodeVersion2
    AmexCardSecurityCodeVersion2 (..),
    newAmexCardSecurityCodeVersion2,
    amexCardSecurityCodeVersion2_cardExpiryDate,
    amexCardSecurityCodeVersion2_serviceCode,

    -- * AsymmetricEncryptionAttributes
    AsymmetricEncryptionAttributes (..),
    newAsymmetricEncryptionAttributes,
    asymmetricEncryptionAttributes_paddingType,

    -- * CardGenerationAttributes
    CardGenerationAttributes (..),
    newCardGenerationAttributes,
    cardGenerationAttributes_amexCardSecurityCodeVersion1,
    cardGenerationAttributes_amexCardSecurityCodeVersion2,
    cardGenerationAttributes_cardHolderVerificationValue,
    cardGenerationAttributes_cardVerificationValue1,
    cardGenerationAttributes_cardVerificationValue2,
    cardGenerationAttributes_dynamicCardVerificationCode,
    cardGenerationAttributes_dynamicCardVerificationValue,

    -- * CardHolderVerificationValue
    CardHolderVerificationValue (..),
    newCardHolderVerificationValue,
    cardHolderVerificationValue_applicationTransactionCounter,
    cardHolderVerificationValue_panSequenceNumber,
    cardHolderVerificationValue_unpredictableNumber,

    -- * CardVerificationAttributes
    CardVerificationAttributes (..),
    newCardVerificationAttributes,
    cardVerificationAttributes_amexCardSecurityCodeVersion1,
    cardVerificationAttributes_amexCardSecurityCodeVersion2,
    cardVerificationAttributes_cardHolderVerificationValue,
    cardVerificationAttributes_cardVerificationValue1,
    cardVerificationAttributes_cardVerificationValue2,
    cardVerificationAttributes_discoverDynamicCardVerificationCode,
    cardVerificationAttributes_dynamicCardVerificationCode,
    cardVerificationAttributes_dynamicCardVerificationValue,

    -- * CardVerificationValue1
    CardVerificationValue1 (..),
    newCardVerificationValue1,
    cardVerificationValue1_cardExpiryDate,
    cardVerificationValue1_serviceCode,

    -- * CardVerificationValue2
    CardVerificationValue2 (..),
    newCardVerificationValue2,
    cardVerificationValue2_cardExpiryDate,

    -- * CryptogramAuthResponse
    CryptogramAuthResponse (..),
    newCryptogramAuthResponse,
    cryptogramAuthResponse_arpcMethod1,
    cryptogramAuthResponse_arpcMethod2,

    -- * CryptogramVerificationArpcMethod1
    CryptogramVerificationArpcMethod1 (..),
    newCryptogramVerificationArpcMethod1,
    cryptogramVerificationArpcMethod1_authResponseCode,

    -- * CryptogramVerificationArpcMethod2
    CryptogramVerificationArpcMethod2 (..),
    newCryptogramVerificationArpcMethod2,
    cryptogramVerificationArpcMethod2_proprietaryAuthenticationData,
    cryptogramVerificationArpcMethod2_cardStatusUpdate,

    -- * DiscoverDynamicCardVerificationCode
    DiscoverDynamicCardVerificationCode (..),
    newDiscoverDynamicCardVerificationCode,
    discoverDynamicCardVerificationCode_applicationTransactionCounter,
    discoverDynamicCardVerificationCode_cardExpiryDate,
    discoverDynamicCardVerificationCode_unpredictableNumber,

    -- * DukptAttributes
    DukptAttributes (..),
    newDukptAttributes,
    dukptAttributes_dukptDerivationType,
    dukptAttributes_keySerialNumber,

    -- * DukptDerivationAttributes
    DukptDerivationAttributes (..),
    newDukptDerivationAttributes,
    dukptDerivationAttributes_dukptKeyDerivationType,
    dukptDerivationAttributes_dukptKeyVariant,
    dukptDerivationAttributes_keySerialNumber,

    -- * DukptEncryptionAttributes
    DukptEncryptionAttributes (..),
    newDukptEncryptionAttributes,
    dukptEncryptionAttributes_dukptKeyDerivationType,
    dukptEncryptionAttributes_dukptKeyVariant,
    dukptEncryptionAttributes_initializationVector,
    dukptEncryptionAttributes_mode,
    dukptEncryptionAttributes_keySerialNumber,

    -- * DynamicCardVerificationCode
    DynamicCardVerificationCode (..),
    newDynamicCardVerificationCode,
    dynamicCardVerificationCode_applicationTransactionCounter,
    dynamicCardVerificationCode_panSequenceNumber,
    dynamicCardVerificationCode_trackData,
    dynamicCardVerificationCode_unpredictableNumber,

    -- * DynamicCardVerificationValue
    DynamicCardVerificationValue (..),
    newDynamicCardVerificationValue,
    dynamicCardVerificationValue_applicationTransactionCounter,
    dynamicCardVerificationValue_cardExpiryDate,
    dynamicCardVerificationValue_panSequenceNumber,
    dynamicCardVerificationValue_serviceCode,

    -- * EncryptionDecryptionAttributes
    EncryptionDecryptionAttributes (..),
    newEncryptionDecryptionAttributes,
    encryptionDecryptionAttributes_asymmetric,
    encryptionDecryptionAttributes_dukpt,
    encryptionDecryptionAttributes_symmetric,

    -- * Ibm3624NaturalPin
    Ibm3624NaturalPin (..),
    newIbm3624NaturalPin,
    ibm3624NaturalPin_decimalizationTable,
    ibm3624NaturalPin_pinValidationData,
    ibm3624NaturalPin_pinValidationDataPadCharacter,

    -- * Ibm3624PinFromOffset
    Ibm3624PinFromOffset (..),
    newIbm3624PinFromOffset,
    ibm3624PinFromOffset_decimalizationTable,
    ibm3624PinFromOffset_pinOffset,
    ibm3624PinFromOffset_pinValidationData,
    ibm3624PinFromOffset_pinValidationDataPadCharacter,

    -- * Ibm3624PinOffset
    Ibm3624PinOffset (..),
    newIbm3624PinOffset,
    ibm3624PinOffset_decimalizationTable,
    ibm3624PinOffset_encryptedPinBlock,
    ibm3624PinOffset_pinValidationData,
    ibm3624PinOffset_pinValidationDataPadCharacter,

    -- * Ibm3624PinVerification
    Ibm3624PinVerification (..),
    newIbm3624PinVerification,
    ibm3624PinVerification_decimalizationTable,
    ibm3624PinVerification_pinOffset,
    ibm3624PinVerification_pinValidationData,
    ibm3624PinVerification_pinValidationDataPadCharacter,

    -- * Ibm3624RandomPin
    Ibm3624RandomPin (..),
    newIbm3624RandomPin,
    ibm3624RandomPin_decimalizationTable,
    ibm3624RandomPin_pinValidationData,
    ibm3624RandomPin_pinValidationDataPadCharacter,

    -- * MacAlgorithmDukpt
    MacAlgorithmDukpt (..),
    newMacAlgorithmDukpt,
    macAlgorithmDukpt_dukptDerivationType,
    macAlgorithmDukpt_dukptKeyVariant,
    macAlgorithmDukpt_keySerialNumber,

    -- * MacAlgorithmEmv
    MacAlgorithmEmv (..),
    newMacAlgorithmEmv,
    macAlgorithmEmv_majorKeyDerivationMode,
    macAlgorithmEmv_panSequenceNumber,
    macAlgorithmEmv_primaryAccountNumber,
    macAlgorithmEmv_sessionKeyDerivationMode,
    macAlgorithmEmv_sessionKeyDerivationValue,

    -- * MacAttributes
    MacAttributes (..),
    newMacAttributes,
    macAttributes_algorithm,
    macAttributes_dukptCmac,
    macAttributes_dukptIso9797Algorithm1,
    macAttributes_dukptIso9797Algorithm3,
    macAttributes_emvMac,

    -- * PinData
    PinData (..),
    newPinData,
    pinData_pinOffset,
    pinData_verificationValue,

    -- * PinGenerationAttributes
    PinGenerationAttributes (..),
    newPinGenerationAttributes,
    pinGenerationAttributes_ibm3624NaturalPin,
    pinGenerationAttributes_ibm3624PinFromOffset,
    pinGenerationAttributes_ibm3624PinOffset,
    pinGenerationAttributes_ibm3624RandomPin,
    pinGenerationAttributes_visaPin,
    pinGenerationAttributes_visaPinVerificationValue,

    -- * PinVerificationAttributes
    PinVerificationAttributes (..),
    newPinVerificationAttributes,
    pinVerificationAttributes_ibm3624Pin,
    pinVerificationAttributes_visaPin,

    -- * ReEncryptionAttributes
    ReEncryptionAttributes (..),
    newReEncryptionAttributes,
    reEncryptionAttributes_dukpt,
    reEncryptionAttributes_symmetric,

    -- * SessionKeyAmex
    SessionKeyAmex (..),
    newSessionKeyAmex,
    sessionKeyAmex_panSequenceNumber,
    sessionKeyAmex_primaryAccountNumber,

    -- * SessionKeyDerivation
    SessionKeyDerivation (..),
    newSessionKeyDerivation,
    sessionKeyDerivation_amex,
    sessionKeyDerivation_emv2000,
    sessionKeyDerivation_emvCommon,
    sessionKeyDerivation_mastercard,
    sessionKeyDerivation_visa,

    -- * SessionKeyDerivationValue
    SessionKeyDerivationValue (..),
    newSessionKeyDerivationValue,
    sessionKeyDerivationValue_applicationCryptogram,
    sessionKeyDerivationValue_applicationTransactionCounter,

    -- * SessionKeyEmv2000
    SessionKeyEmv2000 (..),
    newSessionKeyEmv2000,
    sessionKeyEmv2000_applicationTransactionCounter,
    sessionKeyEmv2000_panSequenceNumber,
    sessionKeyEmv2000_primaryAccountNumber,

    -- * SessionKeyEmvCommon
    SessionKeyEmvCommon (..),
    newSessionKeyEmvCommon,
    sessionKeyEmvCommon_applicationTransactionCounter,
    sessionKeyEmvCommon_panSequenceNumber,
    sessionKeyEmvCommon_primaryAccountNumber,

    -- * SessionKeyMastercard
    SessionKeyMastercard (..),
    newSessionKeyMastercard,
    sessionKeyMastercard_applicationTransactionCounter,
    sessionKeyMastercard_panSequenceNumber,
    sessionKeyMastercard_primaryAccountNumber,
    sessionKeyMastercard_unpredictableNumber,

    -- * SessionKeyVisa
    SessionKeyVisa (..),
    newSessionKeyVisa,
    sessionKeyVisa_panSequenceNumber,
    sessionKeyVisa_primaryAccountNumber,

    -- * SymmetricEncryptionAttributes
    SymmetricEncryptionAttributes (..),
    newSymmetricEncryptionAttributes,
    symmetricEncryptionAttributes_initializationVector,
    symmetricEncryptionAttributes_paddingType,
    symmetricEncryptionAttributes_mode,

    -- * TranslationIsoFormats
    TranslationIsoFormats (..),
    newTranslationIsoFormats,
    translationIsoFormats_isoFormat0,
    translationIsoFormats_isoFormat1,
    translationIsoFormats_isoFormat3,
    translationIsoFormats_isoFormat4,

    -- * TranslationPinDataIsoFormat034
    TranslationPinDataIsoFormat034 (..),
    newTranslationPinDataIsoFormat034,
    translationPinDataIsoFormat034_primaryAccountNumber,

    -- * TranslationPinDataIsoFormat1
    TranslationPinDataIsoFormat1 (..),
    newTranslationPinDataIsoFormat1,

    -- * VisaPin
    VisaPin (..),
    newVisaPin,
    visaPin_pinVerificationKeyIndex,

    -- * VisaPinVerification
    VisaPinVerification (..),
    newVisaPinVerification,
    visaPinVerification_pinVerificationKeyIndex,
    visaPinVerification_verificationValue,

    -- * VisaPinVerificationValue
    VisaPinVerificationValue (..),
    newVisaPinVerificationValue,
    visaPinVerificationValue_encryptedPinBlock,
    visaPinVerificationValue_pinVerificationKeyIndex,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.PaymentCryptographyData.Types.AmexCardSecurityCodeVersion1
import Amazonka.PaymentCryptographyData.Types.AmexCardSecurityCodeVersion2
import Amazonka.PaymentCryptographyData.Types.AsymmetricEncryptionAttributes
import Amazonka.PaymentCryptographyData.Types.CardGenerationAttributes
import Amazonka.PaymentCryptographyData.Types.CardHolderVerificationValue
import Amazonka.PaymentCryptographyData.Types.CardVerificationAttributes
import Amazonka.PaymentCryptographyData.Types.CardVerificationValue1
import Amazonka.PaymentCryptographyData.Types.CardVerificationValue2
import Amazonka.PaymentCryptographyData.Types.CryptogramAuthResponse
import Amazonka.PaymentCryptographyData.Types.CryptogramVerificationArpcMethod1
import Amazonka.PaymentCryptographyData.Types.CryptogramVerificationArpcMethod2
import Amazonka.PaymentCryptographyData.Types.DiscoverDynamicCardVerificationCode
import Amazonka.PaymentCryptographyData.Types.DukptAttributes
import Amazonka.PaymentCryptographyData.Types.DukptDerivationAttributes
import Amazonka.PaymentCryptographyData.Types.DukptDerivationType
import Amazonka.PaymentCryptographyData.Types.DukptEncryptionAttributes
import Amazonka.PaymentCryptographyData.Types.DukptEncryptionMode
import Amazonka.PaymentCryptographyData.Types.DukptKeyVariant
import Amazonka.PaymentCryptographyData.Types.DynamicCardVerificationCode
import Amazonka.PaymentCryptographyData.Types.DynamicCardVerificationValue
import Amazonka.PaymentCryptographyData.Types.EncryptionDecryptionAttributes
import Amazonka.PaymentCryptographyData.Types.EncryptionMode
import Amazonka.PaymentCryptographyData.Types.Ibm3624NaturalPin
import Amazonka.PaymentCryptographyData.Types.Ibm3624PinFromOffset
import Amazonka.PaymentCryptographyData.Types.Ibm3624PinOffset
import Amazonka.PaymentCryptographyData.Types.Ibm3624PinVerification
import Amazonka.PaymentCryptographyData.Types.Ibm3624RandomPin
import Amazonka.PaymentCryptographyData.Types.MacAlgorithm
import Amazonka.PaymentCryptographyData.Types.MacAlgorithmDukpt
import Amazonka.PaymentCryptographyData.Types.MacAlgorithmEmv
import Amazonka.PaymentCryptographyData.Types.MacAttributes
import Amazonka.PaymentCryptographyData.Types.MajorKeyDerivationMode
import Amazonka.PaymentCryptographyData.Types.PaddingType
import Amazonka.PaymentCryptographyData.Types.PinBlockFormatForPinData
import Amazonka.PaymentCryptographyData.Types.PinData
import Amazonka.PaymentCryptographyData.Types.PinGenerationAttributes
import Amazonka.PaymentCryptographyData.Types.PinVerificationAttributes
import Amazonka.PaymentCryptographyData.Types.ReEncryptionAttributes
import Amazonka.PaymentCryptographyData.Types.SessionKeyAmex
import Amazonka.PaymentCryptographyData.Types.SessionKeyDerivation
import Amazonka.PaymentCryptographyData.Types.SessionKeyDerivationMode
import Amazonka.PaymentCryptographyData.Types.SessionKeyDerivationValue
import Amazonka.PaymentCryptographyData.Types.SessionKeyEmv2000
import Amazonka.PaymentCryptographyData.Types.SessionKeyEmvCommon
import Amazonka.PaymentCryptographyData.Types.SessionKeyMastercard
import Amazonka.PaymentCryptographyData.Types.SessionKeyVisa
import Amazonka.PaymentCryptographyData.Types.SymmetricEncryptionAttributes
import Amazonka.PaymentCryptographyData.Types.TranslationIsoFormats
import Amazonka.PaymentCryptographyData.Types.TranslationPinDataIsoFormat034
import Amazonka.PaymentCryptographyData.Types.TranslationPinDataIsoFormat1
import Amazonka.PaymentCryptographyData.Types.VisaPin
import Amazonka.PaymentCryptographyData.Types.VisaPinVerification
import Amazonka.PaymentCryptographyData.Types.VisaPinVerificationValue
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2022-02-03@ of the Amazon Payment Cryptography Data Plane SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev =
        "PaymentCryptographyData",
      Core.signer = Sign.v4,
      Core.endpointPrefix =
        "dataplane.payment-cryptography",
      Core.signingName = "payment-cryptography",
      Core.version = "2022-02-03",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error =
        Core.parseJSONError "PaymentCryptographyData",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | You do not have sufficient access to perform this action.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | The request processing has failed because of an unknown error,
-- exception, or failure.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The request was denied due to an invalid resource error.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The request was denied due to request throttling.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The request was denied due to an invalid request error.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"

-- | This request failed verification.
_VerificationFailedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_VerificationFailedException =
  Core._MatchServiceError
    defaultService
    "VerificationFailedException"
    Prelude.. Core.hasStatus 400
