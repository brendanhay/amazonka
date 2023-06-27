{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.PaymentCryptographyData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2022-02-03@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- You use the Amazon Web Services Payment Cryptography Data Plane to
-- manage how encryption keys are used for payment-related transaction
-- processing and associated cryptographic operations. You can encrypt,
-- decrypt, generate, verify, and translate payment-related cryptographic
-- operations in Amazon Web Services Payment Cryptography. For more
-- information, see
-- <https://docs.aws.amazon.com/payment-cryptography/latest/userguide/data-operations.html Data operations>
-- in the /Amazon Web Services Payment Cryptography User Guide/.
--
-- To manage your encryption keys, you use the
-- <https://docs.aws.amazon.com/payment-cryptography/latest/APIReference/Welcome.html Amazon Web Services Payment Cryptography Control Plane>.
-- You can create, import, export, share, manage, and delete keys. You can
-- also manage Identity and Access Management (IAM) policies for keys.
module Amazonka.PaymentCryptographyData
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- ** VerificationFailedException
    _VerificationFailedException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DecryptData
    DecryptData (DecryptData'),
    newDecryptData,
    DecryptDataResponse (DecryptDataResponse'),
    newDecryptDataResponse,

    -- ** EncryptData
    EncryptData (EncryptData'),
    newEncryptData,
    EncryptDataResponse (EncryptDataResponse'),
    newEncryptDataResponse,

    -- ** GenerateCardValidationData
    GenerateCardValidationData (GenerateCardValidationData'),
    newGenerateCardValidationData,
    GenerateCardValidationDataResponse (GenerateCardValidationDataResponse'),
    newGenerateCardValidationDataResponse,

    -- ** GenerateMac
    GenerateMac (GenerateMac'),
    newGenerateMac,
    GenerateMacResponse (GenerateMacResponse'),
    newGenerateMacResponse,

    -- ** GeneratePinData
    GeneratePinData (GeneratePinData'),
    newGeneratePinData,
    GeneratePinDataResponse (GeneratePinDataResponse'),
    newGeneratePinDataResponse,

    -- ** ReEncryptData
    ReEncryptData (ReEncryptData'),
    newReEncryptData,
    ReEncryptDataResponse (ReEncryptDataResponse'),
    newReEncryptDataResponse,

    -- ** TranslatePinData
    TranslatePinData (TranslatePinData'),
    newTranslatePinData,
    TranslatePinDataResponse (TranslatePinDataResponse'),
    newTranslatePinDataResponse,

    -- ** VerifyAuthRequestCryptogram
    VerifyAuthRequestCryptogram (VerifyAuthRequestCryptogram'),
    newVerifyAuthRequestCryptogram,
    VerifyAuthRequestCryptogramResponse (VerifyAuthRequestCryptogramResponse'),
    newVerifyAuthRequestCryptogramResponse,

    -- ** VerifyCardValidationData
    VerifyCardValidationData (VerifyCardValidationData'),
    newVerifyCardValidationData,
    VerifyCardValidationDataResponse (VerifyCardValidationDataResponse'),
    newVerifyCardValidationDataResponse,

    -- ** VerifyMac
    VerifyMac (VerifyMac'),
    newVerifyMac,
    VerifyMacResponse (VerifyMacResponse'),
    newVerifyMacResponse,

    -- ** VerifyPinData
    VerifyPinData (VerifyPinData'),
    newVerifyPinData,
    VerifyPinDataResponse (VerifyPinDataResponse'),
    newVerifyPinDataResponse,

    -- * Types

    -- ** DukptDerivationType
    DukptDerivationType (..),

    -- ** DukptEncryptionMode
    DukptEncryptionMode (..),

    -- ** DukptKeyVariant
    DukptKeyVariant (..),

    -- ** EncryptionMode
    EncryptionMode (..),

    -- ** MacAlgorithm
    MacAlgorithm (..),

    -- ** MajorKeyDerivationMode
    MajorKeyDerivationMode (..),

    -- ** PaddingType
    PaddingType (..),

    -- ** PinBlockFormatForPinData
    PinBlockFormatForPinData (..),

    -- ** SessionKeyDerivationMode
    SessionKeyDerivationMode (..),

    -- ** AmexCardSecurityCodeVersion1
    AmexCardSecurityCodeVersion1 (AmexCardSecurityCodeVersion1'),
    newAmexCardSecurityCodeVersion1,

    -- ** AmexCardSecurityCodeVersion2
    AmexCardSecurityCodeVersion2 (AmexCardSecurityCodeVersion2'),
    newAmexCardSecurityCodeVersion2,

    -- ** AsymmetricEncryptionAttributes
    AsymmetricEncryptionAttributes (AsymmetricEncryptionAttributes'),
    newAsymmetricEncryptionAttributes,

    -- ** CardGenerationAttributes
    CardGenerationAttributes (CardGenerationAttributes'),
    newCardGenerationAttributes,

    -- ** CardHolderVerificationValue
    CardHolderVerificationValue (CardHolderVerificationValue'),
    newCardHolderVerificationValue,

    -- ** CardVerificationAttributes
    CardVerificationAttributes (CardVerificationAttributes'),
    newCardVerificationAttributes,

    -- ** CardVerificationValue1
    CardVerificationValue1 (CardVerificationValue1'),
    newCardVerificationValue1,

    -- ** CardVerificationValue2
    CardVerificationValue2 (CardVerificationValue2'),
    newCardVerificationValue2,

    -- ** CryptogramAuthResponse
    CryptogramAuthResponse (CryptogramAuthResponse'),
    newCryptogramAuthResponse,

    -- ** CryptogramVerificationArpcMethod1
    CryptogramVerificationArpcMethod1 (CryptogramVerificationArpcMethod1'),
    newCryptogramVerificationArpcMethod1,

    -- ** CryptogramVerificationArpcMethod2
    CryptogramVerificationArpcMethod2 (CryptogramVerificationArpcMethod2'),
    newCryptogramVerificationArpcMethod2,

    -- ** DiscoverDynamicCardVerificationCode
    DiscoverDynamicCardVerificationCode (DiscoverDynamicCardVerificationCode'),
    newDiscoverDynamicCardVerificationCode,

    -- ** DukptAttributes
    DukptAttributes (DukptAttributes'),
    newDukptAttributes,

    -- ** DukptDerivationAttributes
    DukptDerivationAttributes (DukptDerivationAttributes'),
    newDukptDerivationAttributes,

    -- ** DukptEncryptionAttributes
    DukptEncryptionAttributes (DukptEncryptionAttributes'),
    newDukptEncryptionAttributes,

    -- ** DynamicCardVerificationCode
    DynamicCardVerificationCode (DynamicCardVerificationCode'),
    newDynamicCardVerificationCode,

    -- ** DynamicCardVerificationValue
    DynamicCardVerificationValue (DynamicCardVerificationValue'),
    newDynamicCardVerificationValue,

    -- ** EncryptionDecryptionAttributes
    EncryptionDecryptionAttributes (EncryptionDecryptionAttributes'),
    newEncryptionDecryptionAttributes,

    -- ** Ibm3624NaturalPin
    Ibm3624NaturalPin (Ibm3624NaturalPin'),
    newIbm3624NaturalPin,

    -- ** Ibm3624PinFromOffset
    Ibm3624PinFromOffset (Ibm3624PinFromOffset'),
    newIbm3624PinFromOffset,

    -- ** Ibm3624PinOffset
    Ibm3624PinOffset (Ibm3624PinOffset'),
    newIbm3624PinOffset,

    -- ** Ibm3624PinVerification
    Ibm3624PinVerification (Ibm3624PinVerification'),
    newIbm3624PinVerification,

    -- ** Ibm3624RandomPin
    Ibm3624RandomPin (Ibm3624RandomPin'),
    newIbm3624RandomPin,

    -- ** MacAlgorithmDukpt
    MacAlgorithmDukpt (MacAlgorithmDukpt'),
    newMacAlgorithmDukpt,

    -- ** MacAlgorithmEmv
    MacAlgorithmEmv (MacAlgorithmEmv'),
    newMacAlgorithmEmv,

    -- ** MacAttributes
    MacAttributes (MacAttributes'),
    newMacAttributes,

    -- ** PinData
    PinData (PinData'),
    newPinData,

    -- ** PinGenerationAttributes
    PinGenerationAttributes (PinGenerationAttributes'),
    newPinGenerationAttributes,

    -- ** PinVerificationAttributes
    PinVerificationAttributes (PinVerificationAttributes'),
    newPinVerificationAttributes,

    -- ** ReEncryptionAttributes
    ReEncryptionAttributes (ReEncryptionAttributes'),
    newReEncryptionAttributes,

    -- ** SessionKeyAmex
    SessionKeyAmex (SessionKeyAmex'),
    newSessionKeyAmex,

    -- ** SessionKeyDerivation
    SessionKeyDerivation (SessionKeyDerivation'),
    newSessionKeyDerivation,

    -- ** SessionKeyDerivationValue
    SessionKeyDerivationValue (SessionKeyDerivationValue'),
    newSessionKeyDerivationValue,

    -- ** SessionKeyEmv2000
    SessionKeyEmv2000 (SessionKeyEmv2000'),
    newSessionKeyEmv2000,

    -- ** SessionKeyEmvCommon
    SessionKeyEmvCommon (SessionKeyEmvCommon'),
    newSessionKeyEmvCommon,

    -- ** SessionKeyMastercard
    SessionKeyMastercard (SessionKeyMastercard'),
    newSessionKeyMastercard,

    -- ** SessionKeyVisa
    SessionKeyVisa (SessionKeyVisa'),
    newSessionKeyVisa,

    -- ** SymmetricEncryptionAttributes
    SymmetricEncryptionAttributes (SymmetricEncryptionAttributes'),
    newSymmetricEncryptionAttributes,

    -- ** TranslationIsoFormats
    TranslationIsoFormats (TranslationIsoFormats'),
    newTranslationIsoFormats,

    -- ** TranslationPinDataIsoFormat034
    TranslationPinDataIsoFormat034 (TranslationPinDataIsoFormat034'),
    newTranslationPinDataIsoFormat034,

    -- ** TranslationPinDataIsoFormat1
    TranslationPinDataIsoFormat1 (TranslationPinDataIsoFormat1'),
    newTranslationPinDataIsoFormat1,

    -- ** VisaPin
    VisaPin (VisaPin'),
    newVisaPin,

    -- ** VisaPinVerification
    VisaPinVerification (VisaPinVerification'),
    newVisaPinVerification,

    -- ** VisaPinVerificationValue
    VisaPinVerificationValue (VisaPinVerificationValue'),
    newVisaPinVerificationValue,
  )
where

import Amazonka.PaymentCryptographyData.DecryptData
import Amazonka.PaymentCryptographyData.EncryptData
import Amazonka.PaymentCryptographyData.GenerateCardValidationData
import Amazonka.PaymentCryptographyData.GenerateMac
import Amazonka.PaymentCryptographyData.GeneratePinData
import Amazonka.PaymentCryptographyData.Lens
import Amazonka.PaymentCryptographyData.ReEncryptData
import Amazonka.PaymentCryptographyData.TranslatePinData
import Amazonka.PaymentCryptographyData.Types
import Amazonka.PaymentCryptographyData.VerifyAuthRequestCryptogram
import Amazonka.PaymentCryptographyData.VerifyCardValidationData
import Amazonka.PaymentCryptographyData.VerifyMac
import Amazonka.PaymentCryptographyData.VerifyPinData
import Amazonka.PaymentCryptographyData.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'PaymentCryptographyData'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
