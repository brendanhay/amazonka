{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.PaymentCryptography.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptography.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,
    _ServiceUnavailableException,
    _ThrottlingException,
    _ValidationException,

    -- * KeyAlgorithm
    KeyAlgorithm (..),

    -- * KeyCheckValueAlgorithm
    KeyCheckValueAlgorithm (..),

    -- * KeyClass
    KeyClass (..),

    -- * KeyMaterialType
    KeyMaterialType (..),

    -- * KeyOrigin
    KeyOrigin (..),

    -- * KeyState
    KeyState (..),

    -- * KeyUsage
    KeyUsage (..),

    -- * Tr34KeyBlockFormat
    Tr34KeyBlockFormat (..),

    -- * WrappedKeyMaterialFormat
    WrappedKeyMaterialFormat (..),

    -- * Alias
    Alias (..),
    newAlias,
    alias_keyArn,
    alias_aliasName,

    -- * ExportKeyMaterial
    ExportKeyMaterial (..),
    newExportKeyMaterial,
    exportKeyMaterial_tr31KeyBlock,
    exportKeyMaterial_tr34KeyBlock,

    -- * ExportTr31KeyBlock
    ExportTr31KeyBlock (..),
    newExportTr31KeyBlock,
    exportTr31KeyBlock_wrappingKeyIdentifier,

    -- * ExportTr34KeyBlock
    ExportTr34KeyBlock (..),
    newExportTr34KeyBlock,
    exportTr34KeyBlock_randomNonce,
    exportTr34KeyBlock_certificateAuthorityPublicKeyIdentifier,
    exportTr34KeyBlock_exportToken,
    exportTr34KeyBlock_keyBlockFormat,
    exportTr34KeyBlock_wrappingKeyCertificate,

    -- * ImportKeyMaterial
    ImportKeyMaterial (..),
    newImportKeyMaterial,
    importKeyMaterial_rootCertificatePublicKey,
    importKeyMaterial_tr31KeyBlock,
    importKeyMaterial_tr34KeyBlock,
    importKeyMaterial_trustedCertificatePublicKey,

    -- * ImportTr31KeyBlock
    ImportTr31KeyBlock (..),
    newImportTr31KeyBlock,
    importTr31KeyBlock_wrappedKeyBlock,
    importTr31KeyBlock_wrappingKeyIdentifier,

    -- * ImportTr34KeyBlock
    ImportTr34KeyBlock (..),
    newImportTr34KeyBlock,
    importTr34KeyBlock_randomNonce,
    importTr34KeyBlock_certificateAuthorityPublicKeyIdentifier,
    importTr34KeyBlock_importToken,
    importTr34KeyBlock_keyBlockFormat,
    importTr34KeyBlock_signingKeyCertificate,
    importTr34KeyBlock_wrappedKeyBlock,

    -- * Key
    Key (..),
    newKey,
    key_deletePendingTimestamp,
    key_deleteTimestamp,
    key_usageStartTimestamp,
    key_usageStopTimestamp,
    key_createTimestamp,
    key_enabled,
    key_exportable,
    key_keyArn,
    key_keyAttributes,
    key_keyCheckValue,
    key_keyCheckValueAlgorithm,
    key_keyOrigin,
    key_keyState,

    -- * KeyAttributes
    KeyAttributes (..),
    newKeyAttributes,
    keyAttributes_keyAlgorithm,
    keyAttributes_keyClass,
    keyAttributes_keyModesOfUse,
    keyAttributes_keyUsage,

    -- * KeyModesOfUse
    KeyModesOfUse (..),
    newKeyModesOfUse,
    keyModesOfUse_decrypt,
    keyModesOfUse_deriveKey,
    keyModesOfUse_encrypt,
    keyModesOfUse_generate,
    keyModesOfUse_noRestrictions,
    keyModesOfUse_sign,
    keyModesOfUse_unwrap,
    keyModesOfUse_verify,
    keyModesOfUse_wrap,

    -- * KeySummary
    KeySummary (..),
    newKeySummary,
    keySummary_enabled,
    keySummary_exportable,
    keySummary_keyArn,
    keySummary_keyAttributes,
    keySummary_keyCheckValue,
    keySummary_keyState,

    -- * RootCertificatePublicKey
    RootCertificatePublicKey (..),
    newRootCertificatePublicKey,
    rootCertificatePublicKey_keyAttributes,
    rootCertificatePublicKey_publicKeyCertificate,

    -- * Tag
    Tag (..),
    newTag,
    tag_value,
    tag_key,

    -- * TrustedCertificatePublicKey
    TrustedCertificatePublicKey (..),
    newTrustedCertificatePublicKey,
    trustedCertificatePublicKey_certificateAuthorityPublicKeyIdentifier,
    trustedCertificatePublicKey_keyAttributes,
    trustedCertificatePublicKey_publicKeyCertificate,

    -- * WrappedKey
    WrappedKey (..),
    newWrappedKey,
    wrappedKey_keyMaterial,
    wrappedKey_wrappedKeyMaterialFormat,
    wrappedKey_wrappingKeyArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.PaymentCryptography.Types.Alias
import Amazonka.PaymentCryptography.Types.ExportKeyMaterial
import Amazonka.PaymentCryptography.Types.ExportTr31KeyBlock
import Amazonka.PaymentCryptography.Types.ExportTr34KeyBlock
import Amazonka.PaymentCryptography.Types.ImportKeyMaterial
import Amazonka.PaymentCryptography.Types.ImportTr31KeyBlock
import Amazonka.PaymentCryptography.Types.ImportTr34KeyBlock
import Amazonka.PaymentCryptography.Types.Key
import Amazonka.PaymentCryptography.Types.KeyAlgorithm
import Amazonka.PaymentCryptography.Types.KeyAttributes
import Amazonka.PaymentCryptography.Types.KeyCheckValueAlgorithm
import Amazonka.PaymentCryptography.Types.KeyClass
import Amazonka.PaymentCryptography.Types.KeyMaterialType
import Amazonka.PaymentCryptography.Types.KeyModesOfUse
import Amazonka.PaymentCryptography.Types.KeyOrigin
import Amazonka.PaymentCryptography.Types.KeyState
import Amazonka.PaymentCryptography.Types.KeySummary
import Amazonka.PaymentCryptography.Types.KeyUsage
import Amazonka.PaymentCryptography.Types.RootCertificatePublicKey
import Amazonka.PaymentCryptography.Types.Tag
import Amazonka.PaymentCryptography.Types.Tr34KeyBlockFormat
import Amazonka.PaymentCryptography.Types.TrustedCertificatePublicKey
import Amazonka.PaymentCryptography.Types.WrappedKey
import Amazonka.PaymentCryptography.Types.WrappedKeyMaterialFormat
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2021-09-14@ of the Amazon Payment Cryptography Control Plane SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "PaymentCryptography",
      Core.signer = Sign.v4,
      Core.endpointPrefix =
        "controlplane.payment-cryptography",
      Core.signingName = "payment-cryptography",
      Core.version = "2021-09-14",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error =
        Core.parseJSONError "PaymentCryptography",
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

-- | This request can cause an inconsistent state for the resource.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | The request processing has failed because of an unknown error,
-- exception, or failure.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"

-- | The request was denied due to an invalid resource error.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | This request would cause a service quota to be exceeded.
_ServiceQuotaExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"

-- | The service cannot complete the request.
_ServiceUnavailableException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"

-- | The request was denied due to request throttling.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"

-- | The request was denied due to an invalid request error.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
