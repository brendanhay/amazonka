-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types
  ( -- * Service configuration
    kmsService,

    -- * Errors

    -- * AlgorithmSpec
    AlgorithmSpec (..),

    -- * ConnectionErrorCodeType
    ConnectionErrorCodeType (..),

    -- * ConnectionStateType
    ConnectionStateType (..),

    -- * CustomerMasterKeySpec
    CustomerMasterKeySpec (..),

    -- * DataKeyPairSpec
    DataKeyPairSpec (..),

    -- * DataKeySpec
    DataKeySpec (..),

    -- * EncryptionAlgorithmSpec
    EncryptionAlgorithmSpec (..),

    -- * ExpirationModelType
    ExpirationModelType (..),

    -- * GrantOperation
    GrantOperation (..),

    -- * KeyManagerType
    KeyManagerType (..),

    -- * KeyState
    KeyState (..),

    -- * KeyUsageType
    KeyUsageType (..),

    -- * MessageType
    MessageType (..),

    -- * OriginType
    OriginType (..),

    -- * SigningAlgorithmSpec
    SigningAlgorithmSpec (..),

    -- * WrappingKeySpec
    WrappingKeySpec (..),

    -- * AliasListEntry
    AliasListEntry (..),
    mkAliasListEntry,
    aleTargetKeyId,
    aleAliasName,
    aleAliasARN,

    -- * CustomKeyStoresListEntry
    CustomKeyStoresListEntry (..),
    mkCustomKeyStoresListEntry,
    cksleCustomKeyStoreName,
    cksleTrustAnchorCertificate,
    cksleConnectionErrorCode,
    cksleCreationDate,
    cksleCloudHSMClusterId,
    cksleCustomKeyStoreId,
    cksleConnectionState,

    -- * GrantConstraints
    GrantConstraints (..),
    mkGrantConstraints,
    gcEncryptionContextEquals,
    gcEncryptionContextSubset,

    -- * GrantListEntry
    GrantListEntry (..),
    mkGrantListEntry,
    gleKeyId,
    gleRetiringPrincipal,
    gleIssuingAccount,
    gleGrantId,
    gleConstraints,
    gleGranteePrincipal,
    gleName,
    gleCreationDate,
    gleOperations,

    -- * KeyListEntry
    KeyListEntry (..),
    mkKeyListEntry,
    kleKeyId,
    kleKeyARN,

    -- * KeyMetadata
    KeyMetadata (..),
    mkKeyMetadata,
    kmOrigin,
    kmExpirationModel,
    kmKeyManager,
    kmKeyId,
    kmCustomerMasterKeySpec,
    kmEnabled,
    kmValidTo,
    kmARN,
    kmKeyState,
    kmEncryptionAlgorithms,
    kmAWSAccountId,
    kmSigningAlgorithms,
    kmKeyUsage,
    kmCreationDate,
    kmDeletionDate,
    kmCloudHSMClusterId,
    kmDescription,
    kmCustomKeyStoreId,

    -- * ListGrantsResponse
    ListGrantsResponse (..),
    mkListGrantsResponse,
    lgTruncated,
    lgGrants,
    lgNextMarker,

    -- * Tag
    Tag (..),
    mkTag,
    tTagValue,
    tTagKey,
  )
where

import Network.AWS.KMS.Types.AlgorithmSpec
import Network.AWS.KMS.Types.AliasListEntry
import Network.AWS.KMS.Types.ConnectionErrorCodeType
import Network.AWS.KMS.Types.ConnectionStateType
import Network.AWS.KMS.Types.CustomKeyStoresListEntry
import Network.AWS.KMS.Types.CustomerMasterKeySpec
import Network.AWS.KMS.Types.DataKeyPairSpec
import Network.AWS.KMS.Types.DataKeySpec
import Network.AWS.KMS.Types.EncryptionAlgorithmSpec
import Network.AWS.KMS.Types.ExpirationModelType
import Network.AWS.KMS.Types.GrantConstraints
import Network.AWS.KMS.Types.GrantListEntry
import Network.AWS.KMS.Types.GrantOperation
import Network.AWS.KMS.Types.KeyListEntry
import Network.AWS.KMS.Types.KeyManagerType
import Network.AWS.KMS.Types.KeyMetadata
import Network.AWS.KMS.Types.KeyState
import Network.AWS.KMS.Types.KeyUsageType
import Network.AWS.KMS.Types.ListGrantsResponse
import Network.AWS.KMS.Types.MessageType
import Network.AWS.KMS.Types.OriginType
import Network.AWS.KMS.Types.SigningAlgorithmSpec
import Network.AWS.KMS.Types.Tag
import Network.AWS.KMS.Types.WrappingKeySpec
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2014-11-01@ of the Amazon Key Management Service SDK configuration.
kmsService :: Lude.Service
kmsService =
  Lude.Service
    { Lude._svcAbbrev = "KMS",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "kms",
      Lude._svcVersion = "2014-11-01",
      Lude._svcEndpoint = Lude.defaultEndpoint kmsService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "KMS",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
