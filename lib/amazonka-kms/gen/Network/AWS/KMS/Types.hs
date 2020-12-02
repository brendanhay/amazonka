{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types
  ( -- * Service Configuration
    kms,

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
    AliasListEntry,
    aliasListEntry,
    aleTargetKeyId,
    aleAliasName,
    aleAliasARN,

    -- * CustomKeyStoresListEntry
    CustomKeyStoresListEntry,
    customKeyStoresListEntry,
    cksleCustomKeyStoreName,
    cksleTrustAnchorCertificate,
    cksleConnectionErrorCode,
    cksleCreationDate,
    cksleCloudHSMClusterId,
    cksleCustomKeyStoreId,
    cksleConnectionState,

    -- * GrantConstraints
    GrantConstraints,
    grantConstraints,
    gcEncryptionContextEquals,
    gcEncryptionContextSubset,

    -- * GrantListEntry
    GrantListEntry,
    grantListEntry,
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
    KeyListEntry,
    keyListEntry,
    kleKeyId,
    kleKeyARN,

    -- * KeyMetadata
    KeyMetadata,
    keyMetadata,
    kmOrigin,
    kmExpirationModel,
    kmKeyManager,
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
    kmKeyId,

    -- * ListGrantsResponse
    ListGrantsResponse,
    listGrantsResponse,
    lgTruncated,
    lgGrants,
    lgNextMarker,

    -- * Tag
    Tag,
    tag,
    tagTagKey,
    tagTagValue,
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
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2014-11-01@ of the Amazon Key Management Service SDK configuration.
kms :: Service
kms =
  Service
    { _svcAbbrev = "KMS",
      _svcSigner = v4,
      _svcPrefix = "kms",
      _svcVersion = "2014-11-01",
      _svcEndpoint = defaultEndpoint kms,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "KMS",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
