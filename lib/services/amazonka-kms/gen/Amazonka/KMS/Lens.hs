{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.KMS.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KMS.Lens
  ( -- * Operations

    -- ** CancelKeyDeletion
    cancelKeyDeletion_keyId,
    cancelKeyDeletionResponse_keyId,
    cancelKeyDeletionResponse_httpStatus,

    -- ** ConnectCustomKeyStore
    connectCustomKeyStore_customKeyStoreId,
    connectCustomKeyStoreResponse_httpStatus,

    -- ** CreateAlias
    createAlias_aliasName,
    createAlias_targetKeyId,

    -- ** CreateCustomKeyStore
    createCustomKeyStore_customKeyStoreName,
    createCustomKeyStore_cloudHsmClusterId,
    createCustomKeyStore_trustAnchorCertificate,
    createCustomKeyStore_keyStorePassword,
    createCustomKeyStoreResponse_customKeyStoreId,
    createCustomKeyStoreResponse_httpStatus,

    -- ** CreateGrant
    createGrant_name,
    createGrant_constraints,
    createGrant_retiringPrincipal,
    createGrant_grantTokens,
    createGrant_keyId,
    createGrant_granteePrincipal,
    createGrant_operations,
    createGrantResponse_grantToken,
    createGrantResponse_grantId,
    createGrantResponse_httpStatus,

    -- ** CreateKey
    createKey_tags,
    createKey_policy,
    createKey_customKeyStoreId,
    createKey_customerMasterKeySpec,
    createKey_keyUsage,
    createKey_description,
    createKey_multiRegion,
    createKey_keySpec,
    createKey_bypassPolicyLockoutSafetyCheck,
    createKey_origin,
    createKeyResponse_keyMetadata,
    createKeyResponse_httpStatus,

    -- ** Decrypt
    decrypt_encryptionAlgorithm,
    decrypt_grantTokens,
    decrypt_keyId,
    decrypt_encryptionContext,
    decrypt_ciphertextBlob,
    decryptResponse_encryptionAlgorithm,
    decryptResponse_plaintext,
    decryptResponse_keyId,
    decryptResponse_httpStatus,

    -- ** DeleteAlias
    deleteAlias_aliasName,

    -- ** DeleteCustomKeyStore
    deleteCustomKeyStore_customKeyStoreId,
    deleteCustomKeyStoreResponse_httpStatus,

    -- ** DeleteImportedKeyMaterial
    deleteImportedKeyMaterial_keyId,

    -- ** DescribeCustomKeyStores
    describeCustomKeyStores_marker,
    describeCustomKeyStores_customKeyStoreId,
    describeCustomKeyStores_limit,
    describeCustomKeyStores_customKeyStoreName,
    describeCustomKeyStoresResponse_truncated,
    describeCustomKeyStoresResponse_customKeyStores,
    describeCustomKeyStoresResponse_nextMarker,
    describeCustomKeyStoresResponse_httpStatus,

    -- ** DescribeKey
    describeKey_grantTokens,
    describeKey_keyId,
    describeKeyResponse_keyMetadata,
    describeKeyResponse_httpStatus,

    -- ** DisableKey
    disableKey_keyId,

    -- ** DisableKeyRotation
    disableKeyRotation_keyId,

    -- ** DisconnectCustomKeyStore
    disconnectCustomKeyStore_customKeyStoreId,
    disconnectCustomKeyStoreResponse_httpStatus,

    -- ** EnableKey
    enableKey_keyId,

    -- ** EnableKeyRotation
    enableKeyRotation_keyId,

    -- ** Encrypt
    encrypt_encryptionAlgorithm,
    encrypt_grantTokens,
    encrypt_encryptionContext,
    encrypt_keyId,
    encrypt_plaintext,
    encryptResponse_encryptionAlgorithm,
    encryptResponse_ciphertextBlob,
    encryptResponse_keyId,
    encryptResponse_httpStatus,

    -- ** GenerateDataKey
    generateDataKey_grantTokens,
    generateDataKey_keySpec,
    generateDataKey_numberOfBytes,
    generateDataKey_encryptionContext,
    generateDataKey_keyId,
    generateDataKeyResponse_httpStatus,
    generateDataKeyResponse_keyId,
    generateDataKeyResponse_plaintext,
    generateDataKeyResponse_ciphertextBlob,

    -- ** GenerateDataKeyPair
    generateDataKeyPair_grantTokens,
    generateDataKeyPair_encryptionContext,
    generateDataKeyPair_keyId,
    generateDataKeyPair_keyPairSpec,
    generateDataKeyPairResponse_privateKeyPlaintext,
    generateDataKeyPairResponse_publicKey,
    generateDataKeyPairResponse_privateKeyCiphertextBlob,
    generateDataKeyPairResponse_keyPairSpec,
    generateDataKeyPairResponse_keyId,
    generateDataKeyPairResponse_httpStatus,

    -- ** GenerateDataKeyPairWithoutPlaintext
    generateDataKeyPairWithoutPlaintext_grantTokens,
    generateDataKeyPairWithoutPlaintext_encryptionContext,
    generateDataKeyPairWithoutPlaintext_keyId,
    generateDataKeyPairWithoutPlaintext_keyPairSpec,
    generateDataKeyPairWithoutPlaintextResponse_publicKey,
    generateDataKeyPairWithoutPlaintextResponse_privateKeyCiphertextBlob,
    generateDataKeyPairWithoutPlaintextResponse_keyPairSpec,
    generateDataKeyPairWithoutPlaintextResponse_keyId,
    generateDataKeyPairWithoutPlaintextResponse_httpStatus,

    -- ** GenerateDataKeyWithoutPlaintext
    generateDataKeyWithoutPlaintext_grantTokens,
    generateDataKeyWithoutPlaintext_keySpec,
    generateDataKeyWithoutPlaintext_numberOfBytes,
    generateDataKeyWithoutPlaintext_encryptionContext,
    generateDataKeyWithoutPlaintext_keyId,
    generateDataKeyWithoutPlaintextResponse_ciphertextBlob,
    generateDataKeyWithoutPlaintextResponse_keyId,
    generateDataKeyWithoutPlaintextResponse_httpStatus,

    -- ** GenerateRandom
    generateRandom_customKeyStoreId,
    generateRandom_numberOfBytes,
    generateRandomResponse_plaintext,
    generateRandomResponse_httpStatus,

    -- ** GetKeyPolicy
    getKeyPolicy_keyId,
    getKeyPolicy_policyName,
    getKeyPolicyResponse_policy,
    getKeyPolicyResponse_httpStatus,

    -- ** GetKeyRotationStatus
    getKeyRotationStatus_keyId,
    getKeyRotationStatusResponse_keyRotationEnabled,
    getKeyRotationStatusResponse_httpStatus,

    -- ** GetParametersForImport
    getParametersForImport_keyId,
    getParametersForImport_wrappingAlgorithm,
    getParametersForImport_wrappingKeySpec,
    getParametersForImportResponse_publicKey,
    getParametersForImportResponse_keyId,
    getParametersForImportResponse_importToken,
    getParametersForImportResponse_parametersValidTo,
    getParametersForImportResponse_httpStatus,

    -- ** GetPublicKey
    getPublicKey_grantTokens,
    getPublicKey_keyId,
    getPublicKeyResponse_publicKey,
    getPublicKeyResponse_encryptionAlgorithms,
    getPublicKeyResponse_customerMasterKeySpec,
    getPublicKeyResponse_keyUsage,
    getPublicKeyResponse_keySpec,
    getPublicKeyResponse_keyId,
    getPublicKeyResponse_signingAlgorithms,
    getPublicKeyResponse_httpStatus,

    -- ** ImportKeyMaterial
    importKeyMaterial_expirationModel,
    importKeyMaterial_validTo,
    importKeyMaterial_keyId,
    importKeyMaterial_importToken,
    importKeyMaterial_encryptedKeyMaterial,
    importKeyMaterialResponse_httpStatus,

    -- ** ListAliases
    listAliases_marker,
    listAliases_limit,
    listAliases_keyId,
    listAliasesResponse_truncated,
    listAliasesResponse_aliases,
    listAliasesResponse_nextMarker,
    listAliasesResponse_httpStatus,

    -- ** ListGrants
    listGrants_granteePrincipal,
    listGrants_marker,
    listGrants_limit,
    listGrants_grantId,
    listGrants_keyId,
    listGrantsResponse_truncated,
    listGrantsResponse_grants,
    listGrantsResponse_nextMarker,

    -- ** ListKeyPolicies
    listKeyPolicies_marker,
    listKeyPolicies_limit,
    listKeyPolicies_keyId,
    listKeyPoliciesResponse_truncated,
    listKeyPoliciesResponse_policyNames,
    listKeyPoliciesResponse_nextMarker,
    listKeyPoliciesResponse_httpStatus,

    -- ** ListKeys
    listKeys_marker,
    listKeys_limit,
    listKeysResponse_truncated,
    listKeysResponse_nextMarker,
    listKeysResponse_keys,
    listKeysResponse_httpStatus,

    -- ** ListResourceTags
    listResourceTags_marker,
    listResourceTags_limit,
    listResourceTags_keyId,
    listResourceTagsResponse_tags,
    listResourceTagsResponse_truncated,
    listResourceTagsResponse_nextMarker,
    listResourceTagsResponse_httpStatus,

    -- ** ListRetirableGrants
    listRetirableGrants_marker,
    listRetirableGrants_limit,
    listRetirableGrants_retiringPrincipal,
    listGrantsResponse_truncated,
    listGrantsResponse_grants,
    listGrantsResponse_nextMarker,

    -- ** PutKeyPolicy
    putKeyPolicy_bypassPolicyLockoutSafetyCheck,
    putKeyPolicy_keyId,
    putKeyPolicy_policyName,
    putKeyPolicy_policy,

    -- ** ReEncrypt
    reEncrypt_sourceKeyId,
    reEncrypt_sourceEncryptionAlgorithm,
    reEncrypt_grantTokens,
    reEncrypt_destinationEncryptionContext,
    reEncrypt_destinationEncryptionAlgorithm,
    reEncrypt_sourceEncryptionContext,
    reEncrypt_ciphertextBlob,
    reEncrypt_destinationKeyId,
    reEncryptResponse_sourceKeyId,
    reEncryptResponse_sourceEncryptionAlgorithm,
    reEncryptResponse_ciphertextBlob,
    reEncryptResponse_keyId,
    reEncryptResponse_destinationEncryptionAlgorithm,
    reEncryptResponse_httpStatus,

    -- ** ReplicateKey
    replicateKey_tags,
    replicateKey_policy,
    replicateKey_description,
    replicateKey_bypassPolicyLockoutSafetyCheck,
    replicateKey_keyId,
    replicateKey_replicaRegion,
    replicateKeyResponse_replicaKeyMetadata,
    replicateKeyResponse_replicaTags,
    replicateKeyResponse_replicaPolicy,
    replicateKeyResponse_httpStatus,

    -- ** RetireGrant
    retireGrant_grantToken,
    retireGrant_grantId,
    retireGrant_keyId,

    -- ** RevokeGrant
    revokeGrant_keyId,
    revokeGrant_grantId,

    -- ** ScheduleKeyDeletion
    scheduleKeyDeletion_pendingWindowInDays,
    scheduleKeyDeletion_keyId,
    scheduleKeyDeletionResponse_pendingWindowInDays,
    scheduleKeyDeletionResponse_keyState,
    scheduleKeyDeletionResponse_deletionDate,
    scheduleKeyDeletionResponse_keyId,
    scheduleKeyDeletionResponse_httpStatus,

    -- ** Sign
    sign_messageType,
    sign_grantTokens,
    sign_keyId,
    sign_message,
    sign_signingAlgorithm,
    signResponse_signature,
    signResponse_signingAlgorithm,
    signResponse_keyId,
    signResponse_httpStatus,

    -- ** TagResource
    tagResource_keyId,
    tagResource_tags,

    -- ** UntagResource
    untagResource_keyId,
    untagResource_tagKeys,

    -- ** UpdateAlias
    updateAlias_aliasName,
    updateAlias_targetKeyId,

    -- ** UpdateCustomKeyStore
    updateCustomKeyStore_newCustomKeyStoreName,
    updateCustomKeyStore_keyStorePassword,
    updateCustomKeyStore_cloudHsmClusterId,
    updateCustomKeyStore_customKeyStoreId,
    updateCustomKeyStoreResponse_httpStatus,

    -- ** UpdateKeyDescription
    updateKeyDescription_keyId,
    updateKeyDescription_description,

    -- ** UpdatePrimaryRegion
    updatePrimaryRegion_keyId,
    updatePrimaryRegion_primaryRegion,

    -- ** Verify
    verify_messageType,
    verify_grantTokens,
    verify_keyId,
    verify_message,
    verify_signature,
    verify_signingAlgorithm,
    verifyResponse_signatureValid,
    verifyResponse_signingAlgorithm,
    verifyResponse_keyId,
    verifyResponse_httpStatus,

    -- * Types

    -- ** AliasListEntry
    aliasListEntry_lastUpdatedDate,
    aliasListEntry_aliasArn,
    aliasListEntry_creationDate,
    aliasListEntry_targetKeyId,
    aliasListEntry_aliasName,

    -- ** CustomKeyStoresListEntry
    customKeyStoresListEntry_customKeyStoreId,
    customKeyStoresListEntry_connectionState,
    customKeyStoresListEntry_creationDate,
    customKeyStoresListEntry_cloudHsmClusterId,
    customKeyStoresListEntry_trustAnchorCertificate,
    customKeyStoresListEntry_customKeyStoreName,
    customKeyStoresListEntry_connectionErrorCode,

    -- ** GrantConstraints
    grantConstraints_encryptionContextSubset,
    grantConstraints_encryptionContextEquals,

    -- ** GrantListEntry
    grantListEntry_issuingAccount,
    grantListEntry_name,
    grantListEntry_granteePrincipal,
    grantListEntry_operations,
    grantListEntry_constraints,
    grantListEntry_creationDate,
    grantListEntry_retiringPrincipal,
    grantListEntry_grantId,
    grantListEntry_keyId,

    -- ** KeyListEntry
    keyListEntry_keyArn,
    keyListEntry_keyId,

    -- ** KeyMetadata
    keyMetadata_aWSAccountId,
    keyMetadata_expirationModel,
    keyMetadata_encryptionAlgorithms,
    keyMetadata_customKeyStoreId,
    keyMetadata_pendingDeletionWindowInDays,
    keyMetadata_arn,
    keyMetadata_multiRegionConfiguration,
    keyMetadata_creationDate,
    keyMetadata_customerMasterKeySpec,
    keyMetadata_keyUsage,
    keyMetadata_description,
    keyMetadata_multiRegion,
    keyMetadata_enabled,
    keyMetadata_keyManager,
    keyMetadata_cloudHsmClusterId,
    keyMetadata_keySpec,
    keyMetadata_keyState,
    keyMetadata_deletionDate,
    keyMetadata_origin,
    keyMetadata_signingAlgorithms,
    keyMetadata_validTo,
    keyMetadata_keyId,

    -- ** ListGrantsResponse
    listGrantsResponse_truncated,
    listGrantsResponse_grants,
    listGrantsResponse_nextMarker,

    -- ** MultiRegionConfiguration
    multiRegionConfiguration_primaryKey,
    multiRegionConfiguration_replicaKeys,
    multiRegionConfiguration_multiRegionKeyType,

    -- ** MultiRegionKey
    multiRegionKey_arn,
    multiRegionKey_region,

    -- ** Tag
    tag_tagKey,
    tag_tagValue,
  )
where

import Amazonka.KMS.CancelKeyDeletion
import Amazonka.KMS.ConnectCustomKeyStore
import Amazonka.KMS.CreateAlias
import Amazonka.KMS.CreateCustomKeyStore
import Amazonka.KMS.CreateGrant
import Amazonka.KMS.CreateKey
import Amazonka.KMS.Decrypt
import Amazonka.KMS.DeleteAlias
import Amazonka.KMS.DeleteCustomKeyStore
import Amazonka.KMS.DeleteImportedKeyMaterial
import Amazonka.KMS.DescribeCustomKeyStores
import Amazonka.KMS.DescribeKey
import Amazonka.KMS.DisableKey
import Amazonka.KMS.DisableKeyRotation
import Amazonka.KMS.DisconnectCustomKeyStore
import Amazonka.KMS.EnableKey
import Amazonka.KMS.EnableKeyRotation
import Amazonka.KMS.Encrypt
import Amazonka.KMS.GenerateDataKey
import Amazonka.KMS.GenerateDataKeyPair
import Amazonka.KMS.GenerateDataKeyPairWithoutPlaintext
import Amazonka.KMS.GenerateDataKeyWithoutPlaintext
import Amazonka.KMS.GenerateRandom
import Amazonka.KMS.GetKeyPolicy
import Amazonka.KMS.GetKeyRotationStatus
import Amazonka.KMS.GetParametersForImport
import Amazonka.KMS.GetPublicKey
import Amazonka.KMS.ImportKeyMaterial
import Amazonka.KMS.ListAliases
import Amazonka.KMS.ListGrants
import Amazonka.KMS.ListKeyPolicies
import Amazonka.KMS.ListKeys
import Amazonka.KMS.ListResourceTags
import Amazonka.KMS.ListRetirableGrants
import Amazonka.KMS.PutKeyPolicy
import Amazonka.KMS.ReEncrypt
import Amazonka.KMS.ReplicateKey
import Amazonka.KMS.RetireGrant
import Amazonka.KMS.RevokeGrant
import Amazonka.KMS.ScheduleKeyDeletion
import Amazonka.KMS.Sign
import Amazonka.KMS.TagResource
import Amazonka.KMS.Types.AliasListEntry
import Amazonka.KMS.Types.CustomKeyStoresListEntry
import Amazonka.KMS.Types.GrantConstraints
import Amazonka.KMS.Types.GrantListEntry
import Amazonka.KMS.Types.KeyListEntry
import Amazonka.KMS.Types.KeyMetadata
import Amazonka.KMS.Types.ListGrantsResponse
import Amazonka.KMS.Types.MultiRegionConfiguration
import Amazonka.KMS.Types.MultiRegionKey
import Amazonka.KMS.Types.Tag
import Amazonka.KMS.UntagResource
import Amazonka.KMS.UpdateAlias
import Amazonka.KMS.UpdateCustomKeyStore
import Amazonka.KMS.UpdateKeyDescription
import Amazonka.KMS.UpdatePrimaryRegion
import Amazonka.KMS.Verify
