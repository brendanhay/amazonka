{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Lens
  ( -- * Operations

    -- ** Encrypt
    encrypt_encryptionContext,
    encrypt_grantTokens,
    encrypt_encryptionAlgorithm,
    encrypt_keyId,
    encrypt_plaintext,
    encryptResponse_keyId,
    encryptResponse_encryptionAlgorithm,
    encryptResponse_ciphertextBlob,
    encryptResponse_httpStatus,

    -- ** CreateCustomKeyStore
    createCustomKeyStore_customKeyStoreName,
    createCustomKeyStore_cloudHsmClusterId,
    createCustomKeyStore_trustAnchorCertificate,
    createCustomKeyStore_keyStorePassword,
    createCustomKeyStoreResponse_customKeyStoreId,
    createCustomKeyStoreResponse_httpStatus,

    -- ** ListGrants
    listGrants_grantId,
    listGrants_granteePrincipal,
    listGrants_marker,
    listGrants_limit,
    listGrants_keyId,
    listGrantsResponse_truncated,
    listGrantsResponse_grants,
    listGrantsResponse_nextMarker,

    -- ** DisableKeyRotation
    disableKeyRotation_keyId,

    -- ** Verify
    verify_messageType,
    verify_grantTokens,
    verify_keyId,
    verify_message,
    verify_signature,
    verify_signingAlgorithm,
    verifyResponse_signingAlgorithm,
    verifyResponse_signatureValid,
    verifyResponse_keyId,
    verifyResponse_httpStatus,

    -- ** GenerateDataKeyWithoutPlaintext
    generateDataKeyWithoutPlaintext_keySpec,
    generateDataKeyWithoutPlaintext_encryptionContext,
    generateDataKeyWithoutPlaintext_numberOfBytes,
    generateDataKeyWithoutPlaintext_grantTokens,
    generateDataKeyWithoutPlaintext_keyId,
    generateDataKeyWithoutPlaintextResponse_keyId,
    generateDataKeyWithoutPlaintextResponse_ciphertextBlob,
    generateDataKeyWithoutPlaintextResponse_httpStatus,

    -- ** UpdateCustomKeyStore
    updateCustomKeyStore_keyStorePassword,
    updateCustomKeyStore_cloudHsmClusterId,
    updateCustomKeyStore_newCustomKeyStoreName,
    updateCustomKeyStore_customKeyStoreId,
    updateCustomKeyStoreResponse_httpStatus,

    -- ** GetParametersForImport
    getParametersForImport_keyId,
    getParametersForImport_wrappingAlgorithm,
    getParametersForImport_wrappingKeySpec,
    getParametersForImportResponse_keyId,
    getParametersForImportResponse_publicKey,
    getParametersForImportResponse_parametersValidTo,
    getParametersForImportResponse_importToken,
    getParametersForImportResponse_httpStatus,

    -- ** EnableKeyRotation
    enableKeyRotation_keyId,

    -- ** DeleteCustomKeyStore
    deleteCustomKeyStore_customKeyStoreId,
    deleteCustomKeyStoreResponse_httpStatus,

    -- ** CreateAlias
    createAlias_aliasName,
    createAlias_targetKeyId,

    -- ** CreateGrant
    createGrant_retiringPrincipal,
    createGrant_grantTokens,
    createGrant_constraints,
    createGrant_name,
    createGrant_keyId,
    createGrant_granteePrincipal,
    createGrant_operations,
    createGrantResponse_grantId,
    createGrantResponse_grantToken,
    createGrantResponse_httpStatus,

    -- ** ListAliases
    listAliases_keyId,
    listAliases_marker,
    listAliases_limit,
    listAliasesResponse_truncated,
    listAliasesResponse_aliases,
    listAliasesResponse_nextMarker,
    listAliasesResponse_httpStatus,

    -- ** UpdatePrimaryRegion
    updatePrimaryRegion_keyId,
    updatePrimaryRegion_primaryRegion,

    -- ** ConnectCustomKeyStore
    connectCustomKeyStore_customKeyStoreId,
    connectCustomKeyStoreResponse_httpStatus,

    -- ** ListRetirableGrants
    listRetirableGrants_marker,
    listRetirableGrants_limit,
    listRetirableGrants_retiringPrincipal,
    listGrantsResponse_truncated,
    listGrantsResponse_grants,
    listGrantsResponse_nextMarker,

    -- ** GetPublicKey
    getPublicKey_grantTokens,
    getPublicKey_keyId,
    getPublicKeyResponse_keySpec,
    getPublicKeyResponse_keyId,
    getPublicKeyResponse_customerMasterKeySpec,
    getPublicKeyResponse_encryptionAlgorithms,
    getPublicKeyResponse_publicKey,
    getPublicKeyResponse_signingAlgorithms,
    getPublicKeyResponse_keyUsage,
    getPublicKeyResponse_httpStatus,

    -- ** GenerateRandom
    generateRandom_numberOfBytes,
    generateRandom_customKeyStoreId,
    generateRandomResponse_plaintext,
    generateRandomResponse_httpStatus,

    -- ** CreateKey
    createKey_origin,
    createKey_keySpec,
    createKey_customerMasterKeySpec,
    createKey_keyUsage,
    createKey_bypassPolicyLockoutSafetyCheck,
    createKey_policy,
    createKey_description,
    createKey_customKeyStoreId,
    createKey_tags,
    createKey_multiRegion,
    createKeyResponse_keyMetadata,
    createKeyResponse_httpStatus,

    -- ** DisableKey
    disableKey_keyId,

    -- ** DisconnectCustomKeyStore
    disconnectCustomKeyStore_customKeyStoreId,
    disconnectCustomKeyStoreResponse_httpStatus,

    -- ** RetireGrant
    retireGrant_keyId,
    retireGrant_grantId,
    retireGrant_grantToken,

    -- ** ListKeys
    listKeys_marker,
    listKeys_limit,
    listKeysResponse_truncated,
    listKeysResponse_keys,
    listKeysResponse_nextMarker,
    listKeysResponse_httpStatus,

    -- ** ListResourceTags
    listResourceTags_marker,
    listResourceTags_limit,
    listResourceTags_keyId,
    listResourceTagsResponse_truncated,
    listResourceTagsResponse_nextMarker,
    listResourceTagsResponse_tags,
    listResourceTagsResponse_httpStatus,

    -- ** GetKeyRotationStatus
    getKeyRotationStatus_keyId,
    getKeyRotationStatusResponse_keyRotationEnabled,
    getKeyRotationStatusResponse_httpStatus,

    -- ** GenerateDataKey
    generateDataKey_keySpec,
    generateDataKey_encryptionContext,
    generateDataKey_numberOfBytes,
    generateDataKey_grantTokens,
    generateDataKey_keyId,
    generateDataKeyResponse_httpStatus,
    generateDataKeyResponse_keyId,
    generateDataKeyResponse_plaintext,
    generateDataKeyResponse_ciphertextBlob,

    -- ** DeleteAlias
    deleteAlias_aliasName,

    -- ** UpdateAlias
    updateAlias_aliasName,
    updateAlias_targetKeyId,

    -- ** DescribeKey
    describeKey_grantTokens,
    describeKey_keyId,
    describeKeyResponse_keyMetadata,
    describeKeyResponse_httpStatus,

    -- ** DescribeCustomKeyStores
    describeCustomKeyStores_customKeyStoreName,
    describeCustomKeyStores_marker,
    describeCustomKeyStores_limit,
    describeCustomKeyStores_customKeyStoreId,
    describeCustomKeyStoresResponse_truncated,
    describeCustomKeyStoresResponse_nextMarker,
    describeCustomKeyStoresResponse_customKeyStores,
    describeCustomKeyStoresResponse_httpStatus,

    -- ** CancelKeyDeletion
    cancelKeyDeletion_keyId,
    cancelKeyDeletionResponse_keyId,
    cancelKeyDeletionResponse_httpStatus,

    -- ** Decrypt
    decrypt_keyId,
    decrypt_encryptionContext,
    decrypt_grantTokens,
    decrypt_encryptionAlgorithm,
    decrypt_ciphertextBlob,
    decryptResponse_keyId,
    decryptResponse_plaintext,
    decryptResponse_encryptionAlgorithm,
    decryptResponse_httpStatus,

    -- ** GenerateDataKeyPairWithoutPlaintext
    generateDataKeyPairWithoutPlaintext_encryptionContext,
    generateDataKeyPairWithoutPlaintext_grantTokens,
    generateDataKeyPairWithoutPlaintext_keyId,
    generateDataKeyPairWithoutPlaintext_keyPairSpec,
    generateDataKeyPairWithoutPlaintextResponse_keyId,
    generateDataKeyPairWithoutPlaintextResponse_publicKey,
    generateDataKeyPairWithoutPlaintextResponse_keyPairSpec,
    generateDataKeyPairWithoutPlaintextResponse_privateKeyCiphertextBlob,
    generateDataKeyPairWithoutPlaintextResponse_httpStatus,

    -- ** UpdateKeyDescription
    updateKeyDescription_keyId,
    updateKeyDescription_description,

    -- ** ReEncrypt
    reEncrypt_destinationEncryptionContext,
    reEncrypt_sourceKeyId,
    reEncrypt_sourceEncryptionContext,
    reEncrypt_grantTokens,
    reEncrypt_destinationEncryptionAlgorithm,
    reEncrypt_sourceEncryptionAlgorithm,
    reEncrypt_ciphertextBlob,
    reEncrypt_destinationKeyId,
    reEncryptResponse_sourceKeyId,
    reEncryptResponse_keyId,
    reEncryptResponse_destinationEncryptionAlgorithm,
    reEncryptResponse_sourceEncryptionAlgorithm,
    reEncryptResponse_ciphertextBlob,
    reEncryptResponse_httpStatus,

    -- ** TagResource
    tagResource_keyId,
    tagResource_tags,

    -- ** ListKeyPolicies
    listKeyPolicies_marker,
    listKeyPolicies_limit,
    listKeyPolicies_keyId,
    listKeyPoliciesResponse_policyNames,
    listKeyPoliciesResponse_truncated,
    listKeyPoliciesResponse_nextMarker,
    listKeyPoliciesResponse_httpStatus,

    -- ** UntagResource
    untagResource_keyId,
    untagResource_tagKeys,

    -- ** Sign
    sign_messageType,
    sign_grantTokens,
    sign_keyId,
    sign_message,
    sign_signingAlgorithm,
    signResponse_signingAlgorithm,
    signResponse_signature,
    signResponse_keyId,
    signResponse_httpStatus,

    -- ** ScheduleKeyDeletion
    scheduleKeyDeletion_pendingWindowInDays,
    scheduleKeyDeletion_keyId,
    scheduleKeyDeletionResponse_keyId,
    scheduleKeyDeletionResponse_keyState,
    scheduleKeyDeletionResponse_deletionDate,
    scheduleKeyDeletionResponse_pendingWindowInDays,
    scheduleKeyDeletionResponse_httpStatus,

    -- ** GenerateDataKeyPair
    generateDataKeyPair_encryptionContext,
    generateDataKeyPair_grantTokens,
    generateDataKeyPair_keyId,
    generateDataKeyPair_keyPairSpec,
    generateDataKeyPairResponse_keyId,
    generateDataKeyPairResponse_publicKey,
    generateDataKeyPairResponse_privateKeyPlaintext,
    generateDataKeyPairResponse_keyPairSpec,
    generateDataKeyPairResponse_privateKeyCiphertextBlob,
    generateDataKeyPairResponse_httpStatus,

    -- ** ReplicateKey
    replicateKey_bypassPolicyLockoutSafetyCheck,
    replicateKey_policy,
    replicateKey_description,
    replicateKey_tags,
    replicateKey_keyId,
    replicateKey_replicaRegion,
    replicateKeyResponse_replicaKeyMetadata,
    replicateKeyResponse_replicaPolicy,
    replicateKeyResponse_replicaTags,
    replicateKeyResponse_httpStatus,

    -- ** PutKeyPolicy
    putKeyPolicy_bypassPolicyLockoutSafetyCheck,
    putKeyPolicy_keyId,
    putKeyPolicy_policyName,
    putKeyPolicy_policy,

    -- ** EnableKey
    enableKey_keyId,

    -- ** RevokeGrant
    revokeGrant_keyId,
    revokeGrant_grantId,

    -- ** GetKeyPolicy
    getKeyPolicy_keyId,
    getKeyPolicy_policyName,
    getKeyPolicyResponse_policy,
    getKeyPolicyResponse_httpStatus,

    -- ** ImportKeyMaterial
    importKeyMaterial_expirationModel,
    importKeyMaterial_validTo,
    importKeyMaterial_keyId,
    importKeyMaterial_importToken,
    importKeyMaterial_encryptedKeyMaterial,
    importKeyMaterialResponse_httpStatus,

    -- ** DeleteImportedKeyMaterial
    deleteImportedKeyMaterial_keyId,

    -- * Types

    -- ** AliasListEntry
    aliasListEntry_targetKeyId,
    aliasListEntry_aliasName,
    aliasListEntry_creationDate,
    aliasListEntry_lastUpdatedDate,
    aliasListEntry_aliasArn,

    -- ** CustomKeyStoresListEntry
    customKeyStoresListEntry_customKeyStoreName,
    customKeyStoresListEntry_trustAnchorCertificate,
    customKeyStoresListEntry_connectionErrorCode,
    customKeyStoresListEntry_creationDate,
    customKeyStoresListEntry_cloudHsmClusterId,
    customKeyStoresListEntry_customKeyStoreId,
    customKeyStoresListEntry_connectionState,

    -- ** GrantConstraints
    grantConstraints_encryptionContextEquals,
    grantConstraints_encryptionContextSubset,

    -- ** GrantListEntry
    grantListEntry_keyId,
    grantListEntry_retiringPrincipal,
    grantListEntry_issuingAccount,
    grantListEntry_grantId,
    grantListEntry_constraints,
    grantListEntry_granteePrincipal,
    grantListEntry_name,
    grantListEntry_creationDate,
    grantListEntry_operations,

    -- ** KeyListEntry
    keyListEntry_keyId,
    keyListEntry_keyArn,

    -- ** KeyMetadata
    keyMetadata_origin,
    keyMetadata_expirationModel,
    keyMetadata_keyManager,
    keyMetadata_keySpec,
    keyMetadata_customerMasterKeySpec,
    keyMetadata_enabled,
    keyMetadata_validTo,
    keyMetadata_arn,
    keyMetadata_keyState,
    keyMetadata_encryptionAlgorithms,
    keyMetadata_aWSAccountId,
    keyMetadata_signingAlgorithms,
    keyMetadata_pendingDeletionWindowInDays,
    keyMetadata_keyUsage,
    keyMetadata_creationDate,
    keyMetadata_deletionDate,
    keyMetadata_cloudHsmClusterId,
    keyMetadata_description,
    keyMetadata_customKeyStoreId,
    keyMetadata_multiRegion,
    keyMetadata_multiRegionConfiguration,
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

import Network.AWS.KMS.CancelKeyDeletion
import Network.AWS.KMS.ConnectCustomKeyStore
import Network.AWS.KMS.CreateAlias
import Network.AWS.KMS.CreateCustomKeyStore
import Network.AWS.KMS.CreateGrant
import Network.AWS.KMS.CreateKey
import Network.AWS.KMS.Decrypt
import Network.AWS.KMS.DeleteAlias
import Network.AWS.KMS.DeleteCustomKeyStore
import Network.AWS.KMS.DeleteImportedKeyMaterial
import Network.AWS.KMS.DescribeCustomKeyStores
import Network.AWS.KMS.DescribeKey
import Network.AWS.KMS.DisableKey
import Network.AWS.KMS.DisableKeyRotation
import Network.AWS.KMS.DisconnectCustomKeyStore
import Network.AWS.KMS.EnableKey
import Network.AWS.KMS.EnableKeyRotation
import Network.AWS.KMS.Encrypt
import Network.AWS.KMS.GenerateDataKey
import Network.AWS.KMS.GenerateDataKeyPair
import Network.AWS.KMS.GenerateDataKeyPairWithoutPlaintext
import Network.AWS.KMS.GenerateDataKeyWithoutPlaintext
import Network.AWS.KMS.GenerateRandom
import Network.AWS.KMS.GetKeyPolicy
import Network.AWS.KMS.GetKeyRotationStatus
import Network.AWS.KMS.GetParametersForImport
import Network.AWS.KMS.GetPublicKey
import Network.AWS.KMS.ImportKeyMaterial
import Network.AWS.KMS.ListAliases
import Network.AWS.KMS.ListGrants
import Network.AWS.KMS.ListKeyPolicies
import Network.AWS.KMS.ListKeys
import Network.AWS.KMS.ListResourceTags
import Network.AWS.KMS.ListRetirableGrants
import Network.AWS.KMS.PutKeyPolicy
import Network.AWS.KMS.ReEncrypt
import Network.AWS.KMS.ReplicateKey
import Network.AWS.KMS.RetireGrant
import Network.AWS.KMS.RevokeGrant
import Network.AWS.KMS.ScheduleKeyDeletion
import Network.AWS.KMS.Sign
import Network.AWS.KMS.TagResource
import Network.AWS.KMS.Types.AliasListEntry
import Network.AWS.KMS.Types.CustomKeyStoresListEntry
import Network.AWS.KMS.Types.GrantConstraints
import Network.AWS.KMS.Types.GrantListEntry
import Network.AWS.KMS.Types.KeyListEntry
import Network.AWS.KMS.Types.KeyMetadata
import Network.AWS.KMS.Types.ListGrantsResponse
import Network.AWS.KMS.Types.MultiRegionConfiguration
import Network.AWS.KMS.Types.MultiRegionKey
import Network.AWS.KMS.Types.Tag
import Network.AWS.KMS.UntagResource
import Network.AWS.KMS.UpdateAlias
import Network.AWS.KMS.UpdateCustomKeyStore
import Network.AWS.KMS.UpdateKeyDescription
import Network.AWS.KMS.UpdatePrimaryRegion
import Network.AWS.KMS.Verify
