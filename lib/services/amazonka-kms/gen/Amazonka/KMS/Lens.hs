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
